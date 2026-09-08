########################################################################################
# Description: Model 2 (RPM) - Log-Normal Distribution
#######################################################################################


### Clear memory
rm(list = ls())

### Load Apollo library
library(apollo)
library(dplyr)
library(readr)

### Set working directory (only works in RStudio)
#apollo_setWorkDir()

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName       = "Model 2_LogNormal",
  modelDescr      = "Mixed-MNL with Log-Normal WTP",
  indivID         = "CaseId",  
  nCores          = min(6, max(1, parallel::detectCores() - 1)),  # was 6; capped for portability
  outputDirectory = "analysis/outputs/models",
  weights = "WEIGHT"
)

# ################################################################# #
#### LOAD DATA                                                   ####
# ################################################################# #

database <- read_csv("data/derived/test/processed_finaldata_batch_1_Apollo.csv")

# Arrange data by RespondentID
database <- database %>%
  arrange(CaseId)

database <- database %>%
  filter(!is.na(VOTE))%>%
  filter(!is.na(WQ_SUBBASIN_LOCAL_CURRENT_SUBONLY_N))%>%
  filter(!is.na(WQ_SUBBASIN_NL_CURRENT_SUBONLY_N))%>%
  filter(!is.na(WQ_SUBBASIN_LOCAL_POLICY_SUBONLY_N))%>%
  filter(!is.na(WQ_SUBBASIN_NL_POLICY_SUBONLY_N))%>%
  filter(!is.na(WQ_BASIN_LOCAL_CURRENT_N))%>%
  filter(!is.na(WQ_BASIN_NL_CURRENT_N))%>%
  filter(!is.na(WQ_BASIN_LOCAL_POLICY_N))%>%
  filter(!is.na(WQ_BASIN_NL_POLICY_N))

# ################################################################# #
#### DEFINE MODEL PARAMETERS - LOG-NORMAL DISTRIBUTION         ####
# ################################################################# #

apollo_beta = c(
  # ASC - keep as normal distribution (can be positive or negative)
  mu_b_asc     = 0,  
  sigma_b_asc = 0.1,
  
  # Cost coefficient - keep as fixed for stability
  b_cost  = -0.01,   
  
  # Log-normal parameters for WTP
  # These are the LOG of the means (ensures positive WTP)
  ln_mu_b_wq_local_basin = log(0.1),        # exp(log(0.1)) = 0.1
  ln_sigma_b_wq_local_basin = log(0.05),    # Small initial variance
  
  ln_mu_b_wq_nonlocal_basin = log(0.05),    # exp(log(0.05)) = 0.05
  ln_sigma_b_wq_nonlocal_basin = log(0.05),
  
  ln_mu_b_wq_local_sub_basin = log(0.1),    # exp(log(0.1)) = 0.1
  ln_sigma_b_wq_local_sub_basin = log(0.05),
  
  ln_mu_b_wq_nonlocal_sub_basin = log(0.05), # exp(log(0.05)) = 0.05
  ln_sigma_b_wq_nonlocal_sub_basin = log(0.05)
)

### Vector with names of parameters to be kept fixed
apollo_fixed = c("b_cost")  # Fix cost coefficient for scale identification

# ################################################################# #
#### DEFINE RANDOM COMPONENTS - LOG-NORMAL                     ####
# ################################################################# #

### Set parameters for generating draws
apollo_draws = list(
  interDrawsType = "sobol",
  interNDraws    = 2000,
  interUnifDraws = c(),
  interNormDraws = c("draws_asc",
                     "draws_wq_local_basin","draws_wq_nonlocal_basin",
                     "draws_wq_local_sub_basin","draws_wq_nonlocal_sub_basin"),
  intraDrawsType = "sobol",
  intraNDraws    = 0,
  intraUnifDraws = c(),
  intraNormDraws = c()
)

### Create random parameters with LOG-NORMAL distributions
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  # ASC - normal distribution (can be positive or negative)
  randcoeff[["b_asc"]] = mu_b_asc + sigma_b_asc * draws_asc 
  
  # WTP parameters - LOG-NORMAL distribution (ensures positive values)
  # exp(ln_mu + ln_sigma * draws) ensures all values > 0
  randcoeff[["b_wq_local_basin"]] = exp(ln_mu_b_wq_local_basin + 
                                          exp(ln_sigma_b_wq_local_basin) * draws_wq_local_basin)
  
  randcoeff[["b_wq_nonlocal_basin"]] = exp(ln_mu_b_wq_nonlocal_basin + 
                                             exp(ln_sigma_b_wq_nonlocal_basin) * draws_wq_nonlocal_basin)
  
  randcoeff[["b_wq_local_sub_basin"]] = exp(ln_mu_b_wq_local_sub_basin + 
                                              exp(ln_sigma_b_wq_local_sub_basin) * draws_wq_local_sub_basin)
  
  randcoeff[["b_wq_nonlocal_sub_basin"]] = exp(ln_mu_b_wq_nonlocal_sub_basin + 
                                                 exp(ln_sigma_b_wq_nonlocal_sub_basin) * draws_wq_nonlocal_sub_basin)
  
  return(randcoeff)
}

# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #

apollo_inputs = apollo_validateInputs()

# Define model and likelihood function
apollo_probabilities = function(apollo_beta, apollo_inputs, functionality = "estimate") {
  
  # Attach inputs
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  # Create list of probabilities
  P = list()
  
  # Define utilities
  V = list()
  V[["policy"]]  = b_asc + b_cost * COST + 
    b_wq_local_basin * WQ_BASIN_LOCAL_POLICY_N +
    b_wq_nonlocal_basin * WQ_BASIN_NL_POLICY_N +
    b_wq_local_sub_basin * WQ_SUBBASIN_LOCAL_POLICY_SUBONLY_N +
    b_wq_nonlocal_sub_basin * WQ_SUBBASIN_NL_POLICY_SUBONLY_N
  
  V[["opt_out"]] = 
    b_wq_local_basin * WQ_BASIN_LOCAL_CURRENT_N +
    b_wq_nonlocal_basin * WQ_BASIN_NL_CURRENT_N +
    b_wq_local_sub_basin * WQ_SUBBASIN_LOCAL_CURRENT_SUBONLY_N +
    b_wq_nonlocal_sub_basin * WQ_SUBBASIN_NL_CURRENT_SUBONLY_N
  
  # Define MNL settings
  mnl_settings = list(
    alternatives  = c(policy = 1, opt_out = 0),
    avail         = 1,
    choiceVar     = VOTE,
    utilities     = V
  )
  
  ### Compute probabilities using MNL model
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Average across inter-individual draws
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  ### Apply weights here (note the functionality argument)
  P = apollo_weighting(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

# ################################################################# #
#### POST-ESTIMATION ANALYSIS                                    ####
# ################################################################# #

# Display model outputs
apollo_modelOutput(model)

# Save model outputs
apollo_saveOutput(model)

