# NOTE: superseded alternative specification (adds b_asc_rectrip_choice * REC_IN_CHOICE_BASIN).
# Previously wrote modelName "Model 7", silently overwriting the output of "Model 7.R".
# Renamed to "Model 7_rectrip_alt" to remove that collision. Not part of the manuscript.
########################################################################################
# Description: RPM Model 7
#######################################################################################


### Clear memory
rm(list = ls())

### Load Apollo library
library(apollo)
library(dplyr)     # %>%, arrange, filter
library(readr)     # read_csv

### Set working directory (only works in RStudio)
#apollo_setWorkDir()

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName       = "Model 7_rectrip_alt",
  modelDescr      = "Mixed-MNL",
  indivID         = "CaseId",  
  nCores          = min(8, max(1, parallel::detectCores() - 1)),  # was 8; capped for portability
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
  
  #filter(!is.na(REC_TRIP))

# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

apollo_beta = c(
  mu_b_asc     = 0,  
  sigma_b_asc = 0.01,
  
  #b_asc_rectrip = 0,
  #sigma_b_asc_rectrip = 0,
  
  b_asc_rectrip_choice = 0,
  #sigma_b_asc_rectrip_choice = 0,
  
  b_cost  = 0,  
  
  mu_b_wq_local_basin = 0,
  sigma_b_wq_local_basin = 0.01,
  
  mu_b_wq_nonlocal_basin = 0,
  sigma_b_wq_nonlocal_basin = 0.01,
  
  mu_b_wq_local_sub_basin = 0,
  sigma_b_wq_local_sub_basin = 0.01,
  
  mu_b_wq_nonlocal_sub_basin = 0,
  sigma_b_wq_nonlocal_sub_basin = 0.01
  
)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c()


# ################################################################# #
#### DEFINE RANDOM COMPONENTS                                    ####
# ################################################################# #

### Set parameters for generating draws
apollo_draws = list(
  interDrawsType = "sobol",
  interNDraws    = 100,
  interUnifDraws = c(),
  interNormDraws = c("draws_asc",
                     "draws_wq_local_basin","draws_wq_nonlocal_basin",
                     "draws_wq_local_sub_basin","draws_wq_nonlocal_sub_basin"),
  intraDrawsType = "sobol",
  intraNDraws    = 0,
  intraUnifDraws = c(),
  intraNormDraws = c()
)


### Create random parameters
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  randcoeff[["b_asc"]] = mu_b_asc + sigma_b_asc*draws_asc 
  
  #randcoeff[["b_asc_rectrip"]] = mu_b_asc_rectrip + sigma_b_asc_rectrip*draws_asc_rectrip 
  
  #randcoeff[["b_asc_rectrip_choice"]] = mu_b_asc_rectrip_choice + sigma_b_asc_rectrip_choice*draws_asc_rectrip_choice 
  
  randcoeff[["b_wq_local_basin"]] =  mu_b_wq_local_basin + sigma_b_wq_local_basin*draws_wq_local_basin
  randcoeff[["b_wq_nonlocal_basin"]] =  mu_b_wq_nonlocal_basin + sigma_b_wq_nonlocal_basin*draws_wq_nonlocal_basin
  
  randcoeff[["b_wq_local_sub_basin"]] =  mu_b_wq_local_sub_basin + sigma_b_wq_local_sub_basin*draws_wq_local_sub_basin
  randcoeff[["b_wq_nonlocal_sub_basin"]] =  mu_b_wq_nonlocal_sub_basin + sigma_b_wq_nonlocal_sub_basin*draws_wq_nonlocal_sub_basin
  
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
  V[["policy"]]  = b_asc + 
    #b_asc_rectrip*REC_TRIP +
    b_asc_rectrip_choice*REC_IN_CHOICE_BASIN +
    b_cost *COST + 
    b_wq_local_basin*WQ_BASIN_LOCAL_POLICY_N +
    b_wq_nonlocal_basin*WQ_BASIN_NL_POLICY_N +
    b_wq_local_sub_basin*WQ_SUBBASIN_LOCAL_POLICY_SUBONLY_N +
    b_wq_nonlocal_sub_basin*WQ_SUBBASIN_NL_POLICY_SUBONLY_N
  
  
  V[["opt_out"]] = 
    b_wq_local_basin*WQ_BASIN_LOCAL_CURRENT_N +
    b_wq_nonlocal_basin*WQ_BASIN_NL_CURRENT_N +
    b_wq_local_sub_basin*WQ_SUBBASIN_LOCAL_CURRENT_SUBONLY_N+
    b_wq_nonlocal_sub_basin*WQ_SUBBASIN_NL_CURRENT_SUBONLY_N
  
  
  # Define MNL settings
  mnl_settings = list(
    alternatives  = c(policy = 1, opt_out = 0),  # Match to VOTE column coding
    avail         = 1,  # Both alternatives are always available
    choiceVar     = VOTE,  # Choice variable in the dataset
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

model = apollo_estimate(apollo_beta, apollo_fixed,apollo_probabilities, apollo_inputs)



# Display model outputs
apollo_modelOutput(model)

# Save model outputs
apollo_saveOutput(model)