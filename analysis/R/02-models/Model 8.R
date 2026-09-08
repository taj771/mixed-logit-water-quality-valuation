########################################################################################
# Description: Model 8 (RPM) - Model 4.4 spec plus perceived-SQ terms
#######################################################################################
### Clear memory
rm(list = ls())



### Set working directory (only works in RStudio)
#apollo_setWorkDir()

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName       = "Model 8",
  modelDescr      = "Mixed-MNL",
  indivID         = "CaseId",  
  nCores          = min(4, max(1, parallel::detectCores() - 1)),  # was 4; capped for portability
  outputDirectory = "analysis/outputs/models",
  weights = "WEIGHT"
)

# ################################################################# #
#### LOAD DATA                                                   ####
# ################################################################# #

database <- read_csv("data/derived/test/processed_finaldata_batch_1_Apollo.csv")

database <- database %>%
  mutate(
    PERCIEVED_WORSE_WQ  = ifelse(is.na(LOCAL_WQ_OPINION), NA,
                                 ifelse(LOCAL_WQ_OPINION == 1, 1, 0)),
    PERCIEVED_SAME_WQ   = ifelse(is.na(LOCAL_WQ_OPINION), NA,
                                 ifelse(LOCAL_WQ_OPINION == 2, 1, 0)),
    PERCIEVED_BETTER_WQ = ifelse(is.na(LOCAL_WQ_OPINION), NA,
                                 ifelse(LOCAL_WQ_OPINION == 3, 1, 0))
  )

# Arrange data by RespondentID
database <- database %>%
  arrange(CaseId)

database <- database %>%
  filter(!is.na(VOTE))%>%
#  filter(CHOICE_AREA == "BASIN") |>
  filter(!is.na(WQ_SUBBASIN_LOCAL_CURRENT_SUBONLY_N))%>%
  filter(!is.na(WQ_SUBBASIN_NL_CURRENT_SUBONLY_N))%>%
  
  filter(!is.na(WQ_SUBBASIN_LOCAL_POLICY_SUBONLY_N))%>%
  filter(!is.na(WQ_SUBBASIN_NL_POLICY_SUBONLY_N))%>%
  
  filter(!is.na(WQ_BASIN_LOCAL_CURRENT_N))%>%
  filter(!is.na(WQ_BASIN_NL_CURRENT_N))%>%
  
  filter(!is.na(WQ_BASIN_LOCAL_POLICY_N))%>%
  filter(!is.na(WQ_BASIN_NL_POLICY_N))%>%
  
  filter(!is.na(PERCIEVED_WORSE_WQ))%>%
  filter(!is.na(PERCIEVED_BETTER_WQ))

# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

apollo_beta = c(
  mu_b_asc     = 1.567,  
  sigma_b_asc = 2.298,
  b_cost  = -0.005,   
  mu_b_wq_local_basin = -0.974,
  sigma_b_wq_local_basin = 0.703,
  mu_b_wq_nonlocal_basin = -0.642,
  sigma_b_wq_nonlocal_basin = 0.127,
  mu_b_wq_local_sub_basin = -0.633,
  sigma_b_wq_local_sub_basin = 0.013,
  mu_b_wq_nonlocal_sub_basin = -0.435,
  sigma_b_wq_nonlocal_sub_basin = 0.284,
  
#  b_basesq_obs = 0,
  
  b_basesq_exp = 0,
  b_basesq_worse = 0,
  b_basesq_better = 0,
  
  
  b_basin_1 = 0,
  b_basin_2 =0,
  b_basin_3 = 0,
b_basin_4 = 0,
b_subbasin_1 = 0,
b_subbasin_2 = 0,
b_subbasin_3 = 0,
b_subbasin_4 = 0,
b_subbasin_5 = 0,
b_subbasin_6 = 0,
b_subbasin_7 = 0,
b_subbasin_8 = 0,
b_subbasin_9 = 0,
b_subbasin_10 = 0,
b_subbasin_11 = 0,
b_subbasin_12 = 0,
b_subbasin_13 = 0,
b_subbasin_14= 0,
b_subbasin_15 = 0,
b_subbasin_16 = 0,
b_subbasin_17 = 0#,
#b_subbasin_18 = 0


  
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
                     "draws_wq_local_sub_basin","draws_wq_nonlocal_sub_basin",
                    # "draws_basesq_obs",
                     "draws_basesq_exp"),
  intraDrawsType = "sobol",
  intraNDraws    = 0,
  intraUnifDraws = c(),
  intraNormDraws = c()
)


### Create random parameters
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  randcoeff[["b_asc"]] = mu_b_asc + sigma_b_asc*draws_asc 
  
  randcoeff[["b_wq_local_basin"]] =  mu_b_wq_local_basin + sigma_b_wq_local_basin*draws_wq_local_basin
  randcoeff[["b_wq_nonlocal_basin"]] =  mu_b_wq_nonlocal_basin + sigma_b_wq_nonlocal_basin*draws_wq_nonlocal_basin
  
  randcoeff[["b_wq_local_sub_basin"]] =  mu_b_wq_local_sub_basin + sigma_b_wq_local_sub_basin*draws_wq_local_sub_basin
  randcoeff[["b_wq_nonlocal_sub_basin"]] =  mu_b_wq_nonlocal_sub_basin + sigma_b_wq_nonlocal_sub_basin*draws_wq_nonlocal_sub_basin
  
  #randcoeff[["b_basesq_obs"]] =  mu_b_basesq_obs + sigma_b_basesq_obs*draws_basesq_obs
  #randcoeff[["b_basesq_exp"]] =  mu_b_basesq_exp + sigma_b_basesq_exp*draws_basesq_exp
  
  
  
  
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
  V[["policy"]]  = b_asc + b_cost *COST + 
    b_wq_local_basin*WQ_BASIN_LOCAL_POLICY_N +
    b_wq_nonlocal_basin*WQ_BASIN_NL_POLICY_N +
    b_wq_local_sub_basin*WQ_SUBBASIN_LOCAL_POLICY_SUBONLY_N +
    b_wq_nonlocal_sub_basin*WQ_SUBBASIN_NL_POLICY_SUBONLY_N +
    
#   b_basesq_obs*WQ_CURRENT_CHOICE *(CHOICE_AREA == "BASIN")  +
    
    b_basesq_exp*WQ_CURRENT_CHOICE +# *(CHOICE_AREA == "BASIN") +
    
    b_basesq_worse*PERCIEVED_WORSE_WQ + 
    b_basesq_better*PERCIEVED_BETTER_WQ +
    
    
    b_basin_1 * (CHOICE_AREA == "BASIN" & CHOICE_BASIN == 1) +
    b_basin_2 * (CHOICE_AREA == "BASIN" & CHOICE_BASIN == 2) +
    b_basin_3 * (CHOICE_AREA == "BASIN" & CHOICE_BASIN == 3) +
    b_basin_4 * (CHOICE_AREA == "BASIN" & CHOICE_BASIN == 4) +
    b_subbasin_1 * (CHOICE_SUB_BASIN == 1) +
    b_subbasin_2 * (CHOICE_SUB_BASIN == 2) +
    b_subbasin_3 * (CHOICE_SUB_BASIN == 3) +
    b_subbasin_4 * (CHOICE_SUB_BASIN == 4) +
    b_subbasin_5 * (CHOICE_SUB_BASIN == 5) +
    b_subbasin_6 * (CHOICE_SUB_BASIN == 6) +
    b_subbasin_7 * (CHOICE_SUB_BASIN == 7) +
    b_subbasin_8 * (CHOICE_SUB_BASIN == 8) +
    b_subbasin_9 * (CHOICE_SUB_BASIN == 9) +
    b_subbasin_10 * (CHOICE_SUB_BASIN == 10) +
    b_subbasin_11 * (CHOICE_SUB_BASIN == 11) +
    b_subbasin_12 * (CHOICE_SUB_BASIN == 12) +
    b_subbasin_13 * (CHOICE_SUB_BASIN == 13) +
    b_subbasin_14 * (CHOICE_SUB_BASIN == 14) +
    b_subbasin_15 * (CHOICE_SUB_BASIN == 15) +
    b_subbasin_16 * (CHOICE_SUB_BASIN == 16) +
    b_subbasin_17 * (CHOICE_SUB_BASIN == 17) #+
 #   b_subbasin_18 * (CHOICE_SUB_BASIN == 18)
    
  
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