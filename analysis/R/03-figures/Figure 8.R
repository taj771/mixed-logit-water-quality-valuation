##############################################################################################
# Description: Figure 8: Assessing the robustness of model estimates using validity questions.
#############################################################################################


### Clear memory
rm(list = ls())

### Load Apollo library
library(apollo)

### Set working directory (only works in RStudio)
#apollo_setWorkDir()

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName       = "Model 10",
  modelDescr      = "Mixed-MNL",
  indivID         = "CaseId",  
  nCores          = 4,
  outputDirectory = "output",
  weights = "WEIGHT"
)

# ################################################################# #
#### LOAD DATA                                                   ####
# ################################################################# #

df <- read_csv("data/derived/test/processed_finaldata_batch_1_Apollo.csv")



# Arrange data by RespondentID
df <- df %>%
  arrange(CaseId)

df <- df %>%
  filter(!is.na(VOTE))%>%
  
  filter(!is.na(WQ_SUBBASIN_LOCAL_CURRENT_SUBONLY_N))%>%
  filter(!is.na(WQ_SUBBASIN_NL_CURRENT_SUBONLY_N))%>%
  
  filter(!is.na(WQ_SUBBASIN_LOCAL_POLICY_SUBONLY_N))%>%
  filter(!is.na(WQ_SUBBASIN_NL_POLICY_SUBONLY_N))%>%
  
  filter(!is.na(WQ_BASIN_LOCAL_CURRENT_N))%>%
  filter(!is.na(WQ_BASIN_NL_CURRENT_N))%>%
  
  filter(!is.na(WQ_BASIN_LOCAL_POLICY_N))%>%
  filter(!is.na(WQ_BASIN_NL_POLICY_N))

##############################################################################
# criteria 1 - cost related
##############################################################################

#vote yes to all + I voted as if my household would face the stated cost (disagree) - Q12
#vote yes to all + The cost of the policy in additional taxes (little or no effect) - Q13
#vote no to all + I would like to see water quality improved, but I cannot afford to pay much for it (1) - Q15
#vote no to all  + I believe that my taxes are too high already and am against any initiative that will increase taxes (1) - Q15



df <- df %>%
  group_by(CaseId) %>%
  mutate(VOTE_YES_ALL = as.integer(all(VOTE == 1))) %>%
  ungroup()%>%
  
  mutate(Q12_COST_FACE_HOUSEHOLD = ifelse(Q12_VOTE_HOUSEHOLD_FACE_COST %in% c(1), 1, 0))%>% # I voted as if my household would face the stated cost # 1 - Disagree 2 - Neutral
  
  mutate(Q13_COST_HAS_EFFECT = ifelse(Q13_INFLUENCE_COST %in% c(1), 1, 0))%>% # The cost of the policy in additional taxes # (little or no effect)
  
  group_by(CaseId) %>%
  mutate(VOTE_NO_ALL = as.integer(all(VOTE == 0))) %>%
  ungroup()%>%
  
  mutate(Q15_6 = ifelse(Q15_GENERAL_THOUGHTS__CH6 %in% c (1), 1, 0))%>% ## I would like to see water quality improved, but I cannot afford to pay much for it (1) - Q15
  
  mutate(Q15_7 = ifelse(Q15_GENERAL_THOUGHTS__CH7 %in% c (1), 1, 0))%>% ## I believe that my taxes are too high already and am against any initiative that will increase taxes (1) - Q15
  
  
  mutate(FILTER_1 = ifelse(
    (VOTE_YES_ALL == 1 & Q12_COST_FACE_HOUSEHOLD == 1) |
      (VOTE_NO_ALL == 1 & Q15_7 == 1),
    1, 0
  ))%>%
  
  # Criteria 2 - - unsure votes
  #Uncertain + I am certain that it I voted the same way I would if I were voting in a public election (disagree)
  #Uncertain + I did not read the information on the proposals carefully. (1)
  #Uncertain + I did not have enough information to make comfortable decisions (1)
  
  mutate(VOTE_UNCERTAIN = ifelse(Q14_CERTAIN_VOTE %in% c(1,2), 1,0))%>%
  
  mutate(VOTE_AS_PUBLIC_ELECTION = ifelse(Q12_VOTE_CERTAIN_PUBLIC_ELEC %in% c(1), 1,0))%>%
  
  mutate(Q15_4 = ifelse(Q15_GENERAL_THOUGHTS__CH4 %in% c (1), 1, 0))%>% 
  
  mutate(Q15_5 = ifelse(Q15_GENERAL_THOUGHTS__CH5 %in% c (1), 1, 0))%>% 
  
  mutate(FILTER_2 = ifelse(
    (VOTE_UNCERTAIN == 1 & VOTE_AS_PUBLIC_ELECTION == 1) |
      (VOTE_UNCERTAIN == 1 & Q15_4 == 1) |
      (VOTE_UNCERTAIN == 1 & Q15_5 == 1),
    1, 0
  ))%>%
  
  
  #criteria 3 - consequality
  #I voted as if the policies would achieve the stated improvements in water quality (disagree)
  #I voted as if the information collected in this survey will be used to inform policy makers (disagree)
  
  
  mutate(FILTER_3 = ifelse(Q12_VOTE_INFORM_POLICY_MAKERS == 1 & 
                             Q12_VOTE_WITHOUT_CONSIDER_OTHER == 1 &
                             Q12_VOTE_POLICY_ACHIEVE_IMPROV == 1, 1, 0))%>%
  
  # criteria 4 - understand the survey instructions 
  # I voted on each proposal without any consideration of the other proposals (disagree)
  # The size of the region affected by the policy (no effect)
  # Whether the policy improved water quality near my home (no efefct)
  # Improvements in the water quality level (no effect)
  
  mutate(FILTER_4 = ifelse(
    Q12_VOTE_WITHOUT_CONSIDER_OTHER == 1 &
      Q13_INFLUENCE_REGIONSIZE == 1 &
      Q13_INFLUENCE_NEAR_HOME == 1 &
      Q13_INFLUENCE_WQ_LEVEL == 1,
    1,0
  ))%>%
  
  mutate(FILTER_5 = ifelse(
    FILTER_1 == 1 |
      FILTER_2 == 1 |
      FILTER_3 == 1 |
      FILTER_4 == 1, 1,0
  ))

######### Marginal WTP ######### Marginal WTP ######### Marginal WTP ######### Marginal WTP


##############################################################################################################

database <- df

# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

apollo_beta = c(
  mu_b_asc     = 0,  
  sigma_b_asc = 0.01,
  b_cost  = 0,   
  mu_b_wq_local_basin = 0,
  sigma_b_wq_local_basin = 0.1,
  mu_b_wq_nonlocal_basin = 0,
  sigma_b_wq_nonlocal_basin = 0.1,
  mu_b_wq_local_sub_basin = 0,
  sigma_b_wq_local_sub_basin = 0.1,
  mu_b_wq_nonlocal_sub_basin = 0,
  sigma_b_wq_nonlocal_sub_basin = 0.1
  
  
)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c()


# ################################################################# #
#### DEFINE RANDOM COMPONENTS                                    ####
# ################################################################# #

### Set parameters for generating draws
apollo_draws = list(
  interDrawsType = "sobol",
  interNDraws    = 1000,
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
  V[["policy"]]  = b_asc + b_cost *COST + 
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
#apollo_saveOutput(model)

################################################################################
# WTP # WTP # WTP # WTP # WTP # WTP # WTP # WTP # WTP # WTP # WTP # WTP # WTP 


# Extract coefficients and covariance matrix
coef_values <- model$estimate
vcov_matrix <- model$robvarcov


# WTP for one unit improvement of WQ at Local Basin

df1 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g =  "(mu_b_wq_local_basin/b_cost)"
)%>% 
  {`rownames<-`(., NULL)}%>%
  mutate(`WQ change scenario` = "Local Basin")%>%
  relocate(`WQ change scenario`, .before = 1)%>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))


# WTP for one unit improvement of WQ at Non-Local Basin

df2 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g =  "(mu_b_wq_nonlocal_basin/b_cost)"
)%>% 
  {`rownames<-`(., NULL)}%>%
  mutate(`WQ change scenario` = "Non-Local Basin")%>%
  relocate(`WQ change scenario`, .before = 1)%>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))



# WTP for one unit improvement of WQ at Local subbasin

df3 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g =  "(mu_b_wq_local_sub_basin/b_cost)"
)%>% 
  {`rownames<-`(., NULL)}%>%
  mutate(`WQ change scenario` = "Local Sub Basin")%>%
  relocate(`WQ change scenario`, .before = 1)%>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))



# WTP for one unit improvement of WQ at non-local subbasin

df4 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g =  "(mu_b_wq_nonlocal_sub_basin/b_cost)"
)%>% 
  {`rownames<-`(., NULL)}%>%
  mutate(`WQ change scenario` = "Non-Local  Sub Basin")%>%
  relocate(`WQ change scenario`, .before = 1)%>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))

num_respond <- model$numResids


df_all <- rbind(df1,df2,df3,df4)%>%
  mutate(Validity_Q = "full sample")%>%
  mutate(N = num_respond)

  


write.csv(df_all, "analysis/outputs/tables/WTP_marginal_full_sample.csv")



###############################################################################################################

database <- df%>%
  filter(FILTER_1 == 0)

# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

apollo_beta = c(
  mu_b_asc     = 0,  
  sigma_b_asc = 0.01,
  b_cost  = 0,   
  mu_b_wq_local_basin = 0,
  sigma_b_wq_local_basin = 0.1,
  mu_b_wq_nonlocal_basin = 0,
  sigma_b_wq_nonlocal_basin = 0.1,
  mu_b_wq_local_sub_basin = 0,
  sigma_b_wq_local_sub_basin = 0.1,
  mu_b_wq_nonlocal_sub_basin = 0,
  sigma_b_wq_nonlocal_sub_basin = 0.1
  
  
)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c()


# ################################################################# #
#### DEFINE RANDOM COMPONENTS                                    ####
# ################################################################# #

### Set parameters for generating draws
apollo_draws = list(
  interDrawsType = "sobol",
  interNDraws    = 1000,
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
  V[["policy"]]  = b_asc + b_cost *COST + 
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
#apollo_saveOutput(model)

################################################################################
# WTP # WTP # WTP # WTP # WTP # WTP # WTP # WTP # WTP # WTP # WTP # WTP # WTP 


# Extract coefficients and covariance matrix
coef_values <- model$estimate
vcov_matrix <- model$robvarcov


# WTP for one unit improvement of WQ at Local Basin

df1 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g =  "(mu_b_wq_local_basin/b_cost)"
)%>% 
  {`rownames<-`(., NULL)}%>%
  mutate(`WQ change scenario` = "Local Basin")%>%
  relocate(`WQ change scenario`, .before = 1)%>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))


# WTP for one unit improvement of WQ at Non-Local Basin

df2 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g =  "(mu_b_wq_nonlocal_basin/b_cost)"
)%>% 
  {`rownames<-`(., NULL)}%>%
  mutate(`WQ change scenario` = "Non-Local Basin")%>%
  relocate(`WQ change scenario`, .before = 1)%>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))



# WTP for one unit improvement of WQ at Local subbasin

df3 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g =  "(mu_b_wq_local_sub_basin/b_cost)"
)%>% 
  {`rownames<-`(., NULL)}%>%
  mutate(`WQ change scenario` = "Local Sub Basin")%>%
  relocate(`WQ change scenario`, .before = 1)%>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))



# WTP for one unit improvement of WQ at non-local subbasin

df4 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g =  "(mu_b_wq_nonlocal_sub_basin/b_cost)"
)%>% 
  {`rownames<-`(., NULL)}%>%
  mutate(`WQ change scenario` = "Non-Local  Sub Basin")%>%
  relocate(`WQ change scenario`, .before = 1)%>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))


num_respond <- model$numResids



df_all <- rbind(df1,df2,df3,df4)%>%
  mutate(Validity_Q = "Filter 1")%>%
  mutate(N = num_respond)



write.csv(df_all, "analysis/outputs/tables/WTP_marginal_filter1.csv")


###############################################################################################################

database <- df%>%
  filter(FILTER_2 == 0)


# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

apollo_beta = c(
  mu_b_asc     = 0,  
  sigma_b_asc = 0.01,
  b_cost  = 0,   
  mu_b_wq_local_basin = 0,
  sigma_b_wq_local_basin = 0.1,
  mu_b_wq_nonlocal_basin = 0,
  sigma_b_wq_nonlocal_basin = 0.1,
  mu_b_wq_local_sub_basin = 0,
  sigma_b_wq_local_sub_basin = 0.1,
  mu_b_wq_nonlocal_sub_basin = 0,
  sigma_b_wq_nonlocal_sub_basin = 0.1
  
  
)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c()


# ################################################################# #
#### DEFINE RANDOM COMPONENTS                                    ####
# ################################################################# #

### Set parameters for generating draws
apollo_draws = list(
  interDrawsType = "sobol",
  interNDraws    = 1000,
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
  V[["policy"]]  = b_asc + b_cost *COST + 
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
#apollo_saveOutput(model)

################################################################################
# WTP # WTP # WTP # WTP # WTP # WTP # WTP # WTP # WTP # WTP # WTP # WTP # WTP 


# Extract coefficients and covariance matrix
coef_values <- model$estimate
vcov_matrix <- model$robvarcov


# WTP for one unit improvement of WQ at Local Basin

df1 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g =  "(mu_b_wq_local_basin/b_cost)"
)%>% 
  {`rownames<-`(., NULL)}%>%
  mutate(`WQ change scenario` = "Local Basin")%>%
  relocate(`WQ change scenario`, .before = 1)%>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))


# WTP for one unit improvement of WQ at Non-Local Basin

df2 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g =  "(mu_b_wq_nonlocal_basin/b_cost)"
)%>% 
  {`rownames<-`(., NULL)}%>%
  mutate(`WQ change scenario` = "Non-Local Basin")%>%
  relocate(`WQ change scenario`, .before = 1)%>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))



# WTP for one unit improvement of WQ at Local subbasin

df3 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g =  "(mu_b_wq_local_sub_basin/b_cost)"
)%>% 
  {`rownames<-`(., NULL)}%>%
  mutate(`WQ change scenario` = "Local Sub Basin")%>%
  relocate(`WQ change scenario`, .before = 1)%>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))



# WTP for one unit improvement of WQ at non-local subbasin

df4 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g =  "(mu_b_wq_nonlocal_sub_basin/b_cost)"
)%>% 
  {`rownames<-`(., NULL)}%>%
  mutate(`WQ change scenario` = "Non-Local  Sub Basin")%>%
  relocate(`WQ change scenario`, .before = 1)%>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))

num_respond <- model$numResids



df_all <- rbind(df1,df2,df3,df4)%>%
  mutate(Validity_Q = "Filter 2")%>%
  mutate(N = num_respond)


write.csv(df_all, "analysis/outputs/tables/WTP_marginal_filter2.csv")


###############################################################################################################

database <- df%>%
  filter(FILTER_3 == 0)


# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

apollo_beta = c(
  mu_b_asc     = 0,  
  sigma_b_asc = 0.01,
  b_cost  = 0,   
  mu_b_wq_local_basin = 0,
  sigma_b_wq_local_basin = 0.1,
  mu_b_wq_nonlocal_basin = 0,
  sigma_b_wq_nonlocal_basin = 0.1,
  mu_b_wq_local_sub_basin = 0,
  sigma_b_wq_local_sub_basin = 0.1,
  mu_b_wq_nonlocal_sub_basin = 0,
  sigma_b_wq_nonlocal_sub_basin = 0.1
  
  
)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c()


# ################################################################# #
#### DEFINE RANDOM COMPONENTS                                    ####
# ################################################################# #

### Set parameters for generating draws
apollo_draws = list(
  interDrawsType = "sobol",
  interNDraws    = 1000,
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
  V[["policy"]]  = b_asc + b_cost *COST + 
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
#apollo_saveOutput(model)

################################################################################
# WTP # WTP # WTP # WTP # WTP # WTP # WTP # WTP # WTP # WTP # WTP # WTP # WTP 


# Extract coefficients and covariance matrix
coef_values <- model$estimate
vcov_matrix <- model$robvarcov


# WTP for one unit improvement of WQ at Local Basin

df1 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g =  "(mu_b_wq_local_basin/b_cost)"
)%>% 
  {`rownames<-`(., NULL)}%>%
  mutate(`WQ change scenario` = "Local Basin")%>%
  relocate(`WQ change scenario`, .before = 1)%>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))


# WTP for one unit improvement of WQ at Non-Local Basin

df2 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g =  "(mu_b_wq_nonlocal_basin/b_cost)"
)%>% 
  {`rownames<-`(., NULL)}%>%
  mutate(`WQ change scenario` = "Non-Local Basin")%>%
  relocate(`WQ change scenario`, .before = 1)%>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))



# WTP for one unit improvement of WQ at Local subbasin

df3 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g =  "(mu_b_wq_local_sub_basin/b_cost)"
)%>% 
  {`rownames<-`(., NULL)}%>%
  mutate(`WQ change scenario` = "Local Sub Basin")%>%
  relocate(`WQ change scenario`, .before = 1)%>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))



# WTP for one unit improvement of WQ at non-local subbasin

df4 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g =  "(mu_b_wq_nonlocal_sub_basin/b_cost)"
)%>% 
  {`rownames<-`(., NULL)}%>%
  mutate(`WQ change scenario` = "Non-Local  Sub Basin")%>%
  relocate(`WQ change scenario`, .before = 1)%>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))

num_respond <- model$numResids


df_all <- rbind(df1,df2,df3,df4)%>%
  mutate(Validity_Q = "Filter 3")%>%
  mutate(N = num_respond)


write.csv(df_all, "analysis/outputs/tables/WTP_marginal_filter3.csv")


# remvoe estimation from filer 4
###############################################################################

###############################################################################################################

database <- df%>%
  filter(FILTER_5 == 0)


# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

apollo_beta = c(
  mu_b_asc     = 0,  
  sigma_b_asc = 0.01,
  b_cost  = 0,   
  mu_b_wq_local_basin = 0,
  sigma_b_wq_local_basin = 0.1,
  mu_b_wq_nonlocal_basin = 0,
  sigma_b_wq_nonlocal_basin = 0.1,
  mu_b_wq_local_sub_basin = 0,
  sigma_b_wq_local_sub_basin = 0.1,
  mu_b_wq_nonlocal_sub_basin = 0,
  sigma_b_wq_nonlocal_sub_basin = 0.1
  
  
)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c()


# ################################################################# #
#### DEFINE RANDOM COMPONENTS                                    ####
# ################################################################# #

### Set parameters for generating draws
apollo_draws = list(
  interDrawsType = "sobol",
  interNDraws    = 1000,
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
  V[["policy"]]  = b_asc + b_cost *COST + 
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
#apollo_saveOutput(model)

################################################################################
# WTP # WTP # WTP # WTP # WTP # WTP # WTP # WTP # WTP # WTP # WTP # WTP # WTP 


# Extract coefficients and covariance matrix
coef_values <- model$estimate
vcov_matrix <- model$robvarcov


# WTP for one unit improvement of WQ at Local Basin

df1 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g =  "(mu_b_wq_local_basin/b_cost)"
)%>% 
  {`rownames<-`(., NULL)}%>%
  mutate(`WQ change scenario` = "Local Basin")%>%
  relocate(`WQ change scenario`, .before = 1)%>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))


# WTP for one unit improvement of WQ at Non-Local Basin

df2 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g =  "(mu_b_wq_nonlocal_basin/b_cost)"
)%>% 
  {`rownames<-`(., NULL)}%>%
  mutate(`WQ change scenario` = "Non-Local Basin")%>%
  relocate(`WQ change scenario`, .before = 1)%>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))



# WTP for one unit improvement of WQ at Local subbasin

df3 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g =  "(mu_b_wq_local_sub_basin/b_cost)"
)%>% 
  {`rownames<-`(., NULL)}%>%
  mutate(`WQ change scenario` = "Local Sub Basin")%>%
  relocate(`WQ change scenario`, .before = 1)%>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))



# WTP for one unit improvement of WQ at non-local subbasin

df4 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g =  "(mu_b_wq_nonlocal_sub_basin/b_cost)"
)%>% 
  {`rownames<-`(., NULL)}%>%
  mutate(`WQ change scenario` = "Non-Local  Sub Basin")%>%
  relocate(`WQ change scenario`, .before = 1)%>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))

num_respond <- model$numResids



df_all <- rbind(df1,df2,df3,df4)%>%
  mutate(Validity_Q = "Filter 5")%>%
  mutate(N = num_respond)



write.csv(df_all, "analysis/outputs/tables/WTP_marginal_filter5.csv")



df1 <- read.csv("analysis/outputs/tables/WTP_marginal_full_sample.csv")


df2 <- read.csv("analysis/outputs/tables/WTP_marginal_filter1.csv")

df3 <- read.csv("analysis/outputs/tables/WTP_marginal_filter2.csv")

df4 <- read.csv("analysis/outputs/tables/WTP_marginal_filter3.csv")

#df5 <- read.csv("analysis/outputs/tables/WTP_marginal_filter4.csv")

df6 <- read.csv("analysis/outputs/tables/WTP_marginal_filter5.csv")



#df4 <- read.csv("analysis/outputs/tables/WTP_oneUnit_removeing_Q15.csv")

df <- rbind(df1,df2,df3,df4,df6)

df$WQ.change.scenario <- factor(df$WQ.change.scenario, levels = c(
  "Local Basin", "Non-Local Basin", "Local Sub Basin", "Non-Local  Sub Basin"
))

df$Validity_Q <- factor(df$Validity_Q, levels = c("full sample", "Filter 1", "Filter 2", "Filter 3","Filter 4","Filter 5"))

p <- ggplot(df, aes(x = WQ.change.scenario, y = Estimate, color = Validity_Q)) +
  # Shaded background by param index (works with coord_flip)
  scale_fill_identity() +
  
  # Points and error bars
  geom_point(position = position_dodge(width = 0.6), size = 3) +
  geom_errorbar(
    aes(ymin = Estimate - SE, ymax = Estimate + SE),
    position = position_dodge(width = 0.6), width = 0.2
  ) +
  # Axis formatting
  #scale_x_discrete(expand = expansion(mult = 0, add = 0.1)) +
  #coord_flip() +
  theme_minimal() +
  labs(x = "", y = "Marginal WTP", title = "", color = "version") +
  theme(
    legend.position = c(0.7, 0.9), # Custom x, y coordinates (0 to 1)
    legend.text = element_text(size = 14),
    axis.text.x = element_text(hjust = 1, size = 14, angle = 45),
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.y = element_line(size = 0.2, color = "black"),
    axis.line.x = element_line(size = 0.2, color = "black")
  )+
  scale_color_manual(
    values = c(
      "full sample" = "#1b9e77",
      "Filter 1" = "#d95f02",
      "Filter 2"  = "#7570b3",
      "Filter 3" = "darkred",
      "Filter 4" = "darkblue",
      "Filter 5" = "yellow3"
    ),
    labels = c(
      "full sample" = "Full Sample (N:3,825)",
      "Filter 1" = "Protest Responses Excluded (N:3,581)",
      "Filter 2" = "Uncertain Responses Excluded (N:3,592)",
      "Filter 3" = "Inconsequential & Non-independence Responses Excluded: (N:3,779)",
      #"Filter 4" = "Non-Independent and Unresponsive Voting Excluded (N:3,775)",
      "Filter 5" = "All Exclusion Criteria Applied (N: 3,323)"
    ),
    name = "",
    guide = guide_legend(nrow = 6)
  ) +
  scale_y_continuous(
    limits = c(50, 300),
    breaks = seq(50, 300, by = 50)
  )

# Save to PNG
ggsave("analysis/outputs/figures/sensitivity_WTP_marginal_checks.png", plot = p, width = 12, height = 10, units = "in", dpi = 300)
