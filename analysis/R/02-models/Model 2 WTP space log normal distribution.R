########################################################################################
# Description: Model 2 (RPM) - WTP Space with Log-Normal Distribution
########################################################################################

### Clear memory
rm(list = ls())

### Load Apollo library
library(apollo)
library(dplyr)
library(readr)
library(stringr)   # str_starts / str_detect / str_remove in the table block below

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName       = "Model 2_WTPspace_lognormal",
  modelDescr      = "Mixed-MNL in WTP Space with Log-Normal",
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

# ################################################################# #
#### DEFINE MODEL PARAMETERS - WTP SPACE WITH LOG-NORMAL       ####
# ################################################################# #

apollo_beta = c(
  # Price coefficient (marginal utility of cost) - fixed for scale
  b_cost  = 0,   # Fixed negative value
  
  # ASC - normal distribution
  asc_mu = 0,
  asc_sigma = 0.1,
  
  # LOG-NORMAL WTP PARAMETERS
  ln_wtp_local_basin_mu = log(0.1),
  ln_wtp_local_basin_sigma = log(0.1),
  
  ln_wtp_nonlocal_basin_mu = log(0.05),
  ln_wtp_nonlocal_basin_sigma = log(0.1),
  
  ln_wtp_local_sub_basin_mu = log(0.1),
  ln_wtp_local_sub_basin_sigma = log(0.1),
  
  ln_wtp_nonlocal_sub_basin_mu = log(0.05),
  ln_wtp_nonlocal_sub_basin_sigma = log(0.1)
)

### Fixed parameters - fix cost coefficient and log-sigmas initially
apollo_fixed = c()

# ################################################################# #
#### DEFINE RANDOM COMPONENTS - LOG-NORMAL DISTRIBUTION        ####
# ################################################################# #

### Set parameters for generating draws
apollo_draws = list(
  interDrawsType = "sobol",
  interNDraws    = 1000,
  interUnifDraws = c(),
  interNormDraws = c("draws_asc",
                     "draws_wtp_local_basin", "draws_wtp_nonlocal_basin",
                     "draws_wtp_local_sub_basin", "draws_wtp_nonlocal_sub_basin"),
  intraDrawsType = "sobol",
  intraNDraws    = 0,
  intraUnifDraws = c(),
  intraNormDraws = c()
)

### Create random parameters - LOG-NORMAL DISTRIBUTION
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  # ASC - normal distribution
  randcoeff[["b_asc"]] = asc_mu + asc_sigma * draws_asc 
  
  # WTP parameters - LOG-NORMAL distribution (automatically positive)
  randcoeff[["wtp_local_basin"]] = exp(ln_wtp_local_basin_mu + 
                                         exp(ln_wtp_local_basin_sigma) * draws_wtp_local_basin)
  
  randcoeff[["wtp_nonlocal_basin"]] = exp(ln_wtp_nonlocal_basin_mu + 
                                            exp(ln_wtp_nonlocal_basin_sigma) * draws_wtp_nonlocal_basin)
  
  randcoeff[["wtp_local_sub_basin"]] = exp(ln_wtp_local_sub_basin_mu + 
                                             exp(ln_wtp_local_sub_basin_sigma) * draws_wtp_local_sub_basin)
  
  randcoeff[["wtp_nonlocal_sub_basin"]] = exp(ln_wtp_nonlocal_sub_basin_mu + 
                                                exp(ln_wtp_nonlocal_sub_basin_sigma) * draws_wtp_nonlocal_sub_basin)
  
  return(randcoeff)
}

# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #

apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### DEFINE PROBABILITIES - WTP SPACE FORMULATION                ####
# ################################################################# #

apollo_probabilities = function(apollo_beta, apollo_inputs, functionality = "estimate") {
  
  # Attach inputs
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  # Create list of probabilities
  P = list()
  
  # Define utilities - WTP SPACE
  V = list()
  V[["policy"]] = b_asc + 
    b_cost * (COST + 
                wtp_local_basin * WQ_BASIN_LOCAL_POLICY_N +
                wtp_nonlocal_basin * WQ_BASIN_NL_POLICY_N +
                wtp_local_sub_basin * WQ_SUBBASIN_LOCAL_POLICY_SUBONLY_N +
                wtp_nonlocal_sub_basin * WQ_SUBBASIN_NL_POLICY_SUBONLY_N)
  
  # Opt-out alternative
  V[["opt_out"]] = 
    b_cost * (wtp_local_basin * WQ_BASIN_LOCAL_CURRENT_N +
                wtp_nonlocal_basin * WQ_BASIN_NL_CURRENT_N +
                wtp_local_sub_basin * WQ_SUBBASIN_LOCAL_CURRENT_SUBONLY_N +
                wtp_nonlocal_sub_basin * WQ_SUBBASIN_NL_CURRENT_SUBONLY_N)
  
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


model <- apollo_loadModel("analysis/outputs/models/Model 2_WTPspace_lognormal")  # Replace "mymodel" with your actual modelName





# ################################################################# #
#### POST-ESTIMATION ANALYSIS - WTP TRANSFORMATION              ####
# ################################################################# #

# Display model outputs
apollo_modelOutput(model)

# Transform log-normal parameters to actual WTP values
if(!is.null(model$estimate)) {
  cat("\n\nACTUAL WTP VALUES (transformed from log-normal):\n")
  cat("==================================================\n")
  
  # Function to calculate mean of log-normal distribution
  # For log-normal: mean = exp(μ + σ²/2)
  calculate_actual_wtp <- function(ln_mu, ln_sigma) {
    sigma <- exp(ln_sigma)  # Transform from log-scale back to actual sigma
    actual_mean <- exp(ln_mu + (sigma^2)/2)
    return(actual_mean)
  }
  
  # Extract estimates
  estimates <- model$estimate
  
  # Transform each WTP parameter
  if("ln_wtp_local_basin_mu" %in% names(estimates)) {
    wtp_local <- calculate_actual_wtp(estimates["ln_wtp_local_basin_mu"], 
                                      estimates["ln_wtp_local_basin_sigma"])
    cat("Mean WTP for Local Basin: ", round(wtp_local, 4), "\n")
  }
  
  if("ln_wtp_nonlocal_basin_mu" %in% names(estimates)) {
    wtp_nonlocal <- calculate_actual_wtp(estimates["ln_wtp_nonlocal_basin_mu"], 
                                         estimates["ln_wtp_nonlocal_basin_sigma"])
    cat("Mean WTP for Non-local Basin: ", round(wtp_nonlocal, 4), "\n")
  }
  
  if("ln_wtp_local_sub_basin_mu" %in% names(estimates)) {
    wtp_local_sub <- calculate_actual_wtp(estimates["ln_wtp_local_sub_basin_mu"], 
                                          estimates["ln_wtp_local_sub_basin_sigma"])
    cat("Mean WTP for Local Sub-basin: ", round(wtp_local_sub, 4), "\n")
  }
  
  if("ln_wtp_nonlocal_sub_basin_mu" %in% names(estimates)) {
    wtp_nonlocal_sub <- calculate_actual_wtp(estimates["ln_wtp_nonlocal_sub_basin_mu"], 
                                             estimates["ln_wtp_nonlocal_sub_basin_sigma"])
    cat("Mean WTP for Non-local Sub-basin: ", round(wtp_nonlocal_sub, 4), "\n")
  }
  
  # Display cost coefficient
  if("b_cost" %in% names(estimates)) {
    cat("\nMarginal utility of cost: ", round(estimates["b_cost"], 4), "\n")
  }
}


estimates_df <- read.csv("analysis/outputs/models/Model 2_WTPspace_lognormal_estimates.csv")%>%
  select(X,Estimate,Rob.std.err.,Rob.t.ratio.0.)%>%
  mutate(Significance = case_when(
    #abs(Rob.t.ratio.0.) >= 3.291 ~ "***",  # p < 0.001
    abs(Rob.t.ratio.0.) >= 2.576 ~ "***",   # p < 0.01
    abs(Rob.t.ratio.0.) >= 1.960 ~ "**",    # p < 0.05
    abs(Rob.t.ratio.0.) >= 1.645 ~ "*",    # p < 0.1
    TRUE ~ ""
  ))%>%
  mutate(Estimate_with_Stars = paste0(round(Estimate, 3), Significance))%>%
  rename(Parameter = X,
         SE = Rob.std.err.)%>%
  select(Parameter, Estimate_with_Stars, SE)

df3 <- estimates_df %>%
  filter(str_starts(Parameter, "b_cost"))%>%
  mutate(SE=round(SE, digits = 4))%>%
  rename(Mean = Estimate_with_Stars) %>%
  mutate(SE = paste("(", SE, ")", sep = ""))

df3_mean <- df3%>%
  select(Parameter,Mean)

df3_SE <- df3%>%
  select(Parameter,SE)%>%
  #mutate(SE=round(SE, digits = 4))%>%
  #mutate(SE = paste("(", SE, ")", sep = ""))%>%
  rename(Mean=SE)%>%
  mutate(Parameter = paste0("se_", Parameter))

df3 <- rbind(df3_mean,df3_SE)%>%
  arrange(Parameter)

df1 <- estimates_df %>%
  filter(str_detect(Parameter, "mu")) %>%
  rename(Mean = Estimate_with_Stars) %>%
  mutate(Parameter = str_remove(Parameter, "_mu"))

df1_mean <- df1%>%
  select(Parameter,Mean)

df1_SE <- df1%>%
  select(Parameter,SE)%>%
  mutate(SE=round(SE, digits = 3))%>%
  mutate(SE = paste("(", SE, ")", sep = ""))%>%
  rename(Mean=SE)%>%
  mutate(Parameter = paste0("se_", Parameter))


df1 <- rbind(df1_mean,df1_SE)%>%
  arrange(Parameter)


df2 <- estimates_df %>%
  filter(str_detect(Parameter, "sigma")) %>%
  rename(Standard_Deviation = Estimate_with_Stars) %>%
  mutate(Parameter = str_remove(Parameter, "_sigma"))

df2_mean <- df2%>%
  select(Parameter,Standard_Deviation)

df2_SE <- df2%>%
  select(Parameter,SE)%>%
  mutate(SE=round(SE, digits = 3))%>%
  mutate(SE = paste("(", SE, ")", sep = ""))%>%
  rename(Standard_Deviation=SE)%>%
  mutate(Parameter = paste0("se_", Parameter))



df2 <- rbind(df2_mean,df2_SE)%>%
  arrange(Parameter)

df1 <- rbind(df1,df3)


df_model2 <- df1%>%
  left_join(df2)%>%
  mutate(Parameter = factor(Parameter, levels = c(
    "asc", "se_asc",
    "b_cost", "se_b_cost",
    "ln_wtp_local_basin", "se_ln_wtp_local_basin",
    "ln_wtp_nonlocal_basin", "se_ln_wtp_nonlocal_basin",
    "ln_wtp_local_sub_basin", "se_ln_wtp_local_sub_basin",
    "ln_wtp_nonlocal_sub_basin", "se_ln_wtp_nonlocal_sub_basin"
  ))) %>%
  arrange(Parameter)



model <- apollo_loadModel("analysis/outputs/models/Model 2_WTPspace_lognormal")  # Replace "mymodel" with your actual modelName

N_Obs <- model$nIndivs
N_vote <- model$nObs
LL_final <- model$LLout
Mc_R2 <- model$adjRho2_C
BIC <- model$BIC


# Create new_row data frame with multiple rows
new_row <- data.frame(
  Parameter = c("Individuals", "Voting Scenario", "Log-Likelihood at solution", "McFadden’s R2", "BIC"),
  Mean = c(
    format(N_Obs, scientific = FALSE, digits = 4, trim = TRUE),
    format(N_vote, scientific = FALSE, digits = 4, trim = TRUE),
    format(LL_final, scientific = FALSE, digits = 4, trim = TRUE),
    format(Mc_R2, scientific = FALSE, digits = 4, trim = TRUE),
    format(BIC, scientific = FALSE, digits = 4, trim = TRUE)
  ),
  Standard_Deviation = NA  # new column added with NA for all rows
)

Model2 <- rbind(df_model2,new_row )%>%
  rename(`Model 2` = Mean)%>%
  mutate(Parameter = if_else(str_starts(Parameter, "se_"), "", Parameter))%>%
  mutate(Parameter = if_else(str_starts(Parameter, "asc"), "Program Constant", Parameter))%>%
  mutate(Parameter = if_else(str_starts(Parameter, "b_cost"), "Cost", Parameter))%>%
  mutate(Parameter = if_else(str_starts(Parameter, "ln_wtp_local_basin"), "Basin:local", Parameter))%>%
  mutate(Parameter = if_else(str_starts(Parameter, "ln_wtp_nonlocal_basin"), "Basin:non-local", Parameter))%>%
  mutate(Parameter = if_else(str_starts(Parameter, "ln_wtp_local_sub_basin"), "Sub Basin:local", Parameter))%>%
  mutate(Parameter = if_else(str_starts(Parameter, "ln_wtp_nonlocal_sub_basin"), "Sub Basin:non-local", Parameter))


Model2 <- Model2 %>%
  mutate(WTP = case_when(
    Parameter == "Basin:local" ~ 326,
    Parameter == "Basin:non-local" ~ 160,
    Parameter == "Sub Basin:local" ~ 184,
    Parameter == "Sub Basin:non-local" ~ 92,
    TRUE ~ NA  # keep existing value if none of the conditions match
  ))

library(xtable)

# Convert to LaTeX
latex_table <- xtable(Model2)

# Save to .tex file
print(latex_table, file = "analysis/outputs/tables/Table_MNL_WTP_lognormal.tex", include.rownames = FALSE)


