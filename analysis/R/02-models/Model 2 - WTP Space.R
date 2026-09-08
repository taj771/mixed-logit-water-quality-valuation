########################################################################################
# Description: Model 2 (RPM) - WTP Space
########################################################################################

### Clear memory
rm(list = ls())

### Load Apollo library
library(apollo)
library(dplyr)
library(readr)
library(stringr)   # str_starts / str_remove in the post-estimation block below

### Set working directory (only works in RStudio)
#apollo_setWorkDir()

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName       = "Model 2_WTPspace",
  modelDescr      = "Mixed-MNL in WTP Space",
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
#### DEFINE MODEL PARAMETERS - WTP SPACE                         ####
# ################################################################# #

apollo_beta = c(
  # Price coefficient (marginal utility of cost) - constrained to be negative
  # Was 0. WTP parameters enter the utility ONLY inside b_cost * (...), so at
  # b_cost = 0 the gradient wrt all 8 WTP parameters is exactly zero and the
  # start point is the equal-shares model (LL(start) -18469.94 vs LL(0)
  # -18470.84). BGW escaped it, but that is the optimiser's first step, not a
  # property of the specification. -0.005 matches Model 2.R and the original
  # note on this line.
  b_cost  = -0.005,
  
  # Mean WTP parameters (in monetary units)
  mu_wtp_wq_local_basin = 0,
  mu_wtp_wq_nonlocal_basin = 0,
  mu_wtp_wq_local_sub_basin = 0,
  mu_wtp_wq_nonlocal_sub_basin = 0,
  
  # ASC in WTP space (this represents the "value" of choosing policy)
  mu_b_asc = 0,
  
  # Standard deviations for random parameters
  sigma_b_asc = 0.01,
  sigma_wtp_wq_local_basin = 0.1,
  sigma_wtp_wq_nonlocal_basin = 0.1,
  sigma_wtp_wq_local_sub_basin = 0.1,
  sigma_wtp_wq_nonlocal_sub_basin = 0.1
)

### Vector with names of parameters to be kept fixed at their starting value
apollo_fixed = c()

# ################################################################# #
#### DEFINE RANDOM COMPONENTS                                    ####
# ################################################################# #

### Set parameters for generating draws
apollo_draws = list(
  interDrawsType = "sobol",
  interNDraws    = 2000,
  interUnifDraws = c(),
  interNormDraws = c("draws_asc",
                     "draws_wtp_local_basin", "draws_wtp_nonlocal_basin",
                     "draws_wtp_local_sub_basin", "draws_wtp_nonlocal_sub_basin"),
  intraDrawsType = "sobol",
  intraNDraws    = 0,
  intraUnifDraws = c(),
  intraNormDraws = c()
)

### Create random parameters - NOW IN WTP SPACE
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  # ASC remains as a random utility parameter
  randcoeff[["b_asc"]] = mu_b_asc + sigma_b_asc * draws_asc 
  
  # WTP parameters - these are now directly in monetary units
  randcoeff[["wtp_wq_local_basin"]] = mu_wtp_wq_local_basin + sigma_wtp_wq_local_basin * draws_wtp_local_basin
  randcoeff[["wtp_wq_nonlocal_basin"]] = mu_wtp_wq_nonlocal_basin + sigma_wtp_wq_nonlocal_basin * draws_wtp_nonlocal_basin
  randcoeff[["wtp_wq_local_sub_basin"]] = mu_wtp_wq_local_sub_basin + sigma_wtp_wq_local_sub_basin * draws_wtp_local_sub_basin
  randcoeff[["wtp_wq_nonlocal_sub_basin"]] = mu_wtp_wq_nonlocal_sub_basin + sigma_wtp_wq_nonlocal_sub_basin * draws_wtp_nonlocal_sub_basin
  
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
  
  # Define utilities - KEY DIFFERENCE: WTP parameters multiply attributes
  # The utility for policy alternative includes: ASC + b_cost*(COST + wtp*attributes)
  V = list()
  V[["policy"]] = b_asc + 
    b_cost * (COST + 
                wtp_wq_local_basin * WQ_BASIN_LOCAL_POLICY_N +
                wtp_wq_nonlocal_basin * WQ_BASIN_NL_POLICY_N +
                wtp_wq_local_sub_basin * WQ_SUBBASIN_LOCAL_POLICY_SUBONLY_N +
                wtp_wq_nonlocal_sub_basin * WQ_SUBBASIN_NL_POLICY_SUBONLY_N)
  
  # For opt_out alternative (no cost, current conditions)
  # Note: We need to be careful here - typically only policy alternative has cost
  # The opt_out utility should reflect the value of current conditions
  V[["opt_out"]] = 
    b_cost * (wtp_wq_local_basin * WQ_BASIN_LOCAL_CURRENT_N +
                wtp_wq_nonlocal_basin * WQ_BASIN_NL_CURRENT_N +
                wtp_wq_local_sub_basin * WQ_SUBBASIN_LOCAL_CURRENT_SUBONLY_N +
                wtp_wq_nonlocal_sub_basin * WQ_SUBBASIN_NL_CURRENT_SUBONLY_N)
  
  # Alternative formulation if the above doesn't work well:
  # V[["opt_out"]] = 0  # Simple ASC for opt-out
  
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


estimates_df <- read.csv("analysis/outputs/models/Model 2_WTPspace_estimates.csv")%>%
  select(X,Estimate,Rob.std.err.,Rob.t.ratio.0.)%>%
  mutate(Significance = case_when(
    #abs(Rob.t.ratio.0.) >= 3.291 ~ "***",  # p < 0.001
    abs(Rob.t.ratio.0.) >= 2.576 ~ "***",   # p < 0.01
    abs(Rob.t.ratio.0.) >= 1.960 ~ "**",    # p < 0.05
    abs(Rob.t.ratio.0.) >= 1.645 ~ "*",    # p < 0.1
    TRUE ~ ""
  ))%>%
  # 3 dp collapses b_cost (-0.004865) to -0.005; 4 dp on its SE (0.000112)
  # printed "3e-04" straight into the LaTeX. Fixed-decimal formatting with
  # enough places for a coefficient of this magnitude, trailing zeros kept.
  mutate(Estimate_with_Stars = paste0(
    ifelse(abs(Estimate) < 0.01,
           formatC(Estimate, format = "f", digits = 6),
           formatC(Estimate, format = "f", digits = 3)),
    Significance))%>%
  rename(Parameter = X,
         SE = Rob.std.err.)%>%
  select(Parameter, Estimate_with_Stars, SE)

df3 <- estimates_df %>%
  filter(str_starts(Parameter, "b_cost"))%>%
  mutate(SE = ifelse(abs(SE) < 0.01,
                     formatC(SE, format = "f", digits = 6),
                     formatC(SE, format = "f", digits = 4)))%>%
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
  filter(str_starts(Parameter, "mu")) %>%
  rename(Mean = Estimate_with_Stars) %>%
  mutate(Parameter = str_remove(Parameter, "mu_"))

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
  filter(str_starts(Parameter, "sigma")) %>%
  rename(Standard_Deviation = Estimate_with_Stars) %>%
  mutate(Parameter = str_remove(Parameter, "sigma_"))

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
    "b_asc", "se_b_asc",
    "b_cost", "se_b_cost",
    "wtp_wq_local_basin", "se_wtp_wq_local_basin",
    "wtp_wq_nonlocal_basin", "se_wtp_wq_nonlocal_basin",
    "wtp_wq_local_sub_basin", "se_wtp_wq_local_sub_basin",
    "wtp_wq_nonlocal_sub_basin", "se_wtp_wq_nonlocal_sub_basin"
  ))) %>%
  arrange(Parameter)

model <- apollo_loadModel("analysis/outputs/models/Model 2_WTPspace")  # Replace "mymodel" with your actual modelName

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
    # format(digits=) is SIGNIFICANT digits, not decimals: it printed LL as
    # -13132 (true -13131.69) and BIC as 26376 (true 26375.55), and gets worse
    # as |LL| grows. formatC(format="f") fixes the number of decimal places.
    formatC(LL_final, format = "f", digits = 2),
    format(Mc_R2, scientific = FALSE, digits = 4, trim = TRUE),
    formatC(BIC, format = "f", digits = 2)
  ),
  Standard_Deviation = NA  # new column added with NA for all rows
)

Model2 <- rbind(df_model2,new_row )%>%
  rename(`Model 2` = Mean)%>%
  mutate(Parameter = if_else(str_starts(Parameter, "se_"), "", Parameter))%>%
  mutate(Parameter = if_else(str_starts(Parameter, "b_asc"), "Program Constant", Parameter))%>%
  mutate(Parameter = if_else(str_starts(Parameter, "b_cost"), "Cost", Parameter))%>%
  mutate(Parameter = if_else(str_starts(Parameter, "wtp_wq_local_basin"), "Basin:local", Parameter))%>%
  mutate(Parameter = if_else(str_starts(Parameter, "wtp_wq_nonlocal_basin"), "Basin:non-local", Parameter))%>%
  mutate(Parameter = if_else(str_starts(Parameter, "wtp_wq_local_sub_basin"), "Sub Basin:local", Parameter))%>%
  mutate(Parameter = if_else(str_starts(Parameter, "wtp_wq_nonlocal_sub_basin"), "Sub Basin:non-local", Parameter))


library(xtable)

# Convert to LaTeX
latex_table <- xtable(Model2)

# Save to .tex file
print(latex_table, file = "analysis/outputs/tables/Table_MNL_WTP.tex", include.rownames = FALSE)







