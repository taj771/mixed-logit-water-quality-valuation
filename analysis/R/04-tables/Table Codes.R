###############################################################################
# Load and format model estimates neatly
###############################################################################

rm(list = ls())
library(dplyr)

# Read and prepare estimates
estimates_df <- read.csv("analysis/outputs/models/Model 1_estimates.csv") %>%
  select(Parameter = X,
         Estimate,
         SE = Rob.std.err.,
         t_value = Rob.t.ratio.0.) %>%
  mutate(
    Significance = case_when(
      abs(t_value) >= 2.576 ~ "***",   # p < 0.01
      abs(t_value) >= 1.960 ~ "**",    # p < 0.05
      abs(t_value) >= 1.645 ~ "*",     # p < 0.1
      TRUE ~ ""
    ),
    Estimate = ifelse(str_starts(Parameter, "sigma"), abs(Estimate), Estimate),
    Estimate_with_Stars = paste0(
      ifelse(abs(Estimate) < 0.01,
             formatC(Estimate, format = "f", digits = 6),
             formatC(Estimate, format = "f", digits = 3)),
      Significance)
  )

# Combine estimates and standard errors
df <- estimates_df %>%
  transmute(Parameter, Estimate = Estimate_with_Stars) %>%
  bind_rows(
    estimates_df %>%
      transmute(Parameter = paste0("se_", Parameter),
                Estimate = paste0("(", ifelse(abs(SE) < 0.01,
                                              formatC(SE, format = "f", digits = 6),
                                              formatC(SE, format = "f", digits = 4)), ")"))
  ) %>%
  mutate(Parameter = factor(Parameter, levels = c(
    "b_asc", "se_b_asc",
    "b_cost", "se_b_cost",
    "b_wq_local_basin", "se_b_wq_local_basin",
    "b_wq_nonlocal_basin", "se_b_wq_nonlocal_basin",
    "b_wq_local_sub_basin", "se_b_wq_local_sub_basin",
    "b_wq_nonlocal_sub_basin", "se_b_wq_nonlocal_sub_basin"
  ))) %>%
  arrange(Parameter)


model <- apollo_loadModel("analysis/outputs/models/Model 1")  # Replace "mymodel" with your actual modelName

N_Obs <- model$nIndivs
N_vote <- model$nObs
LL_final <- model$LLout
Mc_R2 <- model$adjRho2_C
BIC <- model$BIC


# Create new_row data frame with multiple rows
new_row <- data.frame(
  Parameter = c("Individuals", "Voting Scenario", "Log-Likelihood at solution", "McFadden’s R2", "BIC"),
  Estimate = c(
    format(N_Obs, scientific = FALSE, digits = 4, trim = TRUE),
    format(N_vote, scientific = FALSE, digits = 4, trim = TRUE),
    formatC(LL_final, format = "f", digits = 2),
    format(Mc_R2, scientific = FALSE, digits = 4, trim = TRUE),
    formatC(BIC, format = "f", digits = 2)
  )
)


Model1 <- rbind(df,new_row )%>%
  rename(`Model 1` = Estimate)

### Model 2

estimates_df <- read.csv("analysis/outputs/models/Model 2_estimates.csv")%>%
  select(X,Estimate,Rob.std.err.,Rob.t.ratio.0.)%>%
  mutate(Significance = case_when(
    #abs(Rob.t.ratio.0.) >= 3.291 ~ "***",  # p < 0.001
    abs(Rob.t.ratio.0.) >= 2.576 ~ "***",   # p < 0.01
    abs(Rob.t.ratio.0.) >= 1.960 ~ "**",    # p < 0.05
    abs(Rob.t.ratio.0.) >= 1.645 ~ "*",    # p < 0.1
    TRUE ~ ""
  ))%>%
  # A normal mixing SD is identified only up to sign, so Apollo can return e.g.
  # sigma_b_wq_nonlocal_sub_basin = -0.4168 - same likelihood as +0.4168. Printing
  # it negative in a "Standard Deviation" column is wrong, and a sign flip on a
  # re-estimation reads as a spurious result change. abs() only on sigma rows.
  mutate(Estimate = ifelse(str_starts(X, "sigma"), abs(Estimate), Estimate))%>%
  # round(x, 3) collapsed b_cost (-0.004864) to -0.005; keep more places for
  # small coefficients and use fixed-decimal formatting so trailing zeros survive.
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
  mutate(SE = formatC(SE, format = "f", digits = 3))%>%
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
  mutate(SE = formatC(SE, format = "f", digits = 3))%>%
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
    "b_wq_local_basin", "se_b_wq_local_basin",
    "b_wq_nonlocal_basin", "se_b_wq_nonlocal_basin",
    "b_wq_local_sub_basin", "se_b_wq_local_sub_basin",
    "b_wq_nonlocal_sub_basin", "se_b_wq_nonlocal_sub_basin"
  ))) %>%
  arrange(Parameter)

model <- apollo_loadModel("analysis/outputs/models/Model 2")  # Replace "mymodel" with your actual modelName

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
    formatC(LL_final, format = "f", digits = 2),
    format(Mc_R2, scientific = FALSE, digits = 4, trim = TRUE),
    formatC(BIC, format = "f", digits = 2)
  ),
  Standard_Deviation = NA  # new column added with NA for all rows
)

Model2 <- rbind(df_model2,new_row )%>%
  rename(`Model 2` = Mean)


Table1 <- Model1%>%
  left_join(Model2)%>%
  mutate(Parameter = if_else(str_starts(Parameter, "se_"), "", Parameter))%>%
  mutate(Parameter = if_else(str_starts(Parameter, "b_asc"), "Program Constant", Parameter))%>%
  mutate(Parameter = if_else(str_starts(Parameter, "b_cost"), "Cost", Parameter))%>%
  mutate(Parameter = if_else(str_starts(Parameter, "b_wq_local_basin"), "Basin:local", Parameter))%>%
  mutate(Parameter = if_else(str_starts(Parameter, "b_wq_nonlocal_basin"), "Basin:non-local", Parameter))%>%
  mutate(Parameter = if_else(str_starts(Parameter, "b_wq_local_sub_basin"), "Sub Basin:local", Parameter))%>%
  mutate(Parameter = if_else(str_starts(Parameter, "b_wq_nonlocal_sub_basin"), "Sub Basin:non-local", Parameter))
  
library(xtable)

# Convert to LaTeX
latex_table <- xtable(Table1)

# Save to .tex file
print(latex_table, file = "analysis/outputs/tables/Table_1.tex", include.rownames = FALSE)




############# Table 2 ############# Table 2 ############# Table 2




### Model 3

estimates_df <- read.csv("analysis/outputs/models/Model 3_estimates.csv")%>%
  select(X,Estimate,Rob.std.err.,Rob.t.ratio.0.)%>%
  mutate(Significance = case_when(
    #abs(Rob.t.ratio.0.) >= 3.291 ~ "***",  # p < 0.001
    abs(Rob.t.ratio.0.) >= 2.576 ~ "***",   # p < 0.01
    abs(Rob.t.ratio.0.) >= 1.960 ~ "**",    # p < 0.05
    abs(Rob.t.ratio.0.) >= 1.645 ~ "*",    # p < 0.1
    TRUE ~ ""
  ))%>%
  # A normal mixing SD is identified only up to sign, so Apollo can return e.g.
  # sigma_b_wq_nonlocal_sub_basin = -0.4168 - same likelihood as +0.4168. Printing
  # it negative in a "Standard Deviation" column is wrong, and a sign flip on a
  # re-estimation reads as a spurious result change. abs() only on sigma rows.
  mutate(Estimate = ifelse(str_starts(X, "sigma"), abs(Estimate), Estimate))%>%
  # round(x, 3) collapsed b_cost (-0.004864) to -0.005; keep more places for
  # small coefficients and use fixed-decimal formatting so trailing zeros survive.
  mutate(Estimate_with_Stars = paste0(
    ifelse(abs(Estimate) < 0.01,
           formatC(Estimate, format = "f", digits = 6),
           formatC(Estimate, format = "f", digits = 3)),
    Significance))%>%
  rename(Parameter = X,
         SE = Rob.std.err.)%>%
  select(Parameter, Estimate_with_Stars, SE)

df3 <- estimates_df %>%
  filter(str_starts(Parameter, "b_cost") | str_starts(Parameter, "b_basesq_obs")) %>%
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
  mutate(SE = formatC(SE, format = "f", digits = 3))%>%
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
  mutate(SE = formatC(SE, format = "f", digits = 3))%>%
  mutate(SE = paste("(", SE, ")", sep = ""))%>%
  rename(Standard_Deviation=SE)%>%
  mutate(Parameter = paste0("se_", Parameter))



df2 <- rbind(df2_mean,df2_SE)%>%
  arrange(Parameter)

df1 <- rbind(df1,df3)


df_model3 <- df1%>%
  left_join(df2)

model <- apollo_loadModel("analysis/outputs/models/Model 3")  # Replace "mymodel" with your actual modelName

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
    formatC(LL_final, format = "f", digits = 2),
    format(Mc_R2, scientific = FALSE, digits = 4, trim = TRUE),
    formatC(BIC, format = "f", digits = 2)
  ),
  Standard_Deviation = NA # new column added with NA for all rows
  
)

extra_row <- data.frame(
  Parameter = c("b_basesq_exp", "se_b_basesq_exp"),
  Mean = rep(NA, 2),
  Standard_Deviation = rep(NA, 2)
)


new_row <- rbind(new_row, extra_row)

Model3 <- rbind(df_model3, new_row) %>%
  rename(`Model 3` = Mean) %>%
  mutate(
    Parameter = factor(Parameter, levels = c(
      "b_asc", "se_b_asc",
      "b_cost", "se_b_cost",
      "b_wq_local_basin", "se_b_wq_local_basin",
      "b_wq_nonlocal_basin", "se_b_wq_nonlocal_basin",
      "b_wq_local_sub_basin", "se_b_wq_local_sub_basin",
      "b_wq_nonlocal_sub_basin", "se_b_wq_nonlocal_sub_basin",
      "b_basesq_obs","se_b_basesq_obs",
      "b_basesq_exp", "se_b_basesq_exp",
      "Individuals", "Voting Scenario", "Log-Likelihood at solution", "McFadden’s R2", "BIC"
    ))
  ) %>%
  arrange(Parameter)%>%
  rename(Standard_Deviation_m3 = Standard_Deviation)

### Model 4

### Model 4

estimates_df <- read.csv("analysis/outputs/models/Model 4_estimates.csv")%>%
  select(X,Estimate,Rob.std.err.,Rob.t.ratio.0.)%>%
  mutate(Significance = case_when(
    #abs(Rob.t.ratio.0.) >= 3.291 ~ "***",  # p < 0.001
    abs(Rob.t.ratio.0.) >= 2.576 ~ "***",   # p < 0.01
    abs(Rob.t.ratio.0.) >= 1.960 ~ "**",    # p < 0.05
    abs(Rob.t.ratio.0.) >= 1.645 ~ "*",    # p < 0.1
    TRUE ~ ""
  ))%>%
  # A normal mixing SD is identified only up to sign, so Apollo can return e.g.
  # sigma_b_wq_nonlocal_sub_basin = -0.4168 - same likelihood as +0.4168. Printing
  # it negative in a "Standard Deviation" column is wrong, and a sign flip on a
  # re-estimation reads as a spurious result change. abs() only on sigma rows.
  mutate(Estimate = ifelse(str_starts(X, "sigma"), abs(Estimate), Estimate))%>%
  # round(x, 3) collapsed b_cost (-0.004864) to -0.005; keep more places for
  # small coefficients and use fixed-decimal formatting so trailing zeros survive.
  mutate(Estimate_with_Stars = paste0(
    ifelse(abs(Estimate) < 0.01,
           formatC(Estimate, format = "f", digits = 6),
           formatC(Estimate, format = "f", digits = 3)),
    Significance))%>%
  rename(Parameter = X,
         SE = Rob.std.err.)%>%
  select(Parameter, Estimate_with_Stars, SE)

df3 <- estimates_df %>%
  filter(
    str_starts(Parameter, "b_cost") |
      str_starts(Parameter, "b_basesq_obs") |
      str_starts(Parameter, "b_basesq_exp") 
  ) %>%
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
  mutate(SE = formatC(SE, format = "f", digits = 3))%>%
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
  mutate(SE = formatC(SE, format = "f", digits = 3))%>%
  mutate(SE = paste("(", SE, ")", sep = ""))%>%
  rename(Standard_Deviation=SE)%>%
  mutate(Parameter = paste0("se_", Parameter))



df2 <- rbind(df2_mean,df2_SE)%>%
  arrange(Parameter)

df1 <- rbind(df1,df3)


df_model4 <- df1%>%
  left_join(df2)

model <- apollo_loadModel("analysis/outputs/models/Model 4")  # Replace "mymodel" with your actual modelName

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
    formatC(LL_final, format = "f", digits = 2),
    format(Mc_R2, scientific = FALSE, digits = 4, trim = TRUE),
    formatC(BIC, format = "f", digits = 2)
  ),
  Standard_Deviation = NA # new column added with NA for all rows
  
)

#extra_row <- data.frame(
#  Parameter = c("b_basewq","se_b_basewq"),
#  Mean = rep(NA, 2),
#  Standard_Deviation = rep(NA, 2)
#)


#new_row <- rbind(new_row, extra_row)

Model4 <- rbind(df_model4, new_row) %>%
  rename(`Model 4` = Mean) %>%
  mutate(
    Parameter = factor(Parameter, levels = c(
      "b_asc", "se_b_asc",
      "b_cost", "se_b_cost",
      "b_basesq_exp", "se_b_basesq_exp",
      "b_wq_local_basin", "se_b_wq_local_basin",
      "b_wq_nonlocal_basin", "se_b_wq_nonlocal_basin",
      "b_wq_local_sub_basin", "se_b_wq_local_sub_basin",
      "b_wq_nonlocal_sub_basin", "se_b_wq_nonlocal_sub_basin",
      "b_basesq_obs","se_b_basesq_obs",

      "Individuals", "Voting Scenario", "Log-Likelihood at solution", "McFadden’s R2", "BIC"
    ))
  ) %>%
  arrange(Parameter)%>%
  rename(Standard_Deviation_m4 = Standard_Deviation)



Table2 <- Model4%>%
  #left_join(Model4)%>%
  mutate(Parameter = if_else(str_starts(Parameter, "se_"), "", Parameter))%>%
  mutate(Parameter = if_else(Parameter == "b_asc", "Program Constant", Parameter))%>%
  mutate(Parameter = if_else(Parameter == "b_cost", "Cost", Parameter))%>%
  mutate(Parameter = if_else(Parameter == "b_wq_local_basin", "Basin:local", Parameter))%>%
  mutate(Parameter = if_else(Parameter == "b_wq_nonlocal_basin", "Basin:non-local", Parameter))%>%
  mutate(Parameter = if_else(Parameter == "b_wq_local_sub_basin", "Sub Basin:local", Parameter))%>%
  mutate(Parameter = if_else(Parameter == "b_wq_nonlocal_sub_basin", "Sub Basin:non-local", Parameter))%>%
  
  mutate(Parameter = if_else(Parameter == "b_basesq_obs", "Current WQ Level", Parameter))%>%
  mutate(Parameter = if_else(Parameter== "b_basesq_exp", " Experimental Variation in SQ", Parameter))




# Convert to LaTeX
latex_table <- xtable(Table2)

# Save to .tex file
print(latex_table, file = "analysis/outputs/tables/Table_2.tex", include.rownames = FALSE)



### Model 5

estimates_df <- read.csv("analysis/outputs/models/Model 5_estimates.csv")%>%
  select(X,Estimate,Rob.std.err.,Rob.t.ratio.0.)%>%
  mutate(Significance = case_when(
    #abs(Rob.t.ratio.0.) >= 3.291 ~ "***",  # p < 0.001
    abs(Rob.t.ratio.0.) >= 2.576 ~ "***",   # p < 0.01
    abs(Rob.t.ratio.0.) >= 1.960 ~ "**",    # p < 0.05
    abs(Rob.t.ratio.0.) >= 1.645 ~ "*",    # p < 0.1
    TRUE ~ ""
  ))%>%
  # A normal mixing SD is identified only up to sign, so Apollo can return e.g.
  # sigma_b_wq_nonlocal_sub_basin = -0.4168 - same likelihood as +0.4168. Printing
  # it negative in a "Standard Deviation" column is wrong, and a sign flip on a
  # re-estimation reads as a spurious result change. abs() only on sigma rows.
  mutate(Estimate = ifelse(str_starts(X, "sigma"), abs(Estimate), Estimate))%>%
  # round(x, 3) collapsed b_cost (-0.004864) to -0.005; keep more places for
  # small coefficients and use fixed-decimal formatting so trailing zeros survive.
  mutate(Estimate_with_Stars = paste0(
    ifelse(abs(Estimate) < 0.01,
           formatC(Estimate, format = "f", digits = 6),
           formatC(Estimate, format = "f", digits = 3)),
    Significance))%>%
  rename(Parameter = X,
         SE = Rob.std.err.)%>%
  select(Parameter, Estimate_with_Stars, SE)

df3 <- estimates_df %>%
  filter(str_starts(Parameter, "b_cost")) %>%
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
  mutate(SE = formatC(SE, format = "f", digits = 3))%>%
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
  mutate(SE = formatC(SE, format = "f", digits = 3))%>%
  mutate(SE = paste("(", SE, ")", sep = ""))%>%
  rename(Standard_Deviation=SE)%>%
  mutate(Parameter = paste0("se_", Parameter))



df2 <- rbind(df2_mean,df2_SE)%>%
  arrange(Parameter)

df1 <- rbind(df1,df3)


df_model5 <- df1%>%
  left_join(df2)

model <- apollo_loadModel("analysis/outputs/models/Model 5")  # Replace "mymodel" with your actual modelName

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
    formatC(LL_final, format = "f", digits = 2),
    format(Mc_R2, scientific = FALSE, digits = 4, trim = TRUE),
    formatC(BIC, format = "f", digits = 2)
  ),
  Standard_Deviation = NA # new column added with NA for all rows
  
)


extra_row <- data.frame(
  Parameter = c("b_asc_home_prov_share", "se_b_asc_home_prov_share","b_asc_rectrip_choice","se_b_asc_rectrip_choice"),
  Mean = rep(NA, 4),
  Standard_Deviation = rep(NA, 4)
)



new_row <- rbind(new_row, extra_row)

Model5 <- rbind(df_model5, new_row) %>%
  rename(`Model 5` = Mean) %>%
  mutate(
    Parameter = factor(
      Parameter,
      levels = c(
        "b_asc", "se_b_asc",
        "b_cost", "se_b_cost",
        
        "b_asc_home_prov_share", "se_b_asc_home_prov_share",
        "b_asc_rectrip_choice", "se_b_asc_rectrip_choice",
        
        "b_wq_local_basin", "se_b_wq_local_basin",
        "b_wq_nonlocal_basin", "se_b_wq_nonlocal_basin",
        "b_wq_local_sub_basin", "se_b_wq_local_sub_basin",
        "b_wq_nonlocal_sub_basin", "se_b_wq_nonlocal_sub_basin",
        
        "b_wq_nl_local_adj", "se_b_wq_nl_local_adj",
        
        "Individuals", "Voting Scenario", "Log-Likelihood at solution",
        "McFadden’s R2", "BIC"
      )
    )
  ) %>%
  arrange(Parameter) %>%
  rename(Standard_Deviation_m5 = Standard_Deviation)

### Model 6

estimates_df <- read.csv("analysis/outputs/models/Model 6_estimates.csv")%>%
  select(X,Estimate,Rob.std.err.,Rob.t.ratio.0.)%>%
  mutate(Significance = case_when(
    #abs(Rob.t.ratio.0.) >= 3.291 ~ "***",  # p < 0.001
    abs(Rob.t.ratio.0.) >= 2.576 ~ "***",   # p < 0.01
    abs(Rob.t.ratio.0.) >= 1.960 ~ "**",    # p < 0.05
    abs(Rob.t.ratio.0.) >= 1.645 ~ "*",    # p < 0.1
    TRUE ~ ""
  ))%>%
  # A normal mixing SD is identified only up to sign, so Apollo can return e.g.
  # sigma_b_wq_nonlocal_sub_basin = -0.4168 - same likelihood as +0.4168. Printing
  # it negative in a "Standard Deviation" column is wrong, and a sign flip on a
  # re-estimation reads as a spurious result change. abs() only on sigma rows.
  mutate(Estimate = ifelse(str_starts(X, "sigma"), abs(Estimate), Estimate))%>%
  # round(x, 3) collapsed b_cost (-0.004864) to -0.005; keep more places for
  # small coefficients and use fixed-decimal formatting so trailing zeros survive.
  mutate(Estimate_with_Stars = paste0(
    ifelse(abs(Estimate) < 0.01,
           formatC(Estimate, format = "f", digits = 6),
           formatC(Estimate, format = "f", digits = 3)),
    Significance))%>%
  rename(Parameter = X,
         SE = Rob.std.err.)%>%
  select(Parameter, Estimate_with_Stars, SE)

df3 <- estimates_df %>%
  filter(
    str_starts(Parameter, "b_cost") |
      str_starts(Parameter, "b_asc_home_prov_share")
  ) %>%
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
  mutate(SE = formatC(SE, format = "f", digits = 3))%>%
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
  mutate(SE = formatC(SE, format = "f", digits = 3))%>%
  mutate(SE = paste("(", SE, ")", sep = ""))%>%
  rename(Standard_Deviation=SE)%>%
  mutate(Parameter = paste0("se_", Parameter))



df2 <- rbind(df2_mean,df2_SE)%>%
  arrange(Parameter)

df1 <- rbind(df1,df3)


df_model6 <- df1%>%
  left_join(df2)

model <- apollo_loadModel("analysis/outputs/models/Model 6")  # Replace "mymodel" with your actual modelName

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
    formatC(LL_final, format = "f", digits = 2),
    format(Mc_R2, scientific = FALSE, digits = 4, trim = TRUE),
    formatC(BIC, format = "f", digits = 2)
  ),
  Standard_Deviation = NA # new column added with NA for all rows
  
)


extra_row <- data.frame(
  Parameter = c("b_asc_rectrip_choice","se_b_asc_rectrip_choice",
                "b_wq_nl_local_adj", "se_b_wq_nl_local_adj"),
  Mean = rep(NA, 4),
  Standard_Deviation = rep(NA, 4)
)



new_row <- rbind(new_row, extra_row)

Model6 <- rbind(df_model6, new_row) %>%
  rename(`Model 6` = Mean) %>%
  mutate(
    Parameter = factor(
      Parameter,
      levels = c(
        "b_asc", "se_b_asc",
        "b_cost", "se_b_cost",
        
        "b_asc_home_prov_share", "se_b_asc_home_prov_share",
        "b_asc_rectrip_choice", "se_b_asc_rectrip_choice",
        
        "b_wq_local_basin", "se_b_wq_local_basin",
        "b_wq_nonlocal_basin", "se_b_wq_nonlocal_basin",
        "b_wq_local_sub_basin", "se_b_wq_local_sub_basin",
        "b_wq_nonlocal_sub_basin", "se_b_wq_nonlocal_sub_basin",
        
        "b_wq_nl_local_adj", "se_b_wq_nl_local_adj",
        
        "Individuals", "Voting Scenario", "Log-Likelihood at solution",
        "McFadden’s R2", "BIC"
      )
    )
  ) %>%
  arrange(Parameter) %>%
  rename(Standard_Deviation_m6 = Standard_Deviation)


### Model 7

estimates_df <- read.csv("analysis/outputs/models/Model 7_estimates.csv")%>%
  select(X,Estimate,Rob.std.err.,Rob.t.ratio.0.)%>%
  mutate(Significance = case_when(
    #abs(Rob.t.ratio.0.) >= 3.291 ~ "***",  # p < 0.001
    abs(Rob.t.ratio.0.) >= 2.576 ~ "***",   # p < 0.01
    abs(Rob.t.ratio.0.) >= 1.960 ~ "**",    # p < 0.05
    abs(Rob.t.ratio.0.) >= 1.645 ~ "*",    # p < 0.1
    TRUE ~ ""
  ))%>%
  # A normal mixing SD is identified only up to sign, so Apollo can return e.g.
  # sigma_b_wq_nonlocal_sub_basin = -0.4168 - same likelihood as +0.4168. Printing
  # it negative in a "Standard Deviation" column is wrong, and a sign flip on a
  # re-estimation reads as a spurious result change. abs() only on sigma rows.
  mutate(Estimate = ifelse(str_starts(X, "sigma"), abs(Estimate), Estimate))%>%
  # round(x, 3) collapsed b_cost (-0.004864) to -0.005; keep more places for
  # small coefficients and use fixed-decimal formatting so trailing zeros survive.
  mutate(Estimate_with_Stars = paste0(
    ifelse(abs(Estimate) < 0.01,
           formatC(Estimate, format = "f", digits = 6),
           formatC(Estimate, format = "f", digits = 3)),
    Significance))%>%
  rename(Parameter = X,
         SE = Rob.std.err.)%>%
  select(Parameter, Estimate_with_Stars, SE)

df3 <- estimates_df %>%
  filter(
    str_starts(Parameter, "b_cost") |
      str_starts(Parameter, "b_asc_rectrip_choice")
  ) %>%
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
  mutate(SE = formatC(SE, format = "f", digits = 3))%>%
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
  mutate(SE = formatC(SE, format = "f", digits = 3))%>%
  mutate(SE = paste("(", SE, ")", sep = ""))%>%
  rename(Standard_Deviation=SE)%>%
  mutate(Parameter = paste0("se_", Parameter))



df2 <- rbind(df2_mean,df2_SE)%>%
  arrange(Parameter)

df1 <- rbind(df1,df3)


df_model7 <- df1%>%
  left_join(df2)

model <- apollo_loadModel("analysis/outputs/models/Model 7")  # Replace "mymodel" with your actual modelName

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
    formatC(LL_final, format = "f", digits = 2),
    format(Mc_R2, scientific = FALSE, digits = 4, trim = TRUE),
    formatC(BIC, format = "f", digits = 2)
  ),
  Standard_Deviation = NA # new column added with NA for all rows
  
)


extra_row <- data.frame(
  Parameter = c("b_asc_home_prov_share", "se_b_asc_home_prov_share",
                "b_wq_nl_local_adj", "se_b_wq_nl_local_adj"
  ),
  Mean = rep(NA, 4),
  Standard_Deviation = rep(NA, 4)
)


new_row <- rbind(new_row, extra_row)

Model7 <- rbind(df_model7, new_row) %>%
  rename(`Model 7` = Mean) %>%
  mutate(
    Parameter = factor(
      Parameter,
      levels = c(
        "b_asc", "se_b_asc",
        "b_cost", "se_b_cost",
        
        "b_asc_home_prov_share", "se_b_asc_home_prov_share",
        "b_asc_rectrip_choice", "se_b_asc_rectrip_choice",
        
        
        "b_wq_local_basin", "se_b_wq_local_basin",
        "b_wq_nonlocal_basin", "se_b_wq_nonlocal_basin",
        "b_wq_local_sub_basin", "se_b_wq_local_sub_basin",
        "b_wq_nonlocal_sub_basin", "se_b_wq_nonlocal_sub_basin",
        
        "b_wq_nl_local_adj", "se_b_wq_nl_local_adj",
      
        "Individuals", "Voting Scenario", "Log-Likelihood at solution",
        "McFadden’s R2", "BIC"
      )
    )
  ) %>%
  arrange(Parameter) %>%
  rename(Standard_Deviation_m7 = Standard_Deviation)



Table3 <- Model5 %>%
  left_join(Model6, by = "Parameter") %>%
  left_join(Model7, by = "Parameter") %>%
  mutate(Parameter = if_else(str_starts(Parameter, "se_"), "", Parameter))%>%
  mutate(Parameter = if_else(Parameter == "b_asc", "Program Constant", Parameter))%>%
  mutate(Parameter = if_else(Parameter == "b_cost", "Cost", Parameter))%>%
  mutate(Parameter = if_else(Parameter == "b_wq_local_basin", "Basin:local", Parameter))%>%
  mutate(Parameter = if_else(Parameter == "b_wq_nonlocal_basin", "Basin:non-local", Parameter))%>%
  mutate(Parameter = if_else(Parameter == "b_wq_local_sub_basin", "Sub Basin:local", Parameter))%>%
  mutate(Parameter = if_else(Parameter == "b_wq_nonlocal_sub_basin", "Sub Basin:non-local", Parameter))%>%
  
  mutate(Parameter = if_else(Parameter == "b_wq_nl_local_adj", "Non-Local Choice Adjacent to Local Sub/Basin", Parameter))%>%
  mutate(Parameter = if_else(Parameter== "b_asc_home_prov_share", "Home Province Share of Policy Site", Parameter))%>%
  mutate(Parameter = if_else(Parameter== "b_asc_rectrip_choice", "Recreational Trip to Choice Sub/Basin", Parameter))
  

# Convert to LaTeX
latex_table <- xtable(Table3)

# Save to .tex file
print(latex_table, file = "analysis/outputs/tables/Table_3.tex", include.rownames = FALSE)











  
