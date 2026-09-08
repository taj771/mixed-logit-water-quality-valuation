rm(list = ls())

estimates_df <- read.csv("analysis/outputs/models/Model 4.4_estimates.csv")%>%
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
  filter(
    str_starts(Parameter, "b_cost") |
      str_starts(Parameter, "b_basesq_obs") |
      str_starts(Parameter, "b_basesq_exp") 
  ) %>%
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


df_model4 <- df1%>%
  left_join(df2)

model <- apollo_loadModel("analysis/outputs/models/Model 4.4")  # Replace "mymodel" with your actual modelName

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
  Standard_Deviation = NA # new column added with NA for all rows
  
)




Model4.6 <- rbind(df_model4, new_row) %>%
  rename(`Model 4.6` = Mean) %>%
  mutate(
    Parameter = factor(Parameter, levels = c(
      "b_asc", "se_b_asc",
      "b_cost", "se_b_cost",
      "b_basesq_obs","se_b_basesq_obs",
      "b_basesq_exp", "se_b_basesq_exp",
      "b_basesq_obs_local","se_b_basesq_obs_local",
      "b_basesq_obs_nonlocal","se_b_basesq_obs_nonlocal",
      "b_basesq_exp_local","se_b_basesq_exp_local",
      "basesq_exp_nonlocal","se_basesq_exp_nonlocal",
      
      
      "b_wq_local_basin", "se_b_wq_local_basin",
      "b_wq_nonlocal_basin", "se_b_wq_nonlocal_basin",
      "b_wq_local_sub_basin", "se_b_wq_local_sub_basin",
      "b_wq_nonlocal_sub_basin", "se_b_wq_nonlocal_sub_basin",
      
      "Individuals", "Voting Scenario", "Log-Likelihood at solution", "McFadden’s R2", "BIC"
    ))
  ) %>%
  arrange(Parameter)%>%
  rename(Standard_Deviation_m4.6 = Standard_Deviation)


Table2 <- Model4.6%>%
  mutate(Parameter = if_else(str_starts(Parameter, "se_"), "", Parameter))%>%
  mutate(Parameter = if_else(Parameter == "b_asc", "Program Constant", Parameter))%>%
  mutate(Parameter = if_else(Parameter == "b_cost", "Cost", Parameter))%>%
  mutate(Parameter = if_else(Parameter == "b_wq_local_basin", "Basin:local", Parameter))%>%
  mutate(Parameter = if_else(Parameter == "b_wq_nonlocal_basin", "Basin:non-local", Parameter))%>%
  mutate(Parameter = if_else(Parameter == "b_wq_local_sub_basin", "Sub Basin:local", Parameter))%>%
  mutate(Parameter = if_else(Parameter == "b_wq_nonlocal_sub_basin", "Sub Basin:non-local", Parameter))%>%
  
  mutate(Parameter = if_else(Parameter == "b_basesq_obs", "Current WQ Level", Parameter))%>%
  mutate(Parameter = if_else(Parameter== "b_basesq_exp", "Current WQ Level", Parameter))%>%
  
  mutate(Parameter = if_else(Parameter== "b_basesq_obs_local", "Current WQ Level:Local", Parameter))%>%
  mutate(Parameter = if_else(Parameter== "b_basesq_obs_nonlocal", "Current WQ Level:Non-local", Parameter))%>%
  
  mutate(Parameter = if_else(Parameter== "b_basesq_exp_local", "Experimental Variation in SQ:Local", Parameter))%>%
  mutate(Parameter = if_else(Parameter== "basesq_exp_nonlocal", "Experimental Variation in SQ:Non-local", Parameter))



# Convert to LaTeX
latex_table <- xtable(Table2)

# Save to .tex file
print(latex_table, file = "analysis/outputs/tables/Table_2.tex", include.rownames = FALSE)
