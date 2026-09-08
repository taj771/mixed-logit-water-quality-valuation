model <- apollo_loadModel("analysis/outputs/models/Model 4")  # Replace "mymodel" with your actual modelName


# Display model outputs
apollo_modelOutput(model)

# Extract coefficients and covariance matrix
coef_values <- model$estimate
vcov_matrix <- model$robvarcov


# baseline = 0 (best), current WQ = 5 ---> one unit improvement
twtp_5_4 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "(-(mu_b_wq_nonlocal_basin*(5-4) + b_basewq_nonlocal*5*(5-4) + b_wq_x_bl_nonlocal*0*(5-4)) / -b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(Scenario = "5 to 4") %>%
  relocate(Scenario, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))

# baseline = 0 (best), current WQ = 4 ---> one unit improvement
twtp_4_3 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "(-(mu_b_wq_nonlocal_basin*(4-3) + b_basewq_nonlocal*4*(4-3) + b_wq_x_bl_nonlocal*0*(4-3)) / -b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(Scenario = "4 to 3") %>%
  relocate(Scenario, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))

# baseline = 0 (best), current WQ = 3 ---> one unit improvement
twtp_3_2 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "(-(mu_b_wq_nonlocal_basin*(3-2) + b_basewq_nonlocal*3*(3-2) + b_wq_x_bl_nonlocal*0*(3-2)) / -b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(Scenario = "3 to 2") %>%
  relocate(Scenario, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))


# baseline = 0 (best), current WQ = 2 ---> one unit improvement
twtp_2_1 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "(-(mu_b_wq_nonlocal_basin*(2-1) + b_basewq_nonlocal*2*(2-1) + b_wq_x_bl_nonlocal*0*(2-1)) / -b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(Scenario = "2 to 1") %>%
  relocate(Scenario, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))




# baseline = 0 (best), current WQ = 5 ---> 3 (two unit improvement)
twtp_5_3 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "(-(mu_b_wq_nonlocal_basin*(5-3) + b_basewq_nonlocal*5*(5-3) + b_wq_x_bl_nonlocal*0*(5-3)) / -b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(Scenario = "5 to 3") %>%
  relocate(Scenario, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))


# baseline = 0 (best), current WQ = 5 ---> 2 (three unit improvement)
twtp_5_2 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "(-(mu_b_wq_nonlocal_basin*(5-2) + b_basewq_nonlocal*5*(5-2) + b_wq_x_bl_nonlocal*0*(5-2)) / -b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(Scenario = "5 to 2") %>%
  relocate(Scenario, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))


# baseline = 0 (best), current WQ = 5 ---> 1 (four unit improvement)
twtp_5_1 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "(-(mu_b_wq_nonlocal_basin*(5-1) + b_basewq_nonlocal*5*(5-1) + b_wq_x_bl_nonlocal*0*(5-1)) / -b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(Scenario = "5 to 1") %>%
  relocate(Scenario, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))



# baseline = 0 (best), current WQ = 4 ---> 2 (two unit improvement)
twtp_4_2 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "(-(mu_b_wq_nonlocal_basin*(4-2) + b_basewq_nonlocal*4*(4-2) + b_wq_x_bl_nonlocal*0*(4-2)) / -b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(Scenario = "4 to 2") %>%
  relocate(Scenario, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))



# baseline = 0 (best), current WQ = 4 ---> 1 (three unit improvement)
twtp_4_1 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "(-(mu_b_wq_nonlocal_basin*(4-1) + b_basewq_nonlocal*4*(4-1) + b_wq_x_bl_nonlocal*0*(4-1)) / -b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(Scenario = "4 to 1") %>%
  relocate(Scenario, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))



# baseline = 0 (best), current WQ = 3 --->  1two unit improvement
twtp_3_1 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "(-(mu_b_wq_nonlocal_basin*(3-1) + b_basewq_nonlocal*3*(3-1) + b_wq_x_bl_nonlocal*0*(3-1)) / -b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(Scenario = "3 to 1") %>%
  relocate(Scenario, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))



# Stepwise WTP: 5->4 + 4->3
stepwise_wtp <- data.frame(
  Scenario = "Stepwise 5 to 3",
  Estimate = twtp_5_4$Estimate + twtp_4_3$Estimate,
  SE = sqrt(twtp_5_4$SE^2 + twtp_5_4$SE^2)   # sum of variances
)

# Compute 95% CI
stepwise_wtp <- stepwise_wtp %>%
  mutate(
    `2.5 %` = Estimate - 1.96 * SE,
    `97.5 %` = Estimate + 1.96 * SE
  )

df1 <- rbind(twtp_5_3,stepwise_wtp)%>%
  mutate(delta = "2 Units")%>%
  mutate(group = "one")


# Stepwise WTP: 5->4 + 4->3 + 3->2
stepwise_wtp <- data.frame(
  Scenario = "Stepwise 5 to 2 ",
  Estimate = twtp_5_4$Estimate + twtp_4_3$Estimate + twtp_3_2$Estimate,
  SE = sqrt(twtp_5_4$SE^2 + twtp_4_3$SE^2 + twtp_3_2$SE^2)   # sum of variances
)

# Compute 95% CI
stepwise_wtp <- stepwise_wtp %>%
  mutate(
    `2.5 %` = Estimate - 1.96 * SE,
    `97.5 %` = Estimate + 1.96 * SE
  )

df2 <- rbind(twtp_5_2,stepwise_wtp)%>%
  mutate(delta = "3 Units")%>%
  mutate(group = "two")




# Stepwise WTP: 5->4 + 4->3 + 3->2 + 2->1
stepwise_wtp <- data.frame(
  Scenario = "Stepwise 5 to 1  ",
  Estimate = twtp_5_4$Estimate + twtp_4_3$Estimate + twtp_3_2$Estimate + twtp_2_1$Estimate,
  SE = sqrt(twtp_5_4$SE^2 + twtp_4_3$SE^2 + twtp_3_2$SE^2 + twtp_2_1$SE^2)   # sum of variances
)

# Compute 95% CI
stepwise_wtp <- stepwise_wtp %>%
  mutate(
    `2.5 %` = Estimate - 1.96 * SE,
    `97.5 %` = Estimate + 1.96 * SE
  )

df3 <- rbind(twtp_5_1,stepwise_wtp)%>%
  mutate(delta = "4 Units")%>%
  mutate(group = "three")


# Stepwise WTP: 4->3 + 3->2 
stepwise_wtp <- data.frame(
  Scenario = "Stepwise 4 to 2",
  Estimate = twtp_4_3$Estimate + twtp_3_2$Estimate,
  SE = sqrt(twtp_4_3$SE^2 + twtp_3_2$SE^2)   # sum of variances
)

# Compute 95% CI
stepwise_wtp <- stepwise_wtp %>%
  mutate(
    `2.5 %` = Estimate - 1.96 * SE,
    `97.5 %` = Estimate + 1.96 * SE
  )

df4 <- rbind(twtp_4_2,stepwise_wtp)%>%
  mutate(delta = "2 Units")%>%
  mutate(group = "four")


# Stepwise WTP: 4->3 + 3->2 + 2->1 
stepwise_wtp <- data.frame(
  Scenario = "Stepwise 4 to 1",
  Estimate = twtp_4_3$Estimate + twtp_3_2$Estimate + twtp_2_1$Estimate,
  SE = sqrt(twtp_4_3$SE^2 + twtp_3_2$SE^2 + twtp_2_1$SE^2)   # sum of variances
)

# Compute 95% CI
stepwise_wtp <- stepwise_wtp %>%
  mutate(
    `2.5 %` = Estimate - 1.96 * SE,
    `97.5 %` = Estimate + 1.96 * SE
  )

df5 <- rbind(twtp_4_1,stepwise_wtp)%>%
  mutate(delta = "3 Units")%>%
  mutate(group = "five")



# Stepwise WTP: 3->2 + 2->1
stepwise_wtp <- data.frame(
  Scenario = "Stepwise 3 to 1",
  Estimate = twtp_3_2$Estimate + twtp_2_1$Estimate,
  SE = sqrt(twtp_3_2$SE^2 + twtp_2_1$SE^2)   # sum of variances
)

# Compute 95% CI
stepwise_wtp <- stepwise_wtp %>%
  mutate(
    `2.5 %` = Estimate - 1.96 * SE,
    `97.5 %` = Estimate + 1.96 * SE
  )

df6 <- rbind(twtp_3_1,stepwise_wtp)%>%
  mutate(delta = "2 Units")%>%
  mutate(group = "six")


df7 <- twtp_5_4%>%
  mutate(delta = "1 Units")%>%
  mutate(group = "seven")

df8 <- twtp_4_3%>%
  mutate(delta = "1 Units")%>%
  mutate(group = "seven")

df9 <- twtp_3_2%>%
  mutate(delta = "1 Units")%>%
  mutate(group = "seven")

df10 <- twtp_2_1%>%
  mutate(delta = "1 Units")%>%
  mutate(group = "seven")

df <- rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10)



df_baseline_0 <- df %>%
  mutate(
    `Current WQ` = case_when(
      Scenario == "5 to 3" ~ 5,
      Scenario == "Stepwise 5 to 3" ~ 5,
      Scenario == "5 to 2" ~ 5,
      Scenario == "Stepwise 5 to 2 " ~ 5,
      Scenario == "5 to 1" ~ 5,
      Scenario == "Stepwise 5 to 1  " ~ 5,
      Scenario == "4 to 2" ~ 4,
      Scenario == "Stepwise 4 to 2" ~ 4,
      Scenario == "4 to 1" ~ 4,
      Scenario == "Stepwise 4 to 1" ~ 4,
      Scenario == "3 to 1" ~ 3,
      Scenario == "Stepwise 3 to 1" ~ 3,
      Scenario == "5 to 4" ~ 5,
      Scenario == "4 to 3" ~ 4,
      Scenario == "3 to 2" ~ 3,
      Scenario == "2 to 1" ~ 2,
      TRUE ~ NA_real_
    ),
    `Policy WQ` = case_when(
      Scenario == "5 to 3" ~ 3,
      Scenario == "Stepwise 5 to 3" ~ 3,
      Scenario == "5 to 2" ~ 2,
      Scenario == "Stepwise 5 to 2 " ~ 2,
      Scenario == "5 to 1" ~ 1,
      Scenario == "Stepwise 5 to 1  " ~ 1,
      Scenario == "4 to 2" ~ 2,
      Scenario == "Stepwise 4 to 2" ~ 2,
      Scenario == "4 to 1" ~ 1,
      Scenario == "Stepwise 4 to 1" ~ 1,
      Scenario == "3 to 1" ~ 1,
      Scenario == "Stepwise 3 to 1" ~ 1,
      Scenario == "5 to 4" ~ 4,
      Scenario == "4 to 3" ~ 3,
      Scenario == "3 to 2" ~ 2,
      Scenario == "2 to 1" ~ 1,
      TRUE ~ NA_real_
    ),
    Stepwise = case_when(
      Scenario == "5 to 3" ~ " ",
      Scenario == "Stepwise 5 to 3" ~ "X",
      Scenario == "5 to 2" ~ " ",
      Scenario == "Stepwise 5 to 2 " ~ "X",
      Scenario == "5 to 1" ~ " ",
      Scenario == "Stepwise 5 to 1  " ~ "X",
      Scenario == "4 to 2" ~ " ",
      Scenario == "Stepwise 4 to 2" ~ "X",
      Scenario == "4 to 1" ~ " ",
      Scenario == "Stepwise 4 to 1" ~ "X",
      Scenario == "3 to 1" ~" ",
      Scenario == "Stepwise 3 to 1" ~ "X",
      Scenario == "5 to 4" ~ " ",
      Scenario == "4 to 3" ~ " ",
      Scenario == "3 to 2" ~ " ",
      Scenario == "2 to 1" ~ " ",
      TRUE ~ NA_character_
    ))%>%
  mutate(`Status Quo` = "Best")%>%
  dplyr::select(Estimate, SE, `Current WQ`, `Policy WQ`, Stepwise, `Status Quo`,delta)



# Simplified version inspired by schart function
create_coef_plot <- function(data, 
                             coef_col = "Estimate", 
                             se_col = "SE",
                             ci_level = 0.95,
                             highlight_rows = NULL,
                             ylab = "Marginal WTP ($)") {
  
  # Calculate confidence intervals
  z_value <- qnorm(1 - (1 - ci_level) / 2)
  data$lower <- data[[coef_col]] - z_value * data[[se_col]]
  data$upper <- data[[coef_col]] + z_value * data[[se_col]]
  data$is_highlight <- 1:nrow(data) %in% highlight_rows
  
  # Top panel: coefficient plot
  p_top <- ggplot(data, aes(x = factor(1:nrow(data)), y = .data[[coef_col]])) +
    #geom_hline(yintercept = 0, linetype = "dashed", color = "maroon") +
    geom_errorbar(aes(ymin = lower, ymax = upper, color = is_highlight), 
                  width = 0.2, size = 0.5) +
    geom_point(aes(fill = is_highlight), size = 2, shape = 21, color = "maroon") +
    scale_color_manual(values = c("FALSE" = "maroon", "TRUE" = "maroon")) +
    scale_fill_manual(values = c("FALSE" = "maroon", "TRUE" = "maroon")) +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      legend.position = "none",
      panel.grid.major.x = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line.y = element_line(size = 0.2, color = "black"),
      axis.line.x = element_line(size = 0.2, color = "black"),
      plot.margin = margin(0, 5, 0, 5),  # Reduced bottom margin
      axis.title.y = element_text(margin = margin(r = 5))  # Reduced right margin for y-axis title
    ) +
    scale_y_continuous(
      limits = c(0, 250),
      breaks = seq(0, 250, by = 50)
    )+
    labs(y = ylab, x = NULL)
  
  return(p_top)
}

# Create table panel using base R only
create_table_panel <- function(data) {
  
  # Define the specific columns we want to display
  feature_cols <- c("Current WQ", "Policy WQ", "Best SQ", "Best SQ - 1","Best SQ - 2","Best SQ - 3")
  
  # Use base R to select columns
  table_subset <- data[, feature_cols, drop = FALSE]
  table_subset$row_id <- 1:nrow(table_subset)
  
  # Convert to long format using base R and tidyr
  table_data <- reshape2::melt(table_subset, id.vars = "row_id", 
                               variable.name = "feature", value.name = "value")
  
  # Convert to factors for proper ordering - use the specific order we want
  table_data$row_id <- factor(table_data$row_id)
  table_data$feature <- factor(table_data$feature, 
                               levels = rev(feature_cols))  # Use our specific column order
  
  p_bottom <- ggplot(table_data, aes(x = row_id, y = feature)) +
    geom_tile(aes(fill = as.character(value)), color = "white", size = 1) +
    geom_text(aes(label = value), size = 3.5, fontface = "bold") +
    scale_fill_manual(values = c("TRUE" = "gray80", "FALSE" = "gray80", "Y" = "gray80", "N" = "gray80", 
                                 "1" = "gray80", "2" = "gray80", "3" = "gray80", "4" = "gray80", "5" = "gray80",
                                 "0" = "gray80", "Best" ="gray80","X" ="gray80", " " ="gray80" )) +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      legend.position = "none",
      panel.grid = element_blank(),
      axis.text.y = element_text(face = "bold")
    ) +
    labs(x = NULL, y = NULL)
  
  return(p_bottom)
}


# baseline = 1 (best-1), current WQ = 5 ---> one unit improvement
twtp_5_4 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "(-(mu_b_wq_nonlocal_basin*(5-4) + b_basewq_nonlocal*5*(5-4) + b_wq_x_bl_nonlocal*1*(5-4)) / -b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(Scenario = "5 to 4") %>%
  relocate(Scenario, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))

# baseline = 1 (best-1), current WQ = 4 ---> one unit improvement
twtp_4_3 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "(-(mu_b_wq_nonlocal_basin*(4-3) + b_basewq_nonlocal*4*(4-3) + b_wq_x_bl_nonlocal*1*(4-3)) / -b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(Scenario = "4 to 3") %>%
  relocate(Scenario, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))

# baseline = 1 (best - 1), current WQ = 3 ---> one unit improvement
twtp_3_2 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "(-(mu_b_wq_nonlocal_basin*(3-2) + b_basewq_nonlocal*3*(3-2) + b_wq_x_bl_nonlocal*1*(3-2)) / -b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(Scenario = "3 to 2") %>%
  relocate(Scenario, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))


# baseline = 1 (best - 1), current WQ = 2 ---> one unit improvement
twtp_2_1 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "(-(mu_b_wq_nonlocal_basin*(2-1) + b_basewq_nonlocal*2*(2-1) + b_wq_x_bl_nonlocal*1*(2-1)) / -b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(Scenario = "2 to 1") %>%
  relocate(Scenario, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))




# baseline = 1 (best - 1), current WQ = 5 ---> 3 (two unit improvement)
twtp_5_3 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "(-(mu_b_wq_nonlocal_basin*(5-3) + b_basewq_nonlocal*5*(5-3) + b_wq_x_bl_nonlocal*1*(5-3)) / -b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(Scenario = "5 to 3") %>%
  relocate(Scenario, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))


# baseline = 1 (best - ), current WQ = 5 ---> 2 (three unit improvement)
twtp_5_2 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "(-(mu_b_wq_nonlocal_basin*(5-2) + b_basewq_nonlocal*5*(5-2) + b_wq_x_bl_nonlocal*1*(5-2)) / -b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(Scenario = "5 to 2") %>%
  relocate(Scenario, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))


# baseline = 1 (best-1), current WQ = 5 ---> 1 (four unit improvement)
twtp_5_1 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "(-(mu_b_wq_nonlocal_basin*(5-1) + b_basewq_nonlocal*5*(5-1) + b_wq_x_bl_nonlocal*1*(5-1)) / -b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(Scenario = "5 to 1") %>%
  relocate(Scenario, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))



# baseline = 1 (best-1), current WQ = 4 ---> 2 (two unit improvement)
twtp_4_2 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "(-(mu_b_wq_nonlocal_basin*(4-2) + b_basewq_nonlocal*4*(4-2) + b_wq_x_bl_nonlocal*1*(4-2)) / -b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(Scenario = "4 to 2") %>%
  relocate(Scenario, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))



# baseline = 1 (best-1), current WQ = 4 ---> 1 (three unit improvement)
twtp_4_1 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "(-(mu_b_wq_nonlocal_basin*(4-1) + b_basewq_nonlocal*4*(4-1) + b_wq_x_bl_nonlocal*1*(4-1)) / -b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(Scenario = "4 to 1") %>%
  relocate(Scenario, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))



# baseline = 1 (best-1), current WQ = 3 --->  1two unit improvement
twtp_3_1 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "(-(mu_b_wq_nonlocal_basin*(3-1) + b_basewq_nonlocal*3*(3-1) + b_wq_x_bl_nonlocal*1*(3-1)) / -b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(Scenario = "3 to 1") %>%
  relocate(Scenario, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))



# Stepwise WTP: 5->4 + 4->3
stepwise_wtp <- data.frame(
  Scenario = "Stepwise 5 to 3",
  Estimate = twtp_5_4$Estimate + twtp_4_3$Estimate,
  SE = sqrt(twtp_5_4$SE^2 + twtp_5_4$SE^2)   # sum of variances
)

# Compute 95% CI
stepwise_wtp <- stepwise_wtp %>%
  mutate(
    `2.5 %` = Estimate - 1.96 * SE,
    `97.5 %` = Estimate + 1.96 * SE
  )

df1 <- rbind(twtp_5_3,stepwise_wtp)%>%
  mutate(delta = "2 Units")%>%
  mutate(group = "one")


# Stepwise WTP: 5->4 + 4->3 + 3->2
stepwise_wtp <- data.frame(
  Scenario = "Stepwise 5 to 2 ",
  Estimate = twtp_5_4$Estimate + twtp_4_3$Estimate + twtp_3_2$Estimate,
  SE = sqrt(twtp_5_4$SE^2 + twtp_4_3$SE^2 + twtp_3_2$SE^2)   # sum of variances
)

# Compute 95% CI
stepwise_wtp <- stepwise_wtp %>%
  mutate(
    `2.5 %` = Estimate - 1.96 * SE,
    `97.5 %` = Estimate + 1.96 * SE
  )

df2 <- rbind(twtp_5_2,stepwise_wtp)%>%
  mutate(delta = "3 Units")%>%
  mutate(group = "two")




# Stepwise WTP: 5->4 + 4->3 + 3->2 + 2->1
stepwise_wtp <- data.frame(
  Scenario = "Stepwise 5 to 1  ",
  Estimate = twtp_5_4$Estimate + twtp_4_3$Estimate + twtp_3_2$Estimate + twtp_2_1$Estimate,
  SE = sqrt(twtp_5_4$SE^2 + twtp_4_3$SE^2 + twtp_3_2$SE^2 + twtp_2_1$SE^2)   # sum of variances
)

# Compute 95% CI
stepwise_wtp <- stepwise_wtp %>%
  mutate(
    `2.5 %` = Estimate - 1.96 * SE,
    `97.5 %` = Estimate + 1.96 * SE
  )

df3 <- rbind(twtp_5_1,stepwise_wtp)%>%
  mutate(delta = "4 Units")%>%
  mutate(group = "three")


# Stepwise WTP: 4->3 + 3->2 
stepwise_wtp <- data.frame(
  Scenario = "Stepwise 4 to 2",
  Estimate = twtp_4_3$Estimate + twtp_3_2$Estimate,
  SE = sqrt(twtp_4_3$SE^2 + twtp_3_2$SE^2)   # sum of variances
)

# Compute 95% CI
stepwise_wtp <- stepwise_wtp %>%
  mutate(
    `2.5 %` = Estimate - 1.96 * SE,
    `97.5 %` = Estimate + 1.96 * SE
  )

df4 <- rbind(twtp_4_2,stepwise_wtp)%>%
  mutate(delta = "2 Units")%>%
  mutate(group = "four")


# Stepwise WTP: 4->3 + 3->2 + 2->1 
stepwise_wtp <- data.frame(
  Scenario = "Stepwise 4 to 1",
  Estimate = twtp_4_3$Estimate + twtp_3_2$Estimate + twtp_2_1$Estimate,
  SE = sqrt(twtp_4_3$SE^2 + twtp_3_2$SE^2 + twtp_2_1$SE^2)   # sum of variances
)

# Compute 95% CI
stepwise_wtp <- stepwise_wtp %>%
  mutate(
    `2.5 %` = Estimate - 1.96 * SE,
    `97.5 %` = Estimate + 1.96 * SE
  )

df5 <- rbind(twtp_4_1,stepwise_wtp)%>%
  mutate(delta = "3 Units")%>%
  mutate(group = "five")



# Stepwise WTP: 3->2 + 2->1
stepwise_wtp <- data.frame(
  Scenario = "Stepwise 3 to 1",
  Estimate = twtp_3_2$Estimate + twtp_2_1$Estimate,
  SE = sqrt(twtp_3_2$SE^2 + twtp_2_1$SE^2)   # sum of variances
)

# Compute 95% CI
stepwise_wtp <- stepwise_wtp %>%
  mutate(
    `2.5 %` = Estimate - 1.96 * SE,
    `97.5 %` = Estimate + 1.96 * SE
  )

df6 <- rbind(twtp_3_1,stepwise_wtp)%>%
  mutate(delta = "2 Units")%>%
  mutate(group = "six")


df7 <- twtp_5_4%>%
  mutate(delta = "1 Units")%>%
  mutate(group = "seven")

df8 <- twtp_4_3%>%
  mutate(delta = "1 Units")%>%
  mutate(group = "seven")

df9 <- twtp_3_2%>%
  mutate(delta = "1 Units")%>%
  mutate(group = "seven")

df10 <- twtp_2_1%>%
  mutate(delta = "1 Units")%>%
  mutate(group = "seven")

df <- rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10)


df_baseline_1 <- df %>%
  mutate(
    `Current WQ` = case_when(
      Scenario == "5 to 3" ~ 5,
      Scenario == "Stepwise 5 to 3" ~ 5,
      Scenario == "5 to 2" ~ 5,
      Scenario == "Stepwise 5 to 2 " ~ 5,
      Scenario == "5 to 1" ~ 5,
      Scenario == "Stepwise 5 to 1  " ~ 5,
      Scenario == "4 to 2" ~ 4,
      Scenario == "Stepwise 4 to 2" ~ 4,
      Scenario == "4 to 1" ~ 4,
      Scenario == "Stepwise 4 to 1" ~ 4,
      Scenario == "3 to 1" ~ 3,
      Scenario == "Stepwise 3 to 1" ~ 3,
      Scenario == "5 to 4" ~ 5,
      Scenario == "4 to 3" ~ 4,
      Scenario == "3 to 2" ~ 3,
      Scenario == "2 to 1" ~ 2,
      TRUE ~ NA_real_
    ),
    `Policy WQ` = case_when(
      Scenario == "5 to 3" ~ 3,
      Scenario == "Stepwise 5 to 3" ~ 3,
      Scenario == "5 to 2" ~ 2,
      Scenario == "Stepwise 5 to 2 " ~ 2,
      Scenario == "5 to 1" ~ 1,
      Scenario == "Stepwise 5 to 1  " ~ 1,
      Scenario == "4 to 2" ~ 2,
      Scenario == "Stepwise 4 to 2" ~ 2,
      Scenario == "4 to 1" ~ 1,
      Scenario == "Stepwise 4 to 1" ~ 1,
      Scenario == "3 to 1" ~ 1,
      Scenario == "Stepwise 3 to 1" ~ 1,
      Scenario == "5 to 4" ~ 4,
      Scenario == "4 to 3" ~ 3,
      Scenario == "3 to 2" ~ 2,
      Scenario == "2 to 1" ~ 1,
      TRUE ~ NA_real_
    ),
    Stepwise = case_when(
      Scenario == "5 to 3" ~ " ",
      Scenario == "Stepwise 5 to 3" ~ "X",
      Scenario == "5 to 2" ~ " ",
      Scenario == "Stepwise 5 to 2 " ~ "X",
      Scenario == "5 to 1" ~ " ",
      Scenario == "Stepwise 5 to 1  " ~ "X",
      Scenario == "4 to 2" ~ " ",
      Scenario == "Stepwise 4 to 2" ~ "X",
      Scenario == "4 to 1" ~ " ",
      Scenario == "Stepwise 4 to 1" ~ "X",
      Scenario == "3 to 1" ~" ",
      Scenario == "Stepwise 3 to 1" ~ "X",
      Scenario == "5 to 4" ~ " ",
      Scenario == "4 to 3" ~ " ",
      Scenario == "3 to 2" ~ " ",
      Scenario == "2 to 1" ~ " ",
      TRUE ~ NA_character_
    ))%>%
  mutate(`Status Quo` = "Best-1")%>%
  dplyr::select(Estimate, SE, `Current WQ`, `Policy WQ`, Stepwise, `Status Quo`,delta)




# baseline = 2 (best-2), current WQ = 5 ---> one unit improvement
twtp_5_4 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "(-(mu_b_wq_nonlocal_basin*(5-4) + b_basewq_nonlocal*5*(5-4) + b_wq_x_bl_nonlocal*2*(5-4)) / -b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(Scenario = "5 to 4") %>%
  relocate(Scenario, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))

# baseline = 2 (best-2), current WQ = 4 ---> one unit improvement
twtp_4_3 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "(-(mu_b_wq_nonlocal_basin*(4-3) + b_basewq_nonlocal*4*(4-3) + b_wq_x_bl_nonlocal*2*(4-3)) / -b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(Scenario = "4 to 3") %>%
  relocate(Scenario, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))

# baseline = 2 (best - 2), current WQ = 3 ---> one unit improvement
twtp_3_2 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "(-(mu_b_wq_nonlocal_basin*(3-2) + b_basewq_nonlocal*3*(3-2) + b_wq_x_bl_nonlocal*2*(3-2)) / -b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(Scenario = "3 to 2") %>%
  relocate(Scenario, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))


# baseline = 2 (best - 2), current WQ = 2 ---> one unit improvement
twtp_2_1 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "(-(mu_b_wq_nonlocal_basin*(2-1) + b_basewq_nonlocal*2*(2-1) + b_wq_x_bl_nonlocal*2*(2-1)) / -b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(Scenario = "2 to 1") %>%
  relocate(Scenario, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))




# baseline = 2 (best - 2), current WQ = 5 ---> 3 (two unit improvement)
twtp_5_3 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "(-(mu_b_wq_nonlocal_basin*(5-3) + b_basewq_nonlocal*5*(5-3) + b_wq_x_bl_nonlocal*2*(5-3)) / -b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(Scenario = "5 to 3") %>%
  relocate(Scenario, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))


# baseline = 2 (best - 2), current WQ = 5 ---> 2 (three unit improvement)
twtp_5_2 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "(-(mu_b_wq_nonlocal_basin*(5-2) + b_basewq_nonlocal*5*(5-2) + b_wq_x_bl_nonlocal*2*(5-2)) / -b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(Scenario = "5 to 2") %>%
  relocate(Scenario, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))


# baseline = 2 (best-2), current WQ = 5 ---> 1 (four unit improvement)
twtp_5_1 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "(-(mu_b_wq_nonlocal_basin*(5-1) + b_basewq_nonlocal*5*(5-1) + b_wq_x_bl_nonlocal*2*(5-1)) / -b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(Scenario = "5 to 1") %>%
  relocate(Scenario, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))



# baseline = 2 (best-2), current WQ = 4 ---> 2 (two unit improvement)
twtp_4_2 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "(-(mu_b_wq_nonlocal_basin*(4-2) + b_basewq_nonlocal*4*(4-2) + b_wq_x_bl_nonlocal*2*(4-2)) / -b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(Scenario = "4 to 2") %>%
  relocate(Scenario, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))



# baseline = 2 (best-2), current WQ = 4 ---> 1 (three unit improvement)
twtp_4_1 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "(-(mu_b_wq_nonlocal_basin*(4-1) + b_basewq_nonlocal*4*(4-1) + b_wq_x_bl_nonlocal*2*(4-1)) / -b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(Scenario = "4 to 1") %>%
  relocate(Scenario, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))



# baseline = 2 (best-2), current WQ = 3 --->  1two unit improvement
twtp_3_1 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "(-(mu_b_wq_nonlocal_basin*(3-1) + b_basewq_nonlocal*3*(3-1) + b_wq_x_bl_nonlocal*2*(3-1)) / -b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(Scenario = "3 to 1") %>%
  relocate(Scenario, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))



# Stepwise WTP: 5->4 + 4->3
stepwise_wtp <- data.frame(
  Scenario = "Stepwise 5 to 3",
  Estimate = twtp_5_4$Estimate + twtp_4_3$Estimate,
  SE = sqrt(twtp_5_4$SE^2 + twtp_5_4$SE^2)   # sum of variances
)

# Compute 95% CI
stepwise_wtp <- stepwise_wtp %>%
  mutate(
    `2.5 %` = Estimate - 1.96 * SE,
    `97.5 %` = Estimate + 1.96 * SE
  )

df1 <- rbind(twtp_5_3,stepwise_wtp)%>%
  mutate(delta = "2 Units")%>%
  mutate(group = "one")


# Stepwise WTP: 5->4 + 4->3 + 3->2
stepwise_wtp <- data.frame(
  Scenario = "Stepwise 5 to 2 ",
  Estimate = twtp_5_4$Estimate + twtp_4_3$Estimate + twtp_3_2$Estimate,
  SE = sqrt(twtp_5_4$SE^2 + twtp_4_3$SE^2 + twtp_3_2$SE^2)   # sum of variances
)

# Compute 95% CI
stepwise_wtp <- stepwise_wtp %>%
  mutate(
    `2.5 %` = Estimate - 1.96 * SE,
    `97.5 %` = Estimate + 1.96 * SE
  )

df2 <- rbind(twtp_5_2,stepwise_wtp)%>%
  mutate(delta = "3 Units")%>%
  mutate(group = "two")




# Stepwise WTP: 5->4 + 4->3 + 3->2 + 2->1
stepwise_wtp <- data.frame(
  Scenario = "Stepwise 5 to 1  ",
  Estimate = twtp_5_4$Estimate + twtp_4_3$Estimate + twtp_3_2$Estimate + twtp_2_1$Estimate,
  SE = sqrt(twtp_5_4$SE^2 + twtp_4_3$SE^2 + twtp_3_2$SE^2 + twtp_2_1$SE^2)   # sum of variances
)

# Compute 95% CI
stepwise_wtp <- stepwise_wtp %>%
  mutate(
    `2.5 %` = Estimate - 1.96 * SE,
    `97.5 %` = Estimate + 1.96 * SE
  )

df3 <- rbind(twtp_5_1,stepwise_wtp)%>%
  mutate(delta = "4 Units")%>%
  mutate(group = "three")


# Stepwise WTP: 4->3 + 3->2 
stepwise_wtp <- data.frame(
  Scenario = "Stepwise 4 to 2",
  Estimate = twtp_4_3$Estimate + twtp_3_2$Estimate,
  SE = sqrt(twtp_4_3$SE^2 + twtp_3_2$SE^2)   # sum of variances
)

# Compute 95% CI
stepwise_wtp <- stepwise_wtp %>%
  mutate(
    `2.5 %` = Estimate - 1.96 * SE,
    `97.5 %` = Estimate + 1.96 * SE
  )

df4 <- rbind(twtp_4_2,stepwise_wtp)%>%
  mutate(delta = "2 Units")%>%
  mutate(group = "four")


# Stepwise WTP: 4->3 + 3->2 + 2->1 
stepwise_wtp <- data.frame(
  Scenario = "Stepwise 4 to 1",
  Estimate = twtp_4_3$Estimate + twtp_3_2$Estimate + twtp_2_1$Estimate,
  SE = sqrt(twtp_4_3$SE^2 + twtp_3_2$SE^2 + twtp_2_1$SE^2)   # sum of variances
)

# Compute 95% CI
stepwise_wtp <- stepwise_wtp %>%
  mutate(
    `2.5 %` = Estimate - 1.96 * SE,
    `97.5 %` = Estimate + 1.96 * SE
  )

df5 <- rbind(twtp_4_1,stepwise_wtp)%>%
  mutate(delta = "3 Units")%>%
  mutate(group = "five")



# Stepwise WTP: 3->2 + 2->1
stepwise_wtp <- data.frame(
  Scenario = "Stepwise 3 to 1",
  Estimate = twtp_3_2$Estimate + twtp_2_1$Estimate,
  SE = sqrt(twtp_3_2$SE^2 + twtp_2_1$SE^2)   # sum of variances
)

# Compute 95% CI
stepwise_wtp <- stepwise_wtp %>%
  mutate(
    `2.5 %` = Estimate - 1.96 * SE,
    `97.5 %` = Estimate + 1.96 * SE
  )

df6 <- rbind(twtp_3_1,stepwise_wtp)%>%
  mutate(delta = "2 Units")%>%
  mutate(group = "six")

df7 <- twtp_5_4%>%
  mutate(delta = "1 Units")%>%
  mutate(group = "seven")

df8 <- twtp_4_3%>%
  mutate(delta = "1 Units")%>%
  mutate(group = "seven")

df9 <- twtp_3_2%>%
  mutate(delta = "1 Units")%>%
  mutate(group = "seven")

df10 <- twtp_2_1%>%
  mutate(delta = "1 Units")%>%
  mutate(group = "seven")


df <- rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10)


df_baseline_2 <- df %>%
  mutate(
    `Current WQ` = case_when(
      Scenario == "5 to 3" ~ 5,
      Scenario == "Stepwise 5 to 3" ~ 5,
      Scenario == "5 to 2" ~ 5,
      Scenario == "Stepwise 5 to 2 " ~ 5,
      Scenario == "5 to 1" ~ 5,
      Scenario == "Stepwise 5 to 1  " ~ 5,
      Scenario == "4 to 2" ~ 4,
      Scenario == "Stepwise 4 to 2" ~ 4,
      Scenario == "4 to 1" ~ 4,
      Scenario == "Stepwise 4 to 1" ~ 4,
      Scenario == "3 to 1" ~ 3,
      Scenario == "Stepwise 3 to 1" ~ 3,
      Scenario == "5 to 4" ~ 5,
      Scenario == "4 to 3" ~ 4,
      Scenario == "3 to 2" ~ 3,
      Scenario == "2 to 1" ~ 2,
      TRUE ~ NA_real_
    ),
    `Policy WQ` = case_when(
      Scenario == "5 to 3" ~ 3,
      Scenario == "Stepwise 5 to 3" ~ 3,
      Scenario == "5 to 2" ~ 2,
      Scenario == "Stepwise 5 to 2 " ~ 2,
      Scenario == "5 to 1" ~ 1,
      Scenario == "Stepwise 5 to 1  " ~ 1,
      Scenario == "4 to 2" ~ 2,
      Scenario == "Stepwise 4 to 2" ~ 2,
      Scenario == "4 to 1" ~ 1,
      Scenario == "Stepwise 4 to 1" ~ 1,
      Scenario == "3 to 1" ~ 1,
      Scenario == "Stepwise 3 to 1" ~ 1,
      Scenario == "5 to 4" ~ 4,
      Scenario == "4 to 3" ~ 3,
      Scenario == "3 to 2" ~ 2,
      Scenario == "2 to 1" ~ 1,
      TRUE ~ NA_real_
    ),
    Stepwise = case_when(
      Scenario == "5 to 3" ~ " ",
      Scenario == "Stepwise 5 to 3" ~ "X",
      Scenario == "5 to 2" ~ " ",
      Scenario == "Stepwise 5 to 2 " ~ "X",
      Scenario == "5 to 1" ~ " ",
      Scenario == "Stepwise 5 to 1  " ~ "X",
      Scenario == "4 to 2" ~ " ",
      Scenario == "Stepwise 4 to 2" ~ "X",
      Scenario == "4 to 1" ~ " ",
      Scenario == "Stepwise 4 to 1" ~ "X",
      Scenario == "3 to 1" ~" ",
      Scenario == "Stepwise 3 to 1" ~ "X",
      Scenario == "5 to 4" ~ " ",
      Scenario == "4 to 3" ~ " ",
      Scenario == "3 to 2" ~ " ",
      Scenario == "2 to 1" ~ " ",
      TRUE ~ NA_character_
    ))%>%
  mutate(`Status Quo` = "Best-2")%>%
  dplyr::select(Estimate, SE, `Current WQ`, `Policy WQ`, Stepwise, `Status Quo`,delta)






# baseline = 3 (best-3), current WQ = 5 ---> one unit improvement
twtp_5_4 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "(-(mu_b_wq_nonlocal_basin*(5-4) + b_basewq_nonlocal*5*(5-4) + b_wq_x_bl_nonlocal*3*(5-4)) / -b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(Scenario = "5 to 4") %>%
  relocate(Scenario, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))

# baseline = 3 (best-3), current WQ = 4 ---> one unit improvement
twtp_4_3 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "(-(mu_b_wq_nonlocal_basin*(4-3) + b_basewq_nonlocal*4*(4-3) + b_wq_x_bl_nonlocal*3*(4-3)) / -b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(Scenario = "4 to 3") %>%
  relocate(Scenario, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))

# baseline = 3 (best - 3), current WQ = 3 ---> one unit improvement
twtp_3_2 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "(-(mu_b_wq_nonlocal_basin*(3-2) + b_basewq_nonlocal*3*(3-2) + b_wq_x_bl_nonlocal*3*(3-2)) / -b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(Scenario = "3 to 2") %>%
  relocate(Scenario, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))


# baseline = 3 (best - 3), current WQ = 2 ---> one unit improvement
twtp_2_1 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "(-(mu_b_wq_nonlocal_basin*(2-1) + b_basewq_nonlocal*2*(2-1) + b_wq_x_bl_nonlocal*3*(2-1)) / -b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(Scenario = "2 to 1") %>%
  relocate(Scenario, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))




# baseline = 3 (best - 3), current WQ = 5 ---> 3 (two unit improvement)
twtp_5_3 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "(-(mu_b_wq_nonlocal_basin*(5-3) + b_basewq_nonlocal*5*(5-3) + b_wq_x_bl_nonlocal*3*(5-3)) / -b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(Scenario = "5 to 3") %>%
  relocate(Scenario, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))


# baseline = 3 (best - 3), current WQ = 5 ---> 2 (three unit improvement)
twtp_5_2 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "(-(mu_b_wq_nonlocal_basin*(5-2) + b_basewq_nonlocal*5*(5-2) + b_wq_x_bl_nonlocal*3*(5-2)) / -b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(Scenario = "5 to 2") %>%
  relocate(Scenario, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))


# baseline = 3 (best-3), current WQ = 5 ---> 1 (four unit improvement)
twtp_5_1 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "(-(mu_b_wq_nonlocal_basin*(5-1) + b_basewq_nonlocal*5*(5-1) + b_wq_x_bl_nonlocal*3*(5-1)) / -b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(Scenario = "5 to 1") %>%
  relocate(Scenario, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))



# baseline = 3 (best-3), current WQ = 4 ---> 2 (two unit improvement)
twtp_4_2 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "(-(mu_b_wq_nonlocal_basin*(4-2) + b_basewq_nonlocal*4*(4-2) + b_wq_x_bl_nonlocal*3*(4-2)) / -b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(Scenario = "4 to 2") %>%
  relocate(Scenario, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))



# baseline = 3 (best-3), current WQ = 4 ---> 1 (three unit improvement)
twtp_4_1 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "(-(mu_b_wq_nonlocal_basin*(4-1) + b_basewq_nonlocal*4*(4-1) + b_wq_x_bl_nonlocal*3*(4-1)) / -b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(Scenario = "4 to 1") %>%
  relocate(Scenario, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))



# baseline = 3 (best-3), current WQ = 3 --->  1two unit improvement
twtp_3_1 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "(-(mu_b_wq_nonlocal_basin*(3-1) + b_basewq_nonlocal*3*(3-1) + b_wq_x_bl_nonlocal*3*(3-1)) / -b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(Scenario = "3 to 1") %>%
  relocate(Scenario, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))



# Stepwise WTP: 5->4 + 4->3
stepwise_wtp <- data.frame(
  Scenario = "Stepwise 5 to 3",
  Estimate = twtp_5_4$Estimate + twtp_4_3$Estimate,
  SE = sqrt(twtp_5_4$SE^2 + twtp_5_4$SE^2)   # sum of variances
)

# Compute 95% CI
stepwise_wtp <- stepwise_wtp %>%
  mutate(
    `2.5 %` = Estimate - 1.96 * SE,
    `97.5 %` = Estimate + 1.96 * SE
  )

df1 <- rbind(twtp_5_3,stepwise_wtp)%>%
  mutate(delta = "2 Units")%>%
  mutate(group = "one")


# Stepwise WTP: 5->4 + 4->3 + 3->2
stepwise_wtp <- data.frame(
  Scenario = "Stepwise 5 to 2 ",
  Estimate = twtp_5_4$Estimate + twtp_4_3$Estimate + twtp_3_2$Estimate,
  SE = sqrt(twtp_5_4$SE^2 + twtp_4_3$SE^2 + twtp_3_2$SE^2)   # sum of variances
)

# Compute 95% CI
stepwise_wtp <- stepwise_wtp %>%
  mutate(
    `2.5 %` = Estimate - 1.96 * SE,
    `97.5 %` = Estimate + 1.96 * SE
  )

df2 <- rbind(twtp_5_2,stepwise_wtp)%>%
  mutate(delta = "3 Units")%>%
  mutate(group = "two")




# Stepwise WTP: 5->4 + 4->3 + 3->2 + 2->1
stepwise_wtp <- data.frame(
  Scenario = "Stepwise 5 to 1  ",
  Estimate = twtp_5_4$Estimate + twtp_4_3$Estimate + twtp_3_2$Estimate + twtp_2_1$Estimate,
  SE = sqrt(twtp_5_4$SE^2 + twtp_4_3$SE^2 + twtp_3_2$SE^2 + twtp_2_1$SE^2)   # sum of variances
)

# Compute 95% CI
stepwise_wtp <- stepwise_wtp %>%
  mutate(
    `2.5 %` = Estimate - 1.96 * SE,
    `97.5 %` = Estimate + 1.96 * SE
  )

df3 <- rbind(twtp_5_1,stepwise_wtp)%>%
  mutate(delta = "4 Units")%>%
  mutate(group = "three")


# Stepwise WTP: 4->3 + 3->2 
stepwise_wtp <- data.frame(
  Scenario = "Stepwise 4 to 2",
  Estimate = twtp_4_3$Estimate + twtp_3_2$Estimate,
  SE = sqrt(twtp_4_3$SE^2 + twtp_3_2$SE^2)   # sum of variances
)

# Compute 95% CI
stepwise_wtp <- stepwise_wtp %>%
  mutate(
    `2.5 %` = Estimate - 1.96 * SE,
    `97.5 %` = Estimate + 1.96 * SE
  )

df4 <- rbind(twtp_4_2,stepwise_wtp)%>%
  mutate(delta = "2 Units")%>%
  mutate(group = "four")


# Stepwise WTP: 4->3 + 3->2 + 2->1 
stepwise_wtp <- data.frame(
  Scenario = "Stepwise 4 to 1",
  Estimate = twtp_4_3$Estimate + twtp_3_2$Estimate + twtp_2_1$Estimate,
  SE = sqrt(twtp_4_3$SE^2 + twtp_3_2$SE^2 + twtp_2_1$SE^2)   # sum of variances
)

# Compute 95% CI
stepwise_wtp <- stepwise_wtp %>%
  mutate(
    `2.5 %` = Estimate - 1.96 * SE,
    `97.5 %` = Estimate + 1.96 * SE
  )

df5 <- rbind(twtp_4_1,stepwise_wtp)%>%
  mutate(delta = "3 Units")%>%
  mutate(group = "five")



# Stepwise WTP: 3->2 + 2->1
stepwise_wtp <- data.frame(
  Scenario = "Stepwise 3 to 1",
  Estimate = twtp_3_2$Estimate + twtp_2_1$Estimate,
  SE = sqrt(twtp_3_2$SE^2 + twtp_2_1$SE^2)   # sum of variances
)

# Compute 95% CI
stepwise_wtp <- stepwise_wtp %>%
  mutate(
    `2.5 %` = Estimate - 1.96 * SE,
    `97.5 %` = Estimate + 1.96 * SE
  )

df6 <- rbind(twtp_3_1,stepwise_wtp)%>%
  mutate(delta = "2 Units")%>%
  mutate(group = "six")

df7 <- twtp_5_4%>%
  mutate(delta = "1 Units")%>%
  mutate(group = "seven")

df8 <- twtp_4_3%>%
  mutate(delta = "1 Units")%>%
  mutate(group = "seven")

df9 <- twtp_3_2%>%
  mutate(delta = "1 Units")%>%
  mutate(group = "seven")

df10 <- twtp_2_1%>%
  mutate(delta = "1 Units")%>%
  mutate(group = "seven")


df <- rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10)


df_baseline_3 <- df %>%
  mutate(
    `Current WQ` = case_when(
      Scenario == "5 to 3" ~ 5,
      Scenario == "Stepwise 5 to 3" ~ 5,
      Scenario == "5 to 2" ~ 5,
      Scenario == "Stepwise 5 to 2 " ~ 5,
      Scenario == "5 to 1" ~ 5,
      Scenario == "Stepwise 5 to 1  " ~ 5,
      Scenario == "4 to 2" ~ 4,
      Scenario == "Stepwise 4 to 2" ~ 4,
      Scenario == "4 to 1" ~ 4,
      Scenario == "Stepwise 4 to 1" ~ 4,
      Scenario == "3 to 1" ~ 3,
      Scenario == "Stepwise 3 to 1" ~ 3,
      Scenario == "5 to 4" ~ 5,
      Scenario == "4 to 3" ~ 4,
      Scenario == "3 to 2" ~ 3,
      Scenario == "2 to 1" ~ 2,
      TRUE ~ NA_real_
    ),
    `Policy WQ` = case_when(
      Scenario == "5 to 3" ~ 3,
      Scenario == "Stepwise 5 to 3" ~ 3,
      Scenario == "5 to 2" ~ 2,
      Scenario == "Stepwise 5 to 2 " ~ 2,
      Scenario == "5 to 1" ~ 1,
      Scenario == "Stepwise 5 to 1  " ~ 1,
      Scenario == "4 to 2" ~ 2,
      Scenario == "Stepwise 4 to 2" ~ 2,
      Scenario == "4 to 1" ~ 1,
      Scenario == "Stepwise 4 to 1" ~ 1,
      Scenario == "3 to 1" ~ 1,
      Scenario == "Stepwise 3 to 1" ~ 1,
      Scenario == "5 to 4" ~ 4,
      Scenario == "4 to 3" ~ 3,
      Scenario == "3 to 2" ~ 2,
      Scenario == "2 to 1" ~ 1,
      TRUE ~ NA_real_
    ),
    Stepwise = case_when(
      Scenario == "5 to 3" ~ " ",
      Scenario == "Stepwise 5 to 3" ~ "X",
      Scenario == "5 to 2" ~ " ",
      Scenario == "Stepwise 5 to 2 " ~ "X",
      Scenario == "5 to 1" ~ " ",
      Scenario == "Stepwise 5 to 1  " ~ "X",
      Scenario == "4 to 2" ~ " ",
      Scenario == "Stepwise 4 to 2" ~ "X",
      Scenario == "4 to 1" ~ " ",
      Scenario == "Stepwise 4 to 1" ~ "X",
      Scenario == "3 to 1" ~" ",
      Scenario == "Stepwise 3 to 1" ~ "X",
      Scenario == "5 to 4" ~ " ",
      Scenario == "4 to 3" ~ " ",
      Scenario == "3 to 2" ~ " ",
      Scenario == "2 to 1" ~ " ",
      TRUE ~ NA_character_
    ))%>%
  mutate(`Status Quo` = "Best-3")%>%
  dplyr::select(Estimate, SE, `Current WQ`, `Policy WQ`, Stepwise, `Status Quo`, delta)





df_all <- rbind(df_baseline_0,df_baseline_1,df_baseline_2,df_baseline_3)%>%
  mutate(`Best SQ` = case_when(
    `Status Quo` == "Best" ~ "X",
    TRUE ~ " "  # or NA_character_ if you prefer
  ))%>%
  mutate(`Best SQ - 1` = case_when(
    `Status Quo` == "Best-1" ~ "X",
    TRUE ~ " "  # or NA_character_ if you prefer
  ))%>%
  mutate(`Best SQ - 2` = case_when(
    `Status Quo` == "Best-2" ~ "X",
    TRUE ~ " "  # or NA_character_ if you prefer
  ))%>%
  mutate(`Best SQ - 3` = case_when(
    `Status Quo` == "Best-3" ~ "X",
    TRUE ~ " "  # or NA_character_ if you prefer
  ))


df_all <- df_all%>%
  filter(Stepwise == " ")



df1 <- df_all%>%
  filter(`Current WQ` == 5 & `Policy WQ` == 4 & `Status Quo` == "Best")

df2 <- df_all%>%
  filter(`Current WQ` == 5 & `Policy WQ` == 4 & `Status Quo` == "Best-1")

df3 <- df_all%>%
  filter(`Current WQ` == 5 & `Policy WQ` == 4 & `Status Quo` == "Best-2")

df4 <- df_all%>%
  filter(`Current WQ` == 5 & `Policy WQ` == 4 & `Status Quo` == "Best-3")


df5 <- df_all%>%
  filter(`Current WQ` == 4 & `Policy WQ` == 3 & `Status Quo` == "Best")

df6 <- df_all%>%
  filter(`Current WQ` == 4 & `Policy WQ` == 3 & `Status Quo` == "Best-1")

df7 <- df_all%>%
  filter(`Current WQ` == 4 & `Policy WQ` == 3 & `Status Quo` == "Best-2")

df8 <- df_all%>%
  filter(`Current WQ` == 4 & `Policy WQ` == 3 & `Status Quo` == "Best-3")


df9 <- df_all%>%
  filter(`Current WQ` == 3 & `Policy WQ` == 2 & `Status Quo` == "Best")

df10 <- df_all%>%
  filter(`Current WQ` == 3 & `Policy WQ` == 2 & `Status Quo` == "Best-1")

df11 <- df_all%>%
  filter(`Current WQ` == 3 & `Policy WQ` == 2 & `Status Quo` == "Best-2")

df12 <- df_all%>%
  filter(`Current WQ` == 3 & `Policy WQ` == 2 & `Status Quo` == "Best-3")


df13 <- df_all%>%
  filter(`Current WQ` == 2 & `Policy WQ` == 1 & `Status Quo` == "Best")

df14 <- df_all%>%
  filter(`Current WQ` == 2 & `Policy WQ` == 1 & `Status Quo` == "Best-1")

df15 <- df_all%>%
  filter(`Current WQ` == 2 & `Policy WQ` == 1 & `Status Quo` == "Best-2")

df16 <- df_all%>%
  filter(`Current WQ` == 2 & `Policy WQ` == 1 & `Status Quo` == "Best-3")



df17 <- df_all%>%
  filter(`Current WQ` == 5 & `Policy WQ` == 3 & `Status Quo` == "Best")

df18 <- df_all%>%
  filter(`Current WQ` == 5 & `Policy WQ` == 3 & `Status Quo` == "Best-1")

df19 <- df_all%>%
  filter(`Current WQ` == 5 & `Policy WQ` == 3 & `Status Quo` == "Best-2")

df20 <- df_all%>%
  filter(`Current WQ` == 5 & `Policy WQ` == 3 & `Status Quo` == "Best-3")



df21 <- df_all%>%
  filter(`Current WQ` == 4 & `Policy WQ` == 2 & `Status Quo` == "Best")

df22 <- df_all%>%
  filter(`Current WQ` == 4 & `Policy WQ` == 2 & `Status Quo` == "Best-1")

df23 <- df_all%>%
  filter(`Current WQ` == 4 & `Policy WQ` == 2 & `Status Quo` == "Best-2")

df24 <- df_all%>%
  filter(`Current WQ` == 4 & `Policy WQ` == 2 & `Status Quo` == "Best-3")



df25 <- df_all%>%
  filter(`Current WQ` == 3 & `Policy WQ` == 1 & `Status Quo` == "Best")

df26 <- df_all%>%
  filter(`Current WQ` == 3 & `Policy WQ` == 1 & `Status Quo` == "Best-1")

df27 <- df_all%>%
  filter(`Current WQ` == 3 & `Policy WQ` == 1 & `Status Quo` == "Best-2")

df28 <- df_all%>%
  filter(`Current WQ` == 3 & `Policy WQ` == 1 & `Status Quo` == "Best-3")


df29 <- df_all%>%
  filter(`Current WQ` == 5 & `Policy WQ` == 2 & `Status Quo` == "Best")

df30 <- df_all%>%
  filter(`Current WQ` == 5 & `Policy WQ` == 2 & `Status Quo` == "Best-1")

df31 <- df_all%>%
  filter(`Current WQ` == 5 & `Policy WQ` == 2 & `Status Quo` == "Best-2")

df32 <- df_all%>%
  filter(`Current WQ` == 5 & `Policy WQ` == 2 & `Status Quo` == "Best-3")


df33 <- df_all%>%
  filter(`Current WQ` == 4 & `Policy WQ` == 1 & `Status Quo` == "Best")

df34 <- df_all%>%
  filter(`Current WQ` == 4 & `Policy WQ` == 1 & `Status Quo` == "Best-1")

df35 <- df_all%>%
  filter(`Current WQ` == 4 & `Policy WQ` == 1 & `Status Quo` == "Best-2")

df36 <- df_all%>%
  filter(`Current WQ` == 4 & `Policy WQ` == 1 & `Status Quo` == "Best-3")

df37 <- df_all%>%
  filter(`Current WQ` == 5 & `Policy WQ` == 1 & `Status Quo` == "Best")

df38 <- df_all%>%
  filter(`Current WQ` == 5 & `Policy WQ` == 1 & `Status Quo` == "Best-1")

df39 <- df_all%>%
  filter(`Current WQ` == 5 & `Policy WQ` == 1 & `Status Quo` == "Best-2")

df40 <- df_all%>%
  filter(`Current WQ` == 5 & `Policy WQ` == 1 & `Status Quo` == "Best-3")






df_all <- rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12,df13,df14,df15,df16,
                df17,df18,df19,df20,df21,df22,df23,df24,df25,df26,df27,df28,
                df29,df30,df31,df32,df33,df34,df35,df36,df37,df38,df39,df40)


df_all <- rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12,df13,df14,df15,df16)





# Create the combined plot
p1 <- create_coef_plot(df_all, highlight_rows = c(3, 4))
p2 <- create_table_panel(df_all)

# Combine plots
final_plot <- p1 / p2 + plot_layout(heights = c(2, 1))
final_plot

ggsave("analysis/outputs/figures/figure_7_new.png", plot = final_plot, width = 10, height = 6, units = "in", dpi = 300)


final_plot <- p1 / p2 + 
  plot_layout(heights = c(2, 1)) &
  theme(plot.margin = margin(0.8, 0.8, 0.8, 0.8))  # Remove additional margins
final_plot




