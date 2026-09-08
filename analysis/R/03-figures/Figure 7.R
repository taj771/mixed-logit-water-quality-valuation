# Load your model
model <- apollo_loadModel("analysis/outputs/models/Model 4.4")

# Display model outputs
apollo_modelOutput(model)

# Extract coefficients and covariance matrix
coef_values <- model$estimate
vcov_matrix <- model$robvarcov

# Calculate WTP for baseline = 0 (best) - one unit improvements only
twtp_5_4 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "((-mu_b_wq_local_sub_basin*(5-4) + mu_b_asc + b_basesq_exp*5)  / -b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(Scenario = "5 to 4") %>%
  relocate(Scenario, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))

twtp_4_3 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "((-mu_b_wq_local_sub_basin*(4-3) + mu_b_asc + b_basesq_exp*4)  / -b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(Scenario = "4 to 3") %>%
  relocate(Scenario, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))

twtp_3_2 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "((-mu_b_wq_local_sub_basin*(3-2) + mu_b_asc + b_basesq_exp*3)  / -b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(Scenario = "3 to 2") %>%
  relocate(Scenario, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))

twtp_2_1 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "((-mu_b_wq_local_sub_basin*(2-1) + mu_b_asc + b_basesq_exp*2)  / -b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(Scenario = "2 to 1") %>%
  relocate(Scenario, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))

# Combine all the one-unit improvement scenarios
df_baseline_0_local_sub <- rbind(twtp_5_4, twtp_4_3, twtp_3_2, twtp_2_1) %>%
  mutate(
    `Current WQ` = case_when(
      Scenario == "5 to 4" ~ 5,
      Scenario == "4 to 3" ~ 4,
      Scenario == "3 to 2" ~ 3,
      Scenario == "2 to 1" ~ 2,
      TRUE ~ NA_real_
    ),
    `Policy WQ` = case_when(
      Scenario == "5 to 4" ~ 4,
      Scenario == "4 to 3" ~ 3,
      Scenario == "3 to 2" ~ 2,
      Scenario == "2 to 1" ~ 1,
      TRUE ~ NA_real_
    ),
    Stepwise = " ",
    `Status Quo` = "Best",
    delta = "1 Unit"
  ) %>%
  dplyr::select(Estimate, SE, `Current WQ`, `Policy WQ`, Stepwise, `Status Quo`, delta) %>%
  mutate(
    `Best SQ` = "",
    `Best SQ - 1` = " ",
    `Best SQ - 2` = " ",
    `Best SQ - 3` = " "
  )%>%
  mutate(
    `Basin:Local` = " ",
    `Basin:Non Local` = " ",
    `Sub Basin:Local` = "X",
    `Sub Basin:Non local` = " "
  )




# Calculate WTP for baseline = 0 (best) - one unit improvements only
twtp_5_4 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "((-mu_b_wq_local_basin*(5-4) + mu_b_asc + b_basesq_exp*5)  / -b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(Scenario = "5 to 4") %>%
  relocate(Scenario, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))

twtp_4_3 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "((-mu_b_wq_local_basin*(4-3) + mu_b_asc + b_basesq_exp*4)  / -b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(Scenario = "4 to 3") %>%
  relocate(Scenario, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))

twtp_3_2 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "((-mu_b_wq_local_basin*(3-2) + mu_b_asc + b_basesq_exp*3)  / -b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(Scenario = "3 to 2") %>%
  relocate(Scenario, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))

twtp_2_1 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "((-mu_b_wq_local_basin*(2-1) + mu_b_asc + b_basesq_exp*2)  / -b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(Scenario = "2 to 1") %>%
  relocate(Scenario, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))

# Combine all the one-unit improvement scenarios
df_baseline_0_local_basin <- rbind(twtp_5_4, twtp_4_3, twtp_3_2, twtp_2_1) %>%
  mutate(
    `Current WQ` = case_when(
      Scenario == "5 to 4" ~ 5,
      Scenario == "4 to 3" ~ 4,
      Scenario == "3 to 2" ~ 3,
      Scenario == "2 to 1" ~ 2,
      TRUE ~ NA_real_
    ),
    `Policy WQ` = case_when(
      Scenario == "5 to 4" ~ 4,
      Scenario == "4 to 3" ~ 3,
      Scenario == "3 to 2" ~ 2,
      Scenario == "2 to 1" ~ 1,
      TRUE ~ NA_real_
    ),
    Stepwise = " ",
    `Status Quo` = "Best",
    delta = "1 Unit"
  ) %>%
  dplyr::select(Estimate, SE, `Current WQ`, `Policy WQ`, Stepwise, `Status Quo`, delta) %>%
  mutate(
    `Best SQ` = "X",
    `Best SQ - 1` = " ",
    `Best SQ - 2` = " ",
    `Best SQ - 3` = " "
  )%>%
  mutate(
    `Basin:Local` = "X",
    `Basin:Non Local` = " ",
    `Sub Basin:Local` = " ",
    `Sub Basin:Non local` = " "
  )


# Calculate WTP for baseline = 0 (best) - one unit improvements only
twtp_5_4 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "((-mu_b_wq_nonlocal_basin*(5-4) + mu_b_asc + b_basesq_exp*5)  / -b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(Scenario = "5 to 4") %>%
  relocate(Scenario, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))

twtp_4_3 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "((-mu_b_wq_nonlocal_basin*(4-3) + mu_b_asc + b_basesq_exp*4)  / -b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(Scenario = "4 to 3") %>%
  relocate(Scenario, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))

twtp_3_2 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "((-mu_b_wq_nonlocal_basin*(3-2) + mu_b_asc + b_basesq_exp*3)  / -b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(Scenario = "3 to 2") %>%
  relocate(Scenario, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))

twtp_2_1 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "((-mu_b_wq_nonlocal_basin*(2-1) + mu_b_asc + b_basesq_exp*2)  / -b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(Scenario = "2 to 1") %>%
  relocate(Scenario, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))

# Combine all the one-unit improvement scenarios
df_baseline_0_nonlocal_basin <- rbind(twtp_5_4, twtp_4_3, twtp_3_2, twtp_2_1) %>%
  mutate(
    `Current WQ` = case_when(
      Scenario == "5 to 4" ~ 5,
      Scenario == "4 to 3" ~ 4,
      Scenario == "3 to 2" ~ 3,
      Scenario == "2 to 1" ~ 2,
      TRUE ~ NA_real_
    ),
    `Policy WQ` = case_when(
      Scenario == "5 to 4" ~ 4,
      Scenario == "4 to 3" ~ 3,
      Scenario == "3 to 2" ~ 2,
      Scenario == "2 to 1" ~ 1,
      TRUE ~ NA_real_
    ),
    Stepwise = " ",
    `Status Quo` = "Best",
    delta = "1 Unit"
  ) %>%
  dplyr::select(Estimate, SE, `Current WQ`, `Policy WQ`, Stepwise, `Status Quo`, delta) %>%
  mutate(
    `Best SQ` = "X",
    `Best SQ - 1` = " ",
    `Best SQ - 2` = " ",
    `Best SQ - 3` = " "
  )%>%
  mutate(
    `Basin:Local` = " ",
    `Basin:Non Local` = "X",
    `Sub Basin:Local` = " ",
    `Sub Basin:Non local` = " "
  )




# Calculate WTP for baseline = 0 (best) - one unit improvements only
twtp_5_4 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "((-mu_b_wq_nonlocal_sub_basin*(5-4) + mu_b_asc + b_basesq_exp*5)  / -b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(Scenario = "5 to 4") %>%
  relocate(Scenario, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))

twtp_4_3 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "((-mu_b_wq_nonlocal_sub_basin*(4-3) + mu_b_asc + b_basesq_exp*4)  / -b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(Scenario = "4 to 3") %>%
  relocate(Scenario, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))

twtp_3_2 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "((-mu_b_wq_nonlocal_sub_basin*(3-2) + mu_b_asc + b_basesq_exp*3)  / -b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(Scenario = "3 to 2") %>%
  relocate(Scenario, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))

twtp_2_1 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "((-mu_b_wq_nonlocal_sub_basin*(2-1) + mu_b_asc + b_basesq_exp*2)  / -b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(Scenario = "2 to 1") %>%
  relocate(Scenario, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))

# Combine all the one-unit improvement scenarios
df_baseline_0_nonlocal_subbasin <- rbind(twtp_5_4, twtp_4_3, twtp_3_2, twtp_2_1) %>%
  mutate(
    `Current WQ` = case_when(
      Scenario == "5 to 4" ~ 5,
      Scenario == "4 to 3" ~ 4,
      Scenario == "3 to 2" ~ 3,
      Scenario == "2 to 1" ~ 2,
      TRUE ~ NA_real_
    ),
    `Policy WQ` = case_when(
      Scenario == "5 to 4" ~ 4,
      Scenario == "4 to 3" ~ 3,
      Scenario == "3 to 2" ~ 2,
      Scenario == "2 to 1" ~ 1,
      TRUE ~ NA_real_
    ),
    Stepwise = " ",
    `Status Quo` = "Best",
    delta = "1 Unit"
  ) %>%
  dplyr::select(Estimate, SE, `Current WQ`, `Policy WQ`, Stepwise, `Status Quo`, delta) %>%
  mutate(
    `Best SQ` = "X",
    `Best SQ - 1` = " ",
    `Best SQ - 2` = " ",
    `Best SQ - 3` = " "
  )%>%
  mutate(
    `Basin:Local` = " ",
    `Basin:Non Local` = " ",
    `Sub Basin:Local` = " ",
    `Sub Basin:Non local` = "X"
  )





df_baseline_0 <- rbind(df_baseline_0_local_basin,df_baseline_0_nonlocal_basin,df_baseline_0_local_sub,df_baseline_0_nonlocal_subbasin)


# df_baseline_0 <- df_baseline_0_local_basin previous plot plot based on local basin but in Model 3 the refernce categpty is sub basin = 18 
# so it should be local sub basin = 18 's value 

df_baseline_0 <- df_baseline_0%>%
  filter(`Sub Basin:Local` == "X")


create_coef_plot <- function(data, 
                             coef_col = "Estimate", 
                             se_col = "SE",
                             ci_level = 0.95,
                             highlight_rows = NULL,
                             ylab = "Total WTP ($)",
                             axis_text_size = 12,
                             axis_title_size = 14,
                             y_axis_text_size = NULL,
                             y_axis_title_size = NULL) {
  
  if(is.null(y_axis_text_size)) y_axis_text_size <- axis_text_size
  if(is.null(y_axis_title_size)) y_axis_title_size <- axis_title_size
  
  data$is_highlight <- 1:nrow(data) %in% highlight_rows
  
  p_top <- ggplot(data, aes(x = factor(1:nrow(data)), y = .data[[coef_col]])) +
    geom_errorbar(aes(ymin = Estimate - 1.96 * SE, ymax = Estimate + 1.96 * SE, 
                      color = is_highlight), 
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
      plot.margin = margin(0, 5, 2, 5),  # Small bottom margin
      axis.title.y = element_text(
        size = y_axis_title_size,
        margin = margin(r = 5)
      ),
      axis.text.y = element_text(size = y_axis_text_size),
      axis.title.x = element_text(size = axis_title_size)
    ) +
    scale_y_continuous(
      limits = c(300, 750),
      breaks = seq(300, 750, by = 50)
    ) +
    labs(y = ylab, x = NULL)
  
  return(p_top)
}

create_table_panel <- function(data, 
                               table_text_size = 1.5,
                               axis_text_size = 12) {
  
  feature_cols <- c("Current WQ", "Policy WQ")
  
  table_subset <- data[, feature_cols, drop = FALSE]
  table_subset$row_id <- 1:nrow(table_subset)
  
  table_data <- reshape2::melt(table_subset, id.vars = "row_id", 
                               variable.name = "feature", value.name = "value")
  
  table_data$row_id <- factor(table_data$row_id)
  table_data$feature <- factor(table_data$feature, 
                               levels = rev(feature_cols))
  
  p_bottom <- ggplot(table_data, aes(x = row_id, y = feature)) +
    geom_tile(aes(fill = as.character(value)), color = "white", size = 0.2,
              height = 0.8,    # Balanced height to keep rows together
              width = 0.8) +
    geom_text(aes(label = value), size = table_text_size, fontface = "bold") +
    scale_fill_manual(values = c("TRUE" = "gray80", "FALSE" = "gray80", "Y" = "gray80", "N" = "gray80", 
                                 "1" = "gray95", "2" = "gray95", "3" = "gray95", "4" = "gray95", "5" = "gray95",
                                 "0" = "gray80", "Best" = "gray80", "X" = "gray80", " " = "gray80","X" = "gray80", " " = "gray80")) +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      legend.position = "none",
      panel.grid = element_blank(),
      axis.text.y = element_text(face = "bold", size = axis_text_size,
                                 margin = margin(0, 0, 0, 0)),
      plot.margin = margin(2, 0, 0, 0),  # Small top margin only
      panel.spacing = unit(0, "pt"),
      axis.ticks.length = unit(0, "pt"),
      axis.title = element_blank(),
      # Remove extra spacing
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks = element_blank()
    ) +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0.1, 0.1)) +  # Small expansion to keep rows visible
    labs(x = NULL, y = NULL)
  
  return(p_bottom)
}

# Create the plots
p1 <- create_coef_plot(df_baseline_0, 
                       axis_text_size = 12,
                       axis_title_size = 12,
                       y_axis_text_size = 11,
                       y_axis_title_size = 12,
                       ylab = "Total WTP ($)")

p2 <- create_table_panel(df_baseline_0,
                         table_text_size = 4,
                         axis_text_size = 10)



# For ultra-close spacing, use negative margins
p1 <- p1 + theme(plot.margin = margin(0, 5, -3, 5))  # Negative bottom margin
p2 <- p2 + theme(plot.margin = margin(-3, -2, -3, -2))  # Negative top margin

combined_plot <- p1 / p2 + 
  plot_layout(heights = c(9, 1))

combined_plot




# Save the plot
ggsave("analysis/outputs/figures/figure_7_new.png", plot =combined_plot , width = 7, height = 6, units = "in", dpi = 300)