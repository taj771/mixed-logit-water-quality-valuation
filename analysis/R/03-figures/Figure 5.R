########################################################################################
# Figure 5: Marginal willingness to pay for improvement in water quality
#######################################################################################


### Clear memory
rm(list = ls())

#load model
model <- apollo_loadModel("analysis/outputs/models/Model 2")  # Replace "mymodel" with your actual modelName



# Display model outputs
apollo_modelOutput(model)

# Extract coefficients and covariance matrix
coef_values <- model$estimate
vcov_matrix <- model$robvarcov


# WTP for one unit improvement of WQ at Local Basin

df1 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "-(mu_b_wq_local_basin*(-1))/b_cost"
)%>% 
  {`rownames<-`(., NULL)}%>%
  mutate(`WQ change scenario` = "Local Basin")%>%
  relocate(`WQ change scenario`, .before = 1)%>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))


# WTP for one unit improvement of WQ at Non-Local Basin

df2 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "-(mu_b_wq_nonlocal_basin*(-1))/b_cost"
)%>% 
  {`rownames<-`(., NULL)}%>%
  mutate(`WQ change scenario` = "Non-Local Basin")%>%
  relocate(`WQ change scenario`, .before = 1)%>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))



# WTP for one unit improvement of WQ at Local subbasin

df3 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "-(mu_b_wq_local_sub_basin*(-1))/b_cost"
)%>% 
  {`rownames<-`(., NULL)}%>%
  mutate(`WQ change scenario` = "Local Sub Basin")%>%
  relocate(`WQ change scenario`, .before = 1)%>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))



# WTP for one unit improvement of WQ at non-local subbasin

df4 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "-(mu_b_wq_nonlocal_sub_basin*(-1))/b_cost"
)%>% 
  {`rownames<-`(., NULL)}%>%
  mutate(`WQ change scenario` = "Non-Local Sub Basin")%>%
  relocate(`WQ change scenario`, .before = 1)%>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))


df_all <- rbind(df1,df2,df3,df4)

# Load library
library(xtable)

# Convert to LaTeX
latex_table <- xtable(df_all)

# Save to .tex file
print(latex_table, file = "analysis/outputs/tables/WTP_marginal.tex", include.rownames = FALSE)


df_all$`WQ change scenario` <- factor(
  df_all$`WQ change scenario`,
  levels = c("Local Basin", "Non-Local Basin", "Local Sub Basin",  "Non-Local Sub Basin")  # desired order
)

#pd <- position_dodge(width = 0.4)  # smaller = less distance


p <- ggplot(df_all, aes(x = `WQ change scenario`, y = Estimate, color = `WQ change scenario`)) +
  # Shaded background by param index (works with coord_flip)
  scale_fill_identity() +
  
  # Points and error bars
  geom_point(position = position_dodge(width = 0.1), size = 3) +
  geom_errorbar(
    aes(ymin = Estimate - SE, ymax = Estimate + SE),
    position = position_dodge(width = 0.1), width = 0.1
  ) +
  # Axis formatting
  #scale_x_discrete(expand = expansion(mult = 0, add = 0.1)) +
  #coord_flip() +
  theme_minimal() +
  labs(x = "", y = "Marginal WTP", title = "", color = "WQ change scenario") +
  theme(
    legend.position = "none",
    #legend.text = element_text(size = 14),
    axis.text.x = element_text(hjust = 1, size = 16, angle = 0),
    axis.text.y = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.y = element_line(size = 0.2, color = "black"),
    axis.line.x = element_line(size = 0.2, color = "black")
  )+
  coord_flip()+
  scale_color_manual(
    values = c(
      "Local Basin" = "#1b9e77",
      "Non-Local Basin" = "#d95f02",
      "Local Sub Basin"  = "#7570b3",
      "Non-Local Sub Basin" = "darkred"
      #"Filter 4" = "darkblue",
      #"Filter 5" = "yellow3"
      
    ),
    #labels = c(
    #  "Local Basin" = "Local Basin",
    #  "Non-Local Basin" = "Non-Local Basin",
    #  "Local Sub Basin"  = "Local Sub Basin",
    #  "Non-Local Sub Basin" = "Non-Local Sub Basin"
    #),
    name = "",
    guide = guide_legend(nrow = 2)
  ) +
  scale_y_continuous(
    limits = c(60, 275),
    breaks = seq(70, 275, by = 25)
  )

# Save to PNG
ggsave("analysis/outputs/figures/marginal_WTP.png", plot = p, width = 10, height = 8, units = "in", dpi = 300)

