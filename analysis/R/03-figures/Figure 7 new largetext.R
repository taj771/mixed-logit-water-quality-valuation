# Large-text version of Figure 7 for presentations.
# Sources Figure 7.R (which loads Model 4.4 and prepares all data),
# then calls the plotting functions with larger text sizes.

library(tidyverse)
library(apollo)
library(car)
library(patchwork)
library(reshape2)
library(ggplot2)

# Override apollo_loadModel to strip the leading "/" that the original script uses,
# which resolves to C:\Finaloutput\ on Windows instead of the project-relative path.
apollo_loadModel <- function(modelName, ...) {
  modelName <- sub("^/", "", modelName)
  apollo::apollo_loadModel(modelName, ...)
}

source("Codes_FinalData_Working/Figure 7.R")


# --- Override create_coef_plot to double point and errorbar sizes ---

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
                  width = 0.4, linewidth = 1.0) +
    geom_point(aes(fill = is_highlight), size = 4, shape = 21, color = "maroon") +
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
      axis.line.y = element_line(linewidth = 0.2, color = "black"),
      axis.line.x = element_line(linewidth = 0.2, color = "black"),
      plot.margin = margin(0, 5, 2, 5),
      axis.title.y = element_text(size = y_axis_title_size, margin = margin(r = 5)),
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


# --- Recreate plots with larger text and point sizes ---

p1 <- create_coef_plot(df_baseline_0,
                       axis_text_size = 40,
                       axis_title_size = 40,
                       y_axis_text_size = 40,
                       y_axis_title_size = 40,
                       ylab = "Total WTP ($)")

p2 <- create_table_panel(df_baseline_0,
                         table_text_size = 12,
                         axis_text_size = 36)

p1 <- p1 + theme(plot.margin = margin(0, 5, -3, 5))
p2 <- p2 + theme(plot.margin = margin(-3, -2, -3, -2))

combined_plot <- p1 / p2 +
  plot_layout(heights = c(9, 1))

ggsave("analysis/outputs/figures/figure_7_new_largetext.png", plot = combined_plot, width = 14, height = 12, units = "in", dpi = 300)
