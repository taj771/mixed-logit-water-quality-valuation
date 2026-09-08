################################################################################
# Survey Data Preparation - Part 4: SURVEY WEIGHTING
#
# Works out how much each respondent counts, so the sample represents the adult
# population of AB / MB / SK rather than just whoever answered. Bins demographics,
# pulls 2021 census targets from Statistics Canada, then rakes (anesrake) per
# province on age, gender, income and education.
# final_weight = province_weight * demo_weight, normalised to mean 1.
#
# IN   data/raw/Water_Quality_Final.xlsx + 5 live StatCan downloads
# OUT  data/derived/test/final_weights.csv         one weight per respondent
#      analysis/outputs/figures/diagnostics/*.png  15 sample-vs-population checks
#      Part 5 joins these on as WEIGHT - every Apollo model estimates with them.
#
# WARNING - does not reproduce exactly. get_cansim() downloads the targets live
# and caches nothing, so this needs internet and the targets shift when StatCan
# revises them. Re-running on 2026-09-06 changed 2,425 of 3,851 weights vs the
# originals (2025-10-15). Regenerating here, then running Part 5, shifts every
# model result away from the manuscript's numbers.
#
# Also note the weights are very uneven: effective sample size 1,092 of 3,851.
################################################################################
### Clear memory
rm(list = ls())

# 1. Load and prepare survey data
df <- read_excel("data/raw/Water_Quality_Final.xlsx")%>%
  rename(CaseId = `{Case.ID}`)%>%
  filter(is.na(SURVEY_CONTENT))%>%
  group_by(CaseId) %>%
  slice_max(order_by = `{Duration.of.connection.in.seconds}`, n = 1) %>%
  ungroup()%>%
  select(CaseId, PROVINCE, AGE, GENDER, EDUCATION, INCOME) %>%
  mutate(
    income_group = case_when(
      INCOME %in% c(0, 1) ~ "Under $19,999",
      INCOME %in% c(2, 3) ~ "$20,000 to $39,999",
      INCOME %in% c(4, 5) ~ "$40,000 to $59,999",
      INCOME %in% c(6, 7) ~ "$60,000 to $79,999", 
      INCOME %in% c(8, 9) ~ "$80,000 to $99,999",
      INCOME %in% c(10, 11) ~ "$100,000 to $149,999",
      INCOME == 12 ~ "$150,000 and over",
      TRUE ~ "Missing_Income"
    ),
    education_group = case_when(
      EDUCATION %in% c(1) ~ "High school or less",
      EDUCATION %in% c(2) ~ "Some college or technical training (no diploma)",
      EDUCATION %in% c(3) ~ "College diploma or technical/trade certificate",
      EDUCATION %in% c(4) ~ "Bachelor's degree",
      EDUCATION %in% c(5) ~ "Advanced/professional degree",
      TRUE ~ "Missing_Education"
    ),
    gender_group = case_when(
      GENDER %in% c(1) ~ "Man",
      GENDER %in% c(2) ~ "Woman",
      # The 2021 census publishes only Men+/Women+, so there is no population
      # benchmark to rake gender-diverse respondents against. They join the
      # Missing_Gender cell (target 0.001) rather than being forced onto a
      # margin that does not exist. 22 respondents, plus 27 with no answer.
      TRUE ~ "Missing_Gender"
    ),
    age_group = case_when(
      AGE %in% c(1, 2) ~ "18 to 29 years",
      AGE %in% c(3, 4) ~ "30 to 39 years",
      AGE %in% c(5, 6) ~ "40 to 49 years",
      AGE %in% c(7, 8) ~ "50 to 59 years",
      AGE %in% c(9, 10) ~ "60 to 69 years",
      AGE %in% c(11, 12, 13, 14) ~ "70 years and older",
      TRUE ~ "Missing_Age"
    ),
    province_group = case_when(
      PROVINCE == 1 ~ "ab",
      PROVINCE == 3 ~ "mb",
      PROVINCE == 12 ~ "sk",
      TRUE ~ "Missing_Province"
    )
  ) %>%
  filter(province_group %in% c("ab", "mb", "sk")) %>%
  select(-PROVINCE, -AGE, -GENDER, -EDUCATION, -INCOME)

# 2. Get provincial population weights ------------------------------------------
df_province_pop <- get_cansim("17-10-0005-01") %>%
  clean_names() %>%
  filter(ref_date == "2021",
         gender == "Total - gender",
         age_group == "18 years and older",
         geo %in% c("Alberta", "Manitoba", "Saskatchewan")) %>%
  mutate(province_code = case_when(
    geo == "Alberta" ~ "ab",
    geo == "Manitoba" ~ "mb",
    geo == "Saskatchewan" ~ "sk"
  )) %>%
  group_by(province_code) %>%
  summarise(population = sum(value)) %>%
  mutate(census_share = population / sum(population))

# The province factor must be census_share / sample_share, NOT census_share alone.
# A weight has to account for how many respondents you actually HAVE from each
# province, not just how large the province is. Using the census share by itself
# multiplied every Alberta respondent by 0.641 and every Saskatchewan respondent
# by 0.165 - a 3.9x ratio where the correct ratio is 1.4x - which pushed Alberta
# to 82.6% of the weighted sample against a true 64.1%, and roughly halved MB
# and SK. That biases exactly the local vs non-local comparison this chapter
# rests on. Correct factors are near 1, as they should be for a sample whose
# provincial composition is already close to the population.
df_sample_share <- df %>%
  count(province_group, name = "n_sample") %>%
  mutate(sample_share = n_sample / sum(n_sample))

df_province_pop <- df_province_pop %>%
  left_join(df_sample_share, by = c("province_code" = "province_group")) %>%
  mutate(province_weight = census_share / sample_share)

message("Province factors (census_share / sample_share):")
print(as.data.frame(df_province_pop[, c("province_code", "census_share",
                                        "sample_share", "province_weight")]),
      digits = 3)

df <- df %>% 
  left_join(df_province_pop, by = c("province_group" = "province_code"))

# 3. Complete prepare_targets() function with proper numeric handling ----------
prepare_targets <- function(province_name) {
  # Income target
  df_income <- get_cansim("11-10-0237-01") %>%
    clean_names() %>%
    filter(geo == province_name, 
           ref_date == "2021", 
           income_concept == "After-tax income",
           economic_family_type == "Economic families and persons not in an economic family") %>%
    select(statistics, value) %>%
    mutate(income_group = case_when(
      statistics %in% c("Percentage under $10,000 (including zeros and losses)", "$10,000 to $19,999") ~ "Under $19,999",
      statistics %in% c("$20,000 to $29,999", "$30,000 to $39,999") ~ "$20,000 to $39,999",
      statistics %in% c("$40,000 to $49,999", "$50,000 to $59,999") ~ "$40,000 to $59,999",
      statistics %in% c("$60,000 to $69,999", "$70,000 to $79,999") ~ "$60,000 to $79,999",
      statistics %in% c("$80,000 to $89,999", "$90,000 to $99,999") ~ "$80,000 to $99,999",
      statistics == "$100,000 to 149,999" ~ "$100,000 to $149,999",
      statistics == "$150,000 and over" ~ "$150,000 and over",
      TRUE ~ NA_character_
    )) %>%
    group_by(income_group) %>%
    summarise(percentage = sum(as.numeric(value), na.rm = TRUE)) %>%  # Ensure numeric
    filter(!is.na(income_group)) %>%
    mutate(percentage = percentage / sum(percentage)) %>%
    add_row(income_group = "Missing_Income", percentage = 0.001) %>%
    mutate(percentage = ifelse(income_group == "Missing_Income", 
                               0.001, 
                               percentage * (1-0.001)))  # Adjust to sum to 1
  
  # Education target
  df_education <- get_cansim("98-10-0384-01") %>%
    clean_names() %>%
    filter(geo == province_name, 
           ref_date == "2021", 
           census_year_4 == "2021", 
           gender_3a == "Total - Gender",
           statistics_2a == "Count") %>%
    select(age_group = age_15a, count = value, 
           education_census = highest_certificate_diploma_or_degree_15) %>%
    filter(
      age_group %in% c("20 to 24 years", "25 to 64 years", "65 years and over"),
      !education_census %in% c(
        "Total - Highest certificate, diploma or degree",
        "Non-apprenticeship trades certificate or diploma",
        "Apprenticeship certificate",
        "Bachelor's degree or higher",
        "Postsecondary certificate, diploma or degree"
      )
    ) %>%
    mutate(
      education_group = case_when(
        education_census %in% c("No certificate, diploma or degree", "High (secondary) school diploma or equivalency certificate") ~ "High school or less",
        education_census == "University certificate or diploma below bachelor level" ~ "Some college or technical training (no diploma)",
        education_census %in% c("College, CEGEP or other non-university certificate or diploma", "Apprenticeship or trades certificate or diploma") ~ "College diploma or technical/trade certificate",
        education_census == "Bachelor's degree" ~ "Bachelor's degree",
        education_census %in% c("University certificate or diploma above bachelor level", "Degree in medicine, dentistry, veterinary medicine or optometry", "Master's degree", "Earned doctorate") ~ "Advanced/professional degree",
        TRUE ~ NA_character_
      )
    ) %>%
    group_by(education_group) %>%
    summarise(count = sum(as.numeric(count), na.rm = TRUE)) %>%  # Ensure numeric
    filter(!is.na(education_group)) %>%
    mutate(percentage = count / sum(count)) %>%
    add_row(education_group = "Missing_Education", percentage = 0.001) %>%
    mutate(percentage = ifelse(education_group == "Missing_Education",
                               0.001,
                               percentage * (1-0.001)))
  
  # Gender target
  df_gender <- get_cansim("17-10-0005-01") %>%
    clean_names() %>%
    filter(geo == province_name, 
           ref_date == "2021", 
           age_group == "18 years and older",
           # WHITELIST. Was `gender != "Both sexes"`, but this table's total row is
           # spelled "Total - gender" (see the age target below), so it survived the
           # filter, fell through TRUE ~ "Other", and - being Men+ plus Women+ by
           # definition - made the Other target exactly 0.50. The raking was then
           # told half the population is Other while only 22 of 3,851 respondents
           # are, which is why weights collapsed to ~1e-154.
           gender %in% c("Men+", "Women+")) %>%
    mutate(gender_group = case_when(
      gender == "Men+" ~ "Man",
      gender == "Women+" ~ "Woman"
      # no TRUE ~ catch-all: an unexpected label must surface as NA, not be
      # silently absorbed into a real category.
    )) %>%
    group_by(gender_group) %>%
    summarise(percentage = sum(as.numeric(value), na.rm = TRUE)) %>%  # Ensure numeric
    mutate(percentage = percentage / sum(percentage)) %>%
    add_row(gender_group = "Missing_Gender", percentage = 0.001) %>%
    mutate(percentage = ifelse(gender_group == "Missing_Gender",
                               0.001,
                               percentage * (1-0.001)))
  
  # Age target
  df_age <- get_cansim("17-10-0005-01") %>%
    clean_names() %>%
    filter(geo == province_name, 
           ref_date == "2021", 
           gender == "Total - gender") %>%
    mutate(age_group = case_when(
      age_group %in% c("18 to 24 years", "25 to 29 years") ~ "18 to 29 years",
      age_group %in% c("30 to 34 years", "35 to 39 years") ~ "30 to 39 years",
      age_group %in% c("40 to 44 years", "45 to 49 years") ~ "40 to 49 years",
      age_group %in% c("50 to 54 years", "55 to 59 years") ~ "50 to 59 years",
      age_group %in% c("60 to 64 years", "65 to 69 years") ~ "60 to 69 years",
      age_group %in% c("70 to 74 years", "75 to 79 years", "80 to 84 years",
                       "85 to 89 years", "90 years and older") ~ "70 years and older",
      TRUE ~ NA_character_
    )) %>%
    group_by(age_group) %>%
    summarise(percentage = sum(as.numeric(value), na.rm = TRUE)) %>%  # Ensure numeric
    filter(!is.na(age_group)) %>%
    mutate(percentage = percentage / sum(percentage)) %>%
    add_row(age_group = "Missing_Age", percentage = 0.001) %>%
    mutate(percentage = ifelse(age_group == "Missing_Age",
                               0.001,
                               percentage * (1-0.001)))
  
  return(list(
    age_group = setNames(as.numeric(df_age$percentage), as.character(df_age$age_group)),
    gender_group = setNames(as.numeric(df_gender$percentage), as.character(df_gender$gender_group)),
    income_group = setNames(as.numeric(df_income$percentage), as.character(df_income$income_group)),
    education_group = setNames(as.numeric(df_education$percentage), as.character(df_education$education_group))
  ))
}

# 4. Raking with proper data handling ------------------------------------------
province_names <- c("ab" = "Alberta", "mb" = "Manitoba", "sk" = "Saskatchewan")
final_weights <- data.frame()

# Census targets are cached so the weighting is reproducible and works offline.
# Without this every run re-downloads from StatCan, whose tables get revised - which
# is why regenerating the weights produced different numbers each time. Refresh
# deliberately with: options(wq.refresh_targets = TRUE)
targets_file <- "data/derived/census_targets.rds"
if (file.exists(targets_file) && !isTRUE(getOption("wq.refresh_targets"))) {
  all_targets <- readRDS(targets_file)
  message("Census targets loaded from cache: ", targets_file)
} else {
  message("Downloading census targets from StatCan (once per province)...")
  all_targets <- lapply(province_names, prepare_targets)
  saveRDS(all_targets, targets_file)
  message("Census targets cached to: ", targets_file)
}

for (prov in names(province_names)) {
  # Prepare data
  prov_data <- df %>% 
    filter(province_group == prov) %>%
    as.data.frame()  # Convert from tibble if necessary
  
  targets <- all_targets[[prov]]
  
  # Convert to factors with EXACTLY matching levels
  prov_data <- prov_data %>%
    mutate(
      income_group = factor(income_group, levels = names(targets$income_group)),
      education_group = factor(education_group, levels = names(targets$education_group)),
      gender_group = factor(gender_group, levels = names(targets$gender_group)),
      age_group = factor(age_group, levels = names(targets$age_group)),
      CaseId = as.numeric(CaseId)  # Ensure CaseId is numeric
    )
  
  # Sanity checks. The previous version compared each factor's levels against the
  # very names they had just been built from, so it could never fail - it passed
  # happily while the gender target said 50% "Other". These can fail:
  #   - every target is a proper distribution summing to 1
  #   - no respondent fell outside the target categories (factor() -> silent NA)
  #   - no single category dominates implausibly
  for (v in c("age_group", "gender_group", "income_group", "education_group")) {
    tg <- targets[[v]]
    stopifnot(abs(sum(tg) - 1) < 1e-8)
    if (any(is.na(prov_data[[v]]))) {
      stop(sprintf("%s: %d respondents fall outside the %s target categories",
                   prov, sum(is.na(prov_data[[v]])), v))
    }
    if (max(tg) > 0.90) {
      warning(sprintf("%s: %s target is dominated by '%s' at %.3f - check the source table",
                      prov, v, names(tg)[which.max(tg)], max(tg)))
    }
  }
  
  # Run raking
  raked <- anesrake(
    inputter = targets,
    dataframe = prov_data,
    caseid = prov_data$CaseId,
    cap = 3,
    force1 = TRUE,
    maxit = 1000000,
    pctlim = 0.01,
    verbose = TRUE
  )
  
  # Report convergence and weight health. maxit = 1000000 means a failure to
  # converge is otherwise completely silent - which is how the degenerate
  # weights went unnoticed. Weights below 0.01 are respondents effectively
  # dropped from the analysis.
  if (!isTRUE(grepl("[Cc]omplete", as.character(raked$converge)[1]))) {
    warning(sprintf("%s: raking did NOT converge - %s", prov,
                    paste(as.character(raked$converge), collapse = " ")))
  }
  .w <- raked$weightvec
  message(sprintf(
    "%s: n=%d  weights %.4f-%.4f  %d below 0.01  effective N %.0f (%.1f%%)  [%s]",
    prov, length(.w), min(.w), max(.w), sum(.w < 0.01),
    1 / sum((.w / sum(.w))^2), 100 * (1 / sum((.w / sum(.w))^2)) / length(.w),
    paste(as.character(raked$converge), collapse = " ")))

  # Store results
  prov_data$demo_weight <- raked$weightvec
  prov_data$final_weight <- prov_data$province_weight * prov_data$demo_weight
  final_weights <- bind_rows(final_weights, prov_data)
}

# 5. Post-processing ----------------------------------------------------------
# Normalize weights to mean = 1
final_weights <- final_weights %>%
  mutate(final_weight = final_weight / mean(final_weight))

# Save results
write.csv(final_weights %>% select(CaseId, final_weight), 
          "data/derived/test/final_weights.csv", row.names = FALSE)

# Calculate effective sample size
n_eff <- 1 / sum((final_weights$final_weight/sum(final_weights$final_weight))^2)
cat("Effective sample size:", round(n_eff), "of", nrow(final_weights), "observations\n")

# Weight diagnostics
summary(final_weights$final_weight)
hist(final_weights$final_weight, main = "Final Weight Distribution")


# Create comparison table
province_check <- final_weights %>%
  group_by(province_group) %>%
  summarise(
    Unweighted_Count = n(),
    Unweighted_Pct = n() / nrow(final_weights),
    Weighted_Count = sum(final_weight),
    Weighted_Pct = sum(final_weight) / sum(final_weights$final_weight)
  ) %>%
  left_join(df_province_pop, by = c("province_group" = "province_code")) %>%
  mutate(
    Census_Pct = population/sum(population),
    Diff_From_Census = Weighted_Pct - Census_Pct
  )

# Print results
print(province_check)


# Visualize
library(ggplot2)
ggplot(province_check) +
  geom_col(aes(x = province_group, y = Census_Pct, fill = "Census"), alpha = 0.5) +
  geom_col(aes(x = province_group, y = Weighted_Pct, fill = "Weighted"), alpha = 0.5, position = "dodge") +
  labs(title = "Provincial Representation: Census vs Weighted Survey",
       y = "Percentage", x = "Province") +
  scale_fill_manual(values = c("Census" = "blue", "Weighted" = "red"))





# Function to create demographic comparison plot for a specific province and demographic
create_demo_plot <- function(province, demo_col, demo_name,
                             province_targets = all_targets[[province]]) {
  # Targets are passed in per province. Previously this read `targets` from the
  # global environment, which after the raking loop holds whichever province ran
  # LAST (sk) - so the ab_* and mb_* diagnostic plots were drawn against
  # Saskatchewan's census line. Diagnostics only; weights were unaffected.
  target_values <- province_targets[[demo_col]]
  
  # Prepare the data
  demo_data <- final_weights %>%
    filter(province_group == province) %>%
    select(all_of(demo_col), final_weight) %>%
    count(across(all_of(demo_col)), name = "Unweighted") %>%
    mutate(Unweighted = Unweighted / sum(Unweighted)) %>%
    left_join(
      final_weights %>%
        filter(province_group == province) %>%
        group_by(across(all_of(demo_col))) %>%
        summarise(Weighted = sum(final_weight)) %>%
        mutate(Weighted = Weighted / sum(Weighted)),
      by = demo_col
    ) %>%
    left_join(
      tibble(
        category = names(target_values),
        Census = as.numeric(target_values)
      ),
      by = setNames("category", demo_col)
    ) %>%
    pivot_longer(
      cols = c(Unweighted, Weighted, Census),
      names_to = "source",
      values_to = "percentage"
    ) %>%
    mutate(
      source = factor(source, levels = c("Unweighted", "Weighted", "Census")),
      category = factor(!!sym(demo_col), levels = names(target_values))
    ) %>%
    # Filter out "Missing" category
    filter(!str_detect(tolower(category), "missing"))
  
  # Check if there's any data left after filtering
  if (nrow(demo_data) == 0) {
    stop("No data remaining after filtering out 'Missing' categories for ", province, " - ", demo_name)
  }
  
  # Create the plot
  ggplot(demo_data, aes(x = category, y = percentage, color = source, group = source)) +
    geom_line(linewidth = 1) +
    geom_point(size = 3) +
    scale_color_manual(
      values = c("Unweighted" = "#E69F00", "Weighted" = "#56B4E9", "Census" = "#009E73"),
      labels = c("Unweighted Survey", "Weighted Survey", "Census Target")
    ) +
    labs(
      title = paste0(toupper(province), " - ", demo_name, " Distribution Comparison"),
      x = demo_name,
      y = "Percentage",
      color = ""
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 60, hjust = 1, size = 12),
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
      axis.text.y = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      axis.title.x = element_text(size = 12),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line.y = element_line(size = 0.2, color = "black"),
      axis.line.x = element_line(size = 0.2, color = "black")
    )
}




# Create all plots
plots <- list()

# Age plots
plots$ab_age <- create_demo_plot("ab", "age_group", "Age Group")
plots$mb_age <- create_demo_plot("mb", "age_group", "Age Group")
plots$sk_age <- create_demo_plot("sk", "age_group", "Age Group")

# Gender plots
plots$ab_gender <- create_demo_plot("ab", "gender_group", "Gender")
plots$mb_gender <- create_demo_plot("mb", "gender_group", "Gender")
plots$sk_gender <- create_demo_plot("sk", "gender_group", "Gender")

# Income plots
plots$ab_income <- create_demo_plot("ab", "income_group", "Income")
plots$mb_income <- create_demo_plot("mb", "income_group", "Income")
plots$sk_income <- create_demo_plot("sk", "income_group", "Income")

# Education plots
plots$ab_education <- create_demo_plot("ab", "education_group", "Education")
plots$mb_education <- create_demo_plot("mb", "education_group", "Education")
plots$sk_education <- create_demo_plot("sk", "education_group", "Education")

# Combine all plots by province
ab_combined <- (plots$ab_age + plots$ab_gender) / (plots$ab_income + plots$ab_education) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

mb_combined <- (plots$mb_age + plots$mb_gender) / (plots$mb_income + plots$mb_education) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

sk_combined <- (plots$sk_age + plots$sk_gender) / (plots$sk_income + plots$sk_education) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

# Save the plots
ggsave("analysis/outputs/figures/diagnostics/ab_demo_comparisons.png", ab_combined, width = 12, height = 8, dpi = 300)
ggsave("analysis/outputs/figures/diagnostics/mb_demo_comparisons.png", mb_combined, width = 12, height = 8, dpi = 300)
ggsave("analysis/outputs/figures/diagnostics/sk_demo_comparisons.png", sk_combined, width = 12, height = 8, dpi = 300)

# Seperate plot 

ab_age <- plots$ab_age
mb_age <- plots$mb_age
sk_age <- plots$sk_age

# Save the plots
ggsave("analysis/outputs/figures/diagnostics/ab_age_weight.png", ab_age, width = 7, height = 6, dpi = 300)
ggsave("analysis/outputs/figures/diagnostics/mb_age_weight.png", mb_age, width = 7, height = 6, dpi = 300)
ggsave("analysis/outputs/figures/diagnostics/sk_age_weight.png", sk_age, width = 7, height = 6, dpi = 300)


ab_gender <- plots$ab_gender
mb_gender <- plots$mb_gender
sk_gender <- plots$sk_gender

# Save the plots
ggsave("analysis/outputs/figures/diagnostics/ab_gender_weight.png", ab_gender, width = 7, height = 6, dpi = 300)
ggsave("analysis/outputs/figures/diagnostics/mb_gender_weight.png", mb_gender, width = 7, height = 6, dpi = 300)
ggsave("analysis/outputs/figures/diagnostics/sk_gender_weight.png", sk_gender, width = 7, height = 6, dpi = 300)


ab_income <- plots$ab_income
mb_income <- plots$mb_income
sk_income <- plots$sk_income

# Save the plots
ggsave("analysis/outputs/figures/diagnostics/ab_income_weight.png", ab_income, width = 7, height = 6, dpi = 300)
ggsave("analysis/outputs/figures/diagnostics/mb_income_weight.png", mb_income, width = 7, height = 6, dpi = 300)
ggsave("analysis/outputs/figures/diagnostics/sk_income_weight.png", sk_income, width = 7, height = 6, dpi = 300)


ab_education <- plots$ab_education
mb_education <- plots$mb_education
sk_education <- plots$sk_education

# Save the plots
ggsave("analysis/outputs/figures/diagnostics/ab_education_weight.png", ab_education, width = 7, height = 6, dpi = 300)
ggsave("analysis/outputs/figures/diagnostics/mb_education_weight.png", mb_education, width = 7, height = 6, dpi = 300)
ggsave("analysis/outputs/figures/diagnostics/sk_education_weight.png", sk_education, width = 7, height = 6, dpi = 300)











