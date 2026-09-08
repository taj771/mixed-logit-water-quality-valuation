model <- apollo_loadModel("analysis/outputs/models/Model 4.4")  # Replace "mymodel" with your actual modelName

# Display model outputs
apollo_modelOutput(model)


# Extract coefficients and covariance matrix
coef_values <- model$estimate
vcov_matrix <- model$robvarcov

# Model 3 - sub basin FE - imprve WQ to level 2

# sub_basin == "Assiniboine" basin code =  1, SQ WQ level avg across 4 version = 3

df1 <- deltaMethod(
  object = coef_values,
  vcov. = vcov_matrix,
  g = "((mu_b_asc + b_basesq_exp*3 + b_subbasin_1 + mu_b_wq_local_sub_basin*(2-3)) / -b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(name_code = "AS") %>%
  relocate(name_code, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))


# sub_basin == "Qu'Appelle" sub basin code =  2, SQ WQ level avg across 4 version = 3

df2 <- deltaMethod(
  object = coef_values,
  vcov. = vcov_matrix,
  g = "((mu_b_asc + b_basesq_exp*3 + b_subbasin_2 + mu_b_wq_local_sub_basin*(2-3)) / -b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(name_code = "QU") %>%
  relocate(name_code, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))


# sub_basin == "Red" sub basin code = 3, SQ WQ level avg across 4 version = 3


df3 <- deltaMethod(
  object = coef_values,
  vcov. = vcov_matrix,
  g = "((mu_b_asc + b_basesq_exp*3 + b_subbasin_3 + mu_b_wq_local_sub_basin*(2-3)) / -b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(name_code = "RE") %>%
  relocate(name_code, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))


# sub_basin == "Souris" sub basin code =  4, SQ WQ level avg across 4 version = 3

df4 <- deltaMethod(
  object = coef_values,
  vcov. = vcov_matrix,
  g = "((mu_b_asc + b_basesq_exp*3 + b_subbasin_4 + mu_b_wq_local_sub_basin*(2-3)) / -b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(name_code = "SO") %>%
  relocate(name_code, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))


# sub_basin == "East Lake Winnipeg" sub basin code = 5, SQ WQ level avg across 4 version = 3

df5 <- deltaMethod(
  object = coef_values,
  vcov. = vcov_matrix,
  g = "((mu_b_asc + b_basesq_exp*3 + b_subbasin_5 + mu_b_wq_local_sub_basin*(2-3)) / -b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(name_code = "ELW") %>%
  relocate(name_code, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))


# sub_basin == "Grass and Burntwood" sub basin code = 6, SQ WQ level avg across 4 version = 4

df6 <- deltaMethod(
  object = coef_values,
  vcov. = vcov_matrix,
  g = "((mu_b_asc + b_basesq_exp*4 + b_subbasin_6 + mu_b_wq_local_sub_basin*(2-4)) / -b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(name_code = "GB") %>%
  relocate(name_code, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))


# sub_basin == "Lake Winnipegosis and Lake Manitoba" sub basin code = 7,SQ WQ level avg across 4 version = 3

df7 <- deltaMethod(
  object = coef_values,
  vcov. = vcov_matrix,
  g = "((mu_b_asc + b_basesq_exp*3 + b_subbasin_7 + mu_b_wq_local_sub_basin*(2-3)) / -b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(name_code = "LWM") %>%
  relocate(name_code, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))

# sub_basin == "Nelson" sub basin code = 8, SQ WQ level avg across 4 version = 3

df8 <- deltaMethod(
  object = coef_values,
  vcov. = vcov_matrix,
  g = "((mu_b_asc + b_basesq_exp*3 + b_subbasin_8 + mu_b_wq_local_sub_basin*(2-3)) / -b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(name_code = "NE") %>%
  relocate(name_code, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))

# sub_basin == "Saskatchewan" sub basin code = 9, SQ WQ level avg across 4 version = 3.5

df9 <- deltaMethod(
  object = coef_values,
  vcov. = vcov_matrix,
  g = "((mu_b_asc + b_basesq_exp*3.5 + b_subbasin_9 + mu_b_wq_local_sub_basin*(2-3.5)) / -b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(name_code = "SA") %>%
  relocate(name_code, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))


# sub_basin == "Western Lake Winnipeg" sub basin code = 10, SQ WQ level avg across 4 version = 5

df10 <- deltaMethod(
  object = coef_values,
  vcov. = vcov_matrix,
  g = "((mu_b_asc + b_basesq_exp*5 + b_subbasin_10 + mu_b_wq_local_sub_basin*(2-5)) / -b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(name_code = "WLW") %>%
  relocate(name_code, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))

# sub_basin == "Battle" sub basin code = 11, SQ WQ level avg across 4 version = 3

df11 <- deltaMethod(
  object = coef_values,
  vcov. = vcov_matrix,
  g = "((mu_b_asc + b_basesq_exp*3 + b_subbasin_11 + mu_b_wq_local_sub_basin*(2-3)) / -b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(name_code = "BA") %>%
  relocate(name_code, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))


# sub_basin == "Central North Saskatchewan" sub basin code = 12, SQ WQ level avg across 4 version = 2

df12 <- deltaMethod(
  object = coef_values,
  vcov. = vcov_matrix,
  g = "((mu_b_asc + b_basesq_exp*2 + b_subbasin_12 + mu_b_wq_local_sub_basin*(2-2)) / -b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(name_code = "CNS") %>%
  relocate(name_code, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))

# sub_basin == "Lower North Saskatchewan" sub basin code = 13, SQ WQ level avg across 4 version = 2.5

df13 <- deltaMethod(
  object = coef_values,
  vcov. = vcov_matrix,
  g = "((mu_b_asc + b_basesq_exp*2.5 + b_subbasin_13 + mu_b_wq_local_sub_basin*(2-2.5)) / -b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(name_code = "LNS") %>%
  relocate(name_code, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))

# sub_basin == "Upper North Saskatchewan" sub basin code = 14, SQ WQ level avg across 4 version = 2

df14 <- deltaMethod(
  object = coef_values,
  vcov. = vcov_matrix,
  g = "((mu_b_asc + b_basesq_exp*2 + b_subbasin_14 + mu_b_wq_local_sub_basin*(2-2)) / -b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(name_code = "UNS") %>%
  relocate(name_code, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))

# sub_basin == "Bow" sub basin code = 15, SQ WQ level avg across 4 version = 1

df15 <- deltaMethod(
  object = coef_values,
  vcov. = vcov_matrix,
  g = "((mu_b_asc + b_basesq_exp*1 + b_subbasin_15 + mu_b_wq_local_sub_basin*(0)) / -b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(name_code = "BO") %>%
  relocate(name_code, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))

# sub_basin == "Lower South Saskatchewan" sub basin code = 16, SQ WQ level avg across 4 version =  3.75

df16 <- deltaMethod(
  object = coef_values,
  vcov. = vcov_matrix,
  g = "((mu_b_asc + b_basesq_exp*3.75 + b_subbasin_16 + mu_b_wq_local_sub_basin*(2-3.75)) / -b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(name_code = "LSS") %>%
  relocate(name_code, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))

# sub_basin == "Red Deer" sub basin code = 17, SQ WQ level avg across 4 version = 2


df17 <- deltaMethod(
  object = coef_values,
  vcov. = vcov_matrix,
  g = "((mu_b_asc + b_basesq_exp*2 + b_subbasin_17 + mu_b_wq_local_sub_basin*(2-2)) / -b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(name_code = "RD") %>%
  relocate(name_code, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))

# sub_basin == "Upper South Saskatchewan" sub basin code = 18, SQ WQ level avg across 4 version = 3.75

df18 <- deltaMethod(
  object = coef_values,
  vcov. = vcov_matrix,
  g = "((mu_b_asc + b_basesq_exp*3.75  + mu_b_wq_local_sub_basin*(2-3.75)) / -b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(name_code = "USS") %>%
  relocate(name_code, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))



df <- rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,
            df11,df12,df13,df14,df15,df16,df17,df18)


df_map <- st_read("data/gis/study_area_map_with_WQ.shp")



df <- df%>%
  dplyr::select(Estimate,name_code)%>%
  rename(WTP_2 = Estimate)%>%
  as.data.frame()

df_map<- df_map%>%
  left_join(df)%>%
  mutate(HEALTH_SCORE = as.numeric(str_remove(WQ_FINAL, "Level")))


ab <- st_read("data/gis/AB.shp")
mb <- st_read("data/gis/MB.shp")
sk <- st_read("data/gis/SK.shp")

library(tidygeocoder)

# Example data frame with city names
cities <- data.frame(city = c("Grand Prairie","Slave Lake","Fort McMurray","Edson","Edmonton","Red Deer","Banf","Calgary","Brooks","Drumheller","Brooks",
                              "Cold lake","North Battleford", "Medicine Hat", "Moose Jaw","Saskatoon","Regina","Prince Albert", "Fort Qu'Apelle",
                              "Swan River", "The pas", "Split Lake", "Cross Lake", "Oxford House", "Altona", "Stonewall", "Minnedosa"))

# Geocode using OSM (default)
geocoded_cities <- cities %>%
  geocode(address = city, method = "osm", lat = latitude, long = longitude)

# Convert to sf object (spatial point)
cities <- st_as_sf(geocoded_cities, coords = c("longitude", "latitude"), crs = 4326)


cities <- cities%>%
  filter(city != "North Battleford")%>%
  filter(city != "Drumheller")%>%
  filter(city != "Moose Jaw")%>%
  filter(city != "Fort Qu'Apelle")

# Get the number of unique values in WTP_2
n_colors <- length(unique(df_map$WTP_2))

library(RColorBrewer)

# Get maximum colors (Blues goes up to 9)
full_blues <- brewer.pal(9, "Blues")  # All 9 colors

# Take from position 4 to end (6 colors)
custom_palette <- full_blues[4:9]

# If you need more than 6 colors, interpolate
if (n_colors > 6) {
  custom_palette <- colorRampPalette(custom_palette)(n_colors)
}


# Create a categorical variable that combines basin and WTP
df_map <- df_map %>%
  mutate(basin_wtp_category = paste(name_code, "($", round(WTP_2, 1), ")", sep = " "))

# Create formatted WTP labels
df_map <- df_map %>%
  mutate(WTP_label = paste0("$", round(WTP_2, 1)))

value_map <- tm_shape(df_map, crs = 3347) +
  tm_fill(
    col = "basin_wtp_category",
    palette = custom_palette,
    style = "cat",
    title = "Basin Name (WTP $)",
    legend.show = F
  ) +
  tm_borders() +
  tm_shape(cities) +
  tm_symbols(col = "blue", size = 0.1) +
  tm_text("city", size = 0.7, col = "black", ymod = -0.5) +
  # Add formatted WTP labels
  tm_shape(df_map, crs = 3347) +
  tm_text(
    "WTP_label",               # Formatted column
    size = 0.9,
    col = "white",             # White text for contrast
    fontface = "bold",
    shadow = TRUE,
    shadow.col = "black",
    alpha = 0.9,
    just = "center"
  ) +
  tm_shape(ab, crs = 3347) +
  tm_borders(col = "black", lwd = 2) +
  tm_shape(mb, crs = 3347) +
  tm_borders(col = "black", lwd = 2) +
  tm_shape(sk, crs = 3347) +
  tm_borders(col = "black", lwd = 2) +
  tm_layout(frame = FALSE) +
  tm_scale_bar(
    breaks = c(0, 100, 200, 300, 400),
    text.size = 0.5,
    position = c(0.6, 0.008),
    color.dark = "black",
    color.light = "white"
  ) +
  tm_compass(
    type = "arrow",
    size = 2,
    position = c(0.9, 0.9)
  ) +
  tm_legend(frame = FALSE)

value_map

# Save to PNG
tmap_save(value_map, "analysis/outputs/figures/value_map_local_subbasin_FE.png", width = 10, height = 8, units = "in", dpi = 300)







