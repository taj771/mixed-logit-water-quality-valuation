########################################################################################
# Figure 6 Spatial distribution of total WTP for improving the health score to Level
# ($ per household in the affected watershed, annual payment for 5 years).
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



# REMOVED an unused read of data/derived/Baseline_WQ_sub_basin.csv. It was loaded
# into sub_basin_wq and never referenced again - the 18 status-quo levels below are
# hardcoded instead. Worth knowing before anyone "de-duplicates" by wiring that CSV
# in: its AVE_WQ disagrees with the hardcoded values for SIX sub-basins (USS, LSS,
# RD, BO, BA, CNS). The hardcoded values match the shapefile field WQ_FINAL, which
# is already loaded as df_map further down.


# Sub-basin - BO - 1

df1 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = paste0("-((mu_b_asc + mu_b_wq_local_basin*1 - (mu_b_wq_local_basin*1", "))/b_cost)")
)%>% 
  {`rownames<-`(., NULL)}%>%
  mutate(name_code = "BO")%>%
  relocate(name_code, .before = 1)%>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))

# Sub-basin - UNS - 2

df2 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = paste0("-((mu_b_asc + mu_b_wq_local_basin*2 - (mu_b_wq_local_basin*2", "))/b_cost)")
)%>% 
  {`rownames<-`(., NULL)}%>%
  mutate(name_code = "UNS")%>%
  relocate(name_code, .before = 1)%>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))

# Sub-basin - AS -3
df3 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = paste0("-((mu_b_asc + mu_b_wq_local_basin*2 - (mu_b_wq_local_basin*3", "))/b_cost)")
)%>% 
  {`rownames<-`(., NULL)}%>%
  mutate(name_code = "AS")%>%
  relocate(name_code, .before = 1)%>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))


# Sub-basin - QU - 3
df4 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = paste0("-((mu_b_asc + mu_b_wq_local_basin*2 - (mu_b_wq_local_basin*3", "))/b_cost)")
)%>% 
  {`rownames<-`(., NULL)}%>%
  mutate(name_code = "QU")%>%
  relocate(name_code, .before = 1)%>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))



# Sub-basin - RE - 3
df5 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = paste0("-((mu_b_asc + mu_b_wq_local_basin*2 - (mu_b_wq_local_basin*3", "))/b_cost)")
)%>% 
  {`rownames<-`(., NULL)}%>%
  mutate(name_code = "RE")%>%
  relocate(name_code, .before = 1)%>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))



# Sub-basin - SO - 3
df6 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = paste0("-((mu_b_asc + mu_b_wq_local_basin*2 - (mu_b_wq_local_basin*3", "))/b_cost)")
)%>% 
  {`rownames<-`(., NULL)}%>%
  mutate(name_code = "SO")%>%
  relocate(name_code, .before = 1)%>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))




# Sub-basin - LNS - 2.5
df7 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = paste0("-((mu_b_asc + mu_b_wq_local_basin*2 - (mu_b_wq_local_basin*2.5", "))/b_cost)")
)%>% 
  {`rownames<-`(., NULL)}%>%
  mutate(name_code = "LNS")%>%
  relocate(name_code, .before = 1)%>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))


# Sub-basin - CNS - 2
df8 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = paste0("-((mu_b_asc + mu_b_wq_local_basin*2 - (mu_b_wq_local_basin*2", "))/b_cost)")
)%>% 
  {`rownames<-`(., NULL)}%>%
  mutate(name_code = "CNS")%>%
  relocate(name_code, .before = 1)%>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))


# Sub-basin - LWM - 3
df9 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = paste0("-((mu_b_asc + mu_b_wq_local_basin*2 - (mu_b_wq_local_basin*3", "))/b_cost)")
)%>% 
  {`rownames<-`(., NULL)}%>%
  mutate(name_code = "LWM")%>%
  relocate(name_code, .before = 1)%>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))



# Sub-basin - USS - 3.75
df10 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = paste0("-((mu_b_asc + mu_b_wq_local_basin*2 - (mu_b_wq_local_basin*3.75", "))/b_cost)")
)%>% 
  {`rownames<-`(., NULL)}%>%
  mutate(name_code = "USS")%>%
  relocate(name_code, .before = 1)%>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))


# Sub-basin - LSS - 3.75
df11 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = paste0("-((mu_b_asc + mu_b_wq_local_basin*2 - (mu_b_wq_local_basin*3.75", "))/b_cost)")
)%>% 
  {`rownames<-`(., NULL)}%>%
  mutate(name_code = "LSS")%>%
  relocate(name_code, .before = 1)%>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))


# Sub-basin - 	RD - 2
df12 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = paste0("-((mu_b_asc + mu_b_wq_local_basin*2 - (mu_b_wq_local_basin*2", "))/b_cost)")
)%>% 
  {`rownames<-`(., NULL)}%>%
  mutate(name_code = "RD")%>%
  relocate(name_code, .before = 1)%>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))


# Sub-basin - 	ELW - 3
df13 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = paste0("-((mu_b_asc + mu_b_wq_local_basin*2 - (mu_b_wq_local_basin*3", "))/b_cost)")
)%>% 
  {`rownames<-`(., NULL)}%>%
  mutate(name_code = "ELW")%>%
  relocate(name_code, .before = 1)%>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))


# Sub-basin - 	NE - 3
df14 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = paste0("-((mu_b_asc + mu_b_wq_local_basin*2 - (mu_b_wq_local_basin*3", "))/b_cost)")
)%>% 
  {`rownames<-`(., NULL)}%>%
  mutate(name_code = "NE")%>%
  relocate(name_code, .before = 1)%>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))


# Sub-basin - 	BA - 3
df15 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = paste0("-((mu_b_asc + mu_b_wq_local_basin*2 - (mu_b_wq_local_basin*3", "))/b_cost)")
)%>% 
  {`rownames<-`(., NULL)}%>%
  mutate(name_code = "BA")%>%
  relocate(name_code, .before = 1)%>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))


# Sub-basin - 	SA - 3.5
df16 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = paste0("-((mu_b_asc + mu_b_wq_local_basin*2 - (mu_b_wq_local_basin*3.5", "))/b_cost)")
)%>% 
  {`rownames<-`(., NULL)}%>%
  mutate(name_code = "SA")%>%
  relocate(name_code, .before = 1)%>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))


# Sub-basin - 	GB - 4
df17 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = paste0("-((mu_b_asc + mu_b_wq_local_basin*2 - (mu_b_wq_local_basin*4", "))/b_cost)")
)%>% 
  {`rownames<-`(., NULL)}%>%
  mutate(name_code = "GB")%>%
  relocate(name_code, .before = 1)%>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))


# Sub-basin - 	WLW - 5
df18 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = paste0("-((mu_b_asc + mu_b_wq_local_basin*2 - (mu_b_wq_local_basin*5", "))/b_cost)")
)%>% 
  {`rownames<-`(., NULL)}%>%
  mutate(name_code = "WLW")%>%
  relocate(name_code, .before = 1)%>%
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

# Sequential palettes (good for ordered data)
display.brewer.all(type = "seq")
#custom_palette <- brewer.pal(9, "YlOrRd")  # Yellow to red
custom_palette <- brewer.pal(n_colors, "Blues")   # Light to dark blue


value_map <- tm_shape(df_map, crs = 3347) +
  tm_fill(
    col = "WTP_2",
    palette = custom_palette,
    style = "cont",  # This indicates continuous data
    title = "Willingness to Pay ($)",
    legend.reverse = TRUE
  ) +
  tm_borders() +
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
  tm_shape(cities) +
  tm_symbols(col = "blue", size = 0.1) +
  tm_text("city", size = 0.7, col = "black", ymod = -0.5) +
  tm_legend(frame = FALSE)

value_map

# Save to PNG
tmap_save(value_map, "analysis/outputs/figures/value_map.png", width = 10, height = 8, units = "in", dpi = 300)

