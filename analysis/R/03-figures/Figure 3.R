###########################################################################################################
# Figure 3: The average baseline Health Score for 18 sub-basins under four baseline
# conditions across survey versions.
###########################################################################################################

### Clear memory
rm(list = ls())

df_map <- st_read("data/gis/study_area_map_with_WQ.shp")


df_map<- df_map%>%
  mutate(HEALTH_SCORE = as.numeric(str_remove(WQ_FINAL, "Level")))


ab <- st_read("data/gis/AB.shp")
mb <- st_read("data/gis/MB.shp")
sk <- st_read("data/gis/SK.shp")


# Example data frame with city names
cities <- data.frame(city = c("Grand Prairie","Slave Lake","Fort McMurray","Edson","Edmonton","Red Deer","Banf","Calgary","Brooks","Drumheller","Brooks",
                              "Cold lake","North Battleford", "Medicine Hat", "Moose Jaw","Saskatoon","Regina","Prince Albert", "Fort Qu'Apelle",
                              "Swan River", "The pas", "Split Lake", "Cross Lake", "Oxford House", "Altona", "Stonewall", "Minnedosa"))

# Geocode using OSM (default)
geocoded_cities <- cities %>%
  geocode(address = city, method = "osm", lat = latitude, long = longitude)

# Convert to sf object (spatial point)
cities <- st_as_sf(geocoded_cities, coords = c("longitude", "latitude"), crs = 4326)


# Get the number of unique values in WTP_2
n_colors <- length(unique(df_map$HEALTH_SCORE))

# Generate a vector of n unique colors
library(viridis)
library(tmap)
my_colors <- colorRampPalette(c("lightblue", "darkgreen", "yellow", "orange", "red"))
custom_palette <- my_colors(n_colors)


value_map <- tm_shape(df_map, crs = 3347)+
  tm_fill(    col = "HEALTH_SCORE",
              palette = custom_palette,
              style = "cont",
              title = "Health Score",
              labels = c("Very good", "Good", "Fair", "Poor", "Very poor")
  ) +
  tm_borders() +
  #tm_text("name_code", size = 0.8, col = "black", remove.overlap = TRUE)+  # Adjust size,
  tm_shape(ab, crs = 3347) +
  tm_borders(col = "black", lwd = 2)+
  tm_shape(mb, crs = 3347) +
  tm_borders(col = "black", lwd = 2)+
  tm_shape(sk, crs = 3347) +
  tm_borders(col = "black", lwd = 2)+ 
  #tm_shape(ab_cities) +
  #tm_borders(col = "black", lwd = 2)+
  #tm_text("name", size = 0.6, col = "black", remove.overlap = TRUE)+  # Adjust size,
  #tm_shape(mb_cities) +
  #tm_borders(col = "black", lwd = 2)+
  #tm_text("name", size = 0.6, col = "black", remove.overlap = TRUE)+  # Adjust size,
  #tm_shape(sk_cities) +
  #tm_borders(col = "black", lwd = 2)+ 
  #tm_text("name", size = 0.6, col = "black", remove.overlap = TRUE)+  # Adjust size,
  tm_layout(frame = FALSE)+
  tm_scale_bar(
    breaks = c(0, 100, 200,300,400),       # custom distance labels
    text.size = 0.5,               # smaller text
    position = c(0.6, 0.008),       # custom position
    color.dark = "black",          # tick and text color
    color.light = "white"          # fill for alternating blocks
  ) +
  tm_compass(
    type = "arrow",              # options: "arrow", "8star", "radar"
    size = 2,                    # size of the compass
    position = c(0.9, 0.9)       # custom position (x, y: 0 to 1 scale)
  )+
  
  tm_shape(cities) +
  tm_symbols(col = "blue", size = 0.1) +
  tm_text("city", size = 0.7, col = "black",ymod = -0.5)+
  
  tm_legend(frame = F)

# Save to PNG
tmap_save(value_map, "analysis/outputs/figures/diagnostics/current_wq_level.png", width = 10, height = 8, units = "in", dpi = 300)



