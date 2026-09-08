###########################################################################################################
# Figure 1:Figure 1: The study area includes four main river basins across the Prairie provinces in Canada.
###########################################################################################################

### Clear memory
rm(list = ls())

df_map <- st_read("data/gis/study_area_map_with_WQ.shp")


ab <- st_read("data/gis/AB.shp")
mb <- st_read("data/gis/MB.shp")
sk <- st_read("data/gis/SK.shp")


# Example data frame with city names
cities <- data.frame(city = c("Calgary","Edmonton", "Regina", "Saskatoon", "Winnipeg"))

# Geocode using OSM (default)
geocoded_cities <- cities %>%
  geocode(address = city, method = "osm", lat = latitude, long = longitude)

# Convert to sf object (spatial point)
cities <- st_as_sf(geocoded_cities, coords = c("longitude", "latitude"), crs = 4326)


# Get the number of unique values in WTP_2
n_colors <- length(unique(df_map$basin))

# Generate a vector of n unique colors
library(viridis)
library(tmap)
# (a first my_colors/custom_palette pair was defined here and immediately
# overwritten by the block below - removed as dead code.)


# Create palette function with transparency
my_colors <- function(n) {
  adjustcolor(colorRampPalette(c("cadetblue", "dodgerblue3", "royalblue4", "deepskyblue3"))(n),
              alpha.f = 0.4)  # opacity set to 60%
}

# n_colors = length(unique(df_map$basin)); currently 4, so this is identical
# output, but it no longer breaks if a basin is ever added or removed.
custom_palette <- my_colors(n_colors)

# Check the colors
custom_palette

df_map$basin <- factor(df_map$basin, levels = c("SS", "NS", "LSN","AR"), 
                       labels = c("South Saskatchewan", "North Saskatchewan", "Lower Saskatchewan - Nelson", "Assiniboine Red"))



value_map <- tm_shape(df_map, crs = 3347)+
  tm_fill(    col = "basin",
              palette = custom_palette,
              style = "cat",
              title = "River Basin",
              labels = levels(as.factor(df_map$basin))
  ) +
  tm_borders(col = "gray46", lwd = 0.1) +
  #tm_text("name_code", size = 0.8, col = "black", remove.overlap = TRUE)+  # Adjust size,
  tm_shape(ab, crs = 3347) +
  tm_borders(col = "black", lwd = 2)+
  tm_text("PRNAME", size = 0.9, col = "black",ymod = 2.5, fontface = "bold")+
  
  tm_shape(mb, crs = 3347) +
  tm_borders(col = "black", lwd = 2)+
  tm_text("PRNAME", size = 0.9, col = "black",ymod = 6.5, fontface = "bold")+
  
  tm_shape(sk, crs = 3347) +
  tm_borders(col = "black", lwd = 2)+ 
  tm_text("PRNAME", size = 0.9, col = "black",ymod = 4.5, fontface = "bold")+
  
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
  tm_symbols(col = "black", size = 0.2) +
  tm_text("city", size = 0.9, col = "black",ymod = -0.5)+
  
  tm_legend(frame = F)


# Save to PNG
tmap_save(value_map, "analysis/outputs/figures/diagnostics/study_area.png", width = 10, height = 8, units = "in", dpi = 300)









