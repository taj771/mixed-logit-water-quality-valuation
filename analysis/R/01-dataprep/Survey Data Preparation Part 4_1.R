##############################################################################

# Here we based in Q 25 and  deciede those recreational trips they have made rec trip
# within their chice basin.sub basins. It is a binary variable that indicate if they 
# made rec trip to during last year to tha water body within the choice basin and sub basin they
# presented with the choice

### Clear memory
rm(list = ls())

library(tidygeocoder)

study_area <- st_read("data/gis/study_area.shp")%>%
  select(basin,WSCSDA_E)%>%
  mutate(subbasin = case_when(
    WSCSDA_E == "Qu'Appelle" ~ "QU", 
    WSCSDA_E == "Assiniboine" ~ "AS", 
    WSCSDA_E == "Souris" ~ "SO", 
    WSCSDA_E == "Red" ~ "RE", 
    
    WSCSDA_E == "Grass and Burntwood River Basin" ~ "GB", 
    WSCSDA_E == "Nelson River Basin" ~ "NE", 
    WSCSDA_E == "Saskatchewan River Basin" ~ "SA", 
    WSCSDA_E == "Eastern Lake Winnipeg River Basin" ~ "ELW", 
    WSCSDA_E == "Lake Winnipegosis and Lake Manitoba River Basin" ~ "LWM", 
    WSCSDA_E == "Western Lake Winnipeg River Basin" ~ "WLW", 
    
    WSCSDA_E == "Central North Saskatchewan Sub River Basin" ~ "CNS", 
    WSCSDA_E == "Upper North Saskatchewan Sub River Basin" ~ "UNS", 
    WSCSDA_E == "Battle Sub River Basin" ~ "BA", 
    WSCSDA_E == "Lower North Saskatchewan Sub River Basin" ~ "LNS", 
    
    WSCSDA_E == "Bow Sub River Basin" ~ "BO", 
    WSCSDA_E == "Red Deer Sub River Basin" ~ "RD", 
    WSCSDA_E == "Lower South Saskatchewan Sub River Basin" ~ "LSS", 
    WSCSDA_E == "Upper South Saskatchewan Sub River Basin" ~ "USS", 
    
    TRUE ~ NA_character_  # Otherwise, assign 0
  ))%>%
  select(-WSCSDA_E)%>%
  mutate(sub_basin = case_when(
    subbasin == "AS" ~ 1,
    subbasin == "QU" ~ 2,
    subbasin == "RE" ~ 3,
    subbasin == "SO" ~ 4,
    subbasin == "ELW" ~ 5,
    subbasin == "GB" ~ 6,
    subbasin == "LWM" ~ 7,
    subbasin == "NE" ~ 8,
    subbasin == "SA" ~ 9,
    subbasin == "WLW" ~ 10,
    subbasin == "BA" ~ 11,
    subbasin == "CNS" ~ 12,
    subbasin == "LNS" ~ 13,
    subbasin == "UNS" ~ 14,
    subbasin == "BO" ~ 15,
    subbasin == "LSS" ~ 16,
    subbasin == "RD"~ 17,
    subbasin == "USS"~ 18,
    TRUE ~ NA_real_
  ))%>%
  select(-subbasin)%>%
  mutate(basin = case_when(
    basin == "AR" ~ 1,
    basin == "LSN" ~ 2,
    basin == "NS" ~ 3,
    basin == "SS" ~ 4,
    TRUE ~ NA_real_
  ))



# Read raw data file from CHAISR
database <- read_excel("data/raw/Water_Quality_Final.xlsx")%>%
  rename(CaseId = `{Case.ID}`)%>%
  filter(is.na(SURVEY_CONTENT))%>%
  group_by(CaseId) %>%
  slice_max(order_by = `{Duration.of.connection.in.seconds}`, n = 1) %>%
  ungroup()%>%
  rename(
    Q25_WB1_NAME = Q25_A1_2,
    Q25_WB2_NAME = Q25_A2_2,
    Q25_WB3_NAME = Q25_A3_2,
    Q25_WB4_NAME = Q25_A4_2,
    Q25_WB5_NAME = Q25_A5_2,
    Q25_WB1_WQ_LEVEL = Q25_B1_2,
    Q25_WB2_WQ_LEVEL = Q25_B2_2,
    Q25_WB3_WQ_LEVEL = Q25_B3_2,
    Q25_WB4_WQ_LEVEL = Q25_B4_2,
    Q25_WB5_WQ_LEVEL = Q25_B5_2,
    Q25_WB1_NEAR_TOWN = Q25_C1_2,
    Q25_WB2_NEAR_TOWN = Q25_C2_2,
    Q25_WB3_NEAR_TOWN = Q25_C3_2,
    Q25_WB4_NEAR_TOWN = Q25_C4_2,
    Q25_WB5_NEAR_TOWN = Q25_C5_2,
  )%>%
  select(CaseId,Q24_2,
         Q25_WB1_NAME,Q25_WB2_NAME,Q25_WB3_NAME,Q25_WB4_NAME,Q25_WB5_NAME,
         Q25_WB1_NEAR_TOWN,Q25_WB2_NEAR_TOWN,Q25_WB3_NEAR_TOWN,Q25_WB4_NEAR_TOWN,Q25_WB5_NEAR_TOWN)%>%
  distinct(CaseId, .keep_all = T)
#filter(Q24_2==1)

#-------------------------------------------------------------------------------
# Q25_WB1_NAME
# Select the address column
locations <- database %>%
  select(CaseId, Q25_WB1_NAME)

# Split into chunks (e.g., 100 addresses each)
chunks <- split(locations, ceiling(seq_len(nrow(locations)) / 100))

# Initialize list to store results
coords_list <- list()

# Loop with delay
for (i in seq_along(chunks)) {
  message("Processing batch ", i, " of ", length(chunks))
  Sys.sleep(2)  # pause 2 seconds between batches
  
  # Geocode each batch
  result <- geocode(
    chunks[[i]],
    address = Q25_WB1_NAME,
    method = "osm",
    full_results = FALSE,
    verbose = TRUE
  )
  
  coords_list[[i]] <- result
}

# Combine all results
coords <- bind_rows(coords_list) %>%
  drop_na(long, lat)

# Convert to sf
coords <- st_as_sf(coords, coords = c("long", "lat"), crs = 4326)
study_area <- st_transform(study_area, crs = st_crs(coords))


basin_wb1 <- st_join(coords, study_area, join = st_within)%>%
  filter(!is.na(sub_basin))%>%
  rename(Q25_WB1_BASIN=basin,
         Q25_WB1_SUBBASIN=sub_basin)%>%
  as.data.frame()%>%
  select(-geometry)

#------------------------------------------------------------------

# Q25_WB2_NAME
# Select the address column
locations <- database %>%
  select(CaseId, Q25_WB2_NAME)

# Split into chunks (e.g., 100 addresses each)
chunks <- split(locations, ceiling(seq_len(nrow(locations)) / 100))

# Initialize list to store results
coords_list <- list()

# Loop with delay
for (i in seq_along(chunks)) {
  message("Processing batch ", i, " of ", length(chunks))
  Sys.sleep(2)  # pause 2 seconds between batches
  
  # Geocode each batch
  result <- geocode(
    chunks[[i]],
    address = Q25_WB2_NAME,
    method = "osm",
    full_results = FALSE,
    verbose = TRUE
  )
  
  coords_list[[i]] <- result
}

# Combine all results
coords <- bind_rows(coords_list) %>%
  drop_na(long, lat)

# Convert to sf
coords <- st_as_sf(coords, coords = c("long", "lat"), crs = 4326)
study_area <- st_transform(study_area, crs = st_crs(coords))


basin_wb2 <- st_join(coords, study_area, join = st_within)%>%
  filter(!is.na(sub_basin))%>%
  rename(Q25_WB2_BASIN=basin,
         Q25_WB2_SUBBASIN=sub_basin)%>%
  as.data.frame()%>%
  select(-geometry)

#-------------------------------------------------------------------------------

# Q25_WB3_NAME
# Select the address column
locations <- database %>%
  select(CaseId, Q25_WB3_NAME)

# Split into chunks (e.g., 100 addresses each)
chunks <- split(locations, ceiling(seq_len(nrow(locations)) / 100))

# Initialize list to store results
coords_list <- list()

# Loop with delay
for (i in seq_along(chunks)) {
  message("Processing batch ", i, " of ", length(chunks))
  Sys.sleep(2)  # pause 2 seconds between batches
  
  # Geocode each batch
  result <- geocode(
    chunks[[i]],
    address = Q25_WB3_NAME,
    method = "osm",
    full_results = FALSE,
    verbose = TRUE
  )
  
  coords_list[[i]] <- result
}

# Combine all results
coords <- bind_rows(coords_list) %>%
  drop_na(long, lat)

# Convert to sf
coords <- st_as_sf(coords, coords = c("long", "lat"), crs = 4326)
study_area <- st_transform(study_area, crs = st_crs(coords))


basin_wb3 <- st_join(coords, study_area, join = st_within)%>%
  filter(!is.na(sub_basin))%>%
  rename(Q25_WB3_BASIN=basin,
         Q25_WB3_SUBBASIN=sub_basin)%>%
  as.data.frame()%>%
  select(-geometry)

#-------------------------------------------------------------------------------

# Q25_WB4_NAME
# Select the address column
locations <- database %>%
  select(CaseId, Q25_WB4_NAME)

# Split into chunks (e.g., 100 addresses each)
chunks <- split(locations, ceiling(seq_len(nrow(locations)) / 100))

# Initialize list to store results
coords_list <- list()

# Loop with delay
for (i in seq_along(chunks)) {
  message("Processing batch ", i, " of ", length(chunks))
  Sys.sleep(2)  # pause 2 seconds between batches
  
  # Geocode each batch
  result <- geocode(
    chunks[[i]],
    address = Q25_WB4_NAME,
    method = "osm",
    full_results = FALSE,
    verbose = TRUE
  )
  
  coords_list[[i]] <- result
}

# Combine all results
coords <- bind_rows(coords_list) %>%
  drop_na(long, lat)

# Convert to sf
coords <- st_as_sf(coords, coords = c("long", "lat"), crs = 4326)
study_area <- st_transform(study_area, crs = st_crs(coords))


basin_wb4 <- st_join(coords, study_area, join = st_within)%>%
  filter(!is.na(sub_basin))%>%
  rename(Q25_WB4_BASIN=basin,
         Q25_WB4_SUBBASIN=sub_basin)%>%
  as.data.frame()%>%
  select(-geometry)

#-------------------------------------------------------------------------------

# Q25_WB5_NAME
# Select the address column
locations <- database %>%
  select(CaseId, Q25_WB5_NAME)

# Split into chunks (e.g., 100 addresses each)
chunks <- split(locations, ceiling(seq_len(nrow(locations)) / 100))

# Initialize list to store results
coords_list <- list()

# Loop with delay
for (i in seq_along(chunks)) {
  message("Processing batch ", i, " of ", length(chunks))
  Sys.sleep(2)  # pause 2 seconds between batches
  
  # Geocode each batch
  result <- geocode(
    chunks[[i]],
    address = Q25_WB5_NAME,
    method = "osm",
    full_results = FALSE,
    verbose = TRUE
  )
  
  coords_list[[i]] <- result
}

# Combine all results
coords <- bind_rows(coords_list) %>%
  drop_na(long, lat)

# Convert to sf
coords <- st_as_sf(coords, coords = c("long", "lat"), crs = 4326)
study_area <- st_transform(study_area, crs = st_crs(coords))


basin_wb5 <- st_join(coords, study_area, join = st_within)%>%
  filter(!is.na(sub_basin))%>%
  rename(Q25_WB5_BASIN=basin,
         Q25_WB5_SUBBASIN=sub_basin)%>%
  as.data.frame()%>%
  select(-geometry)




database <- read_excel("data/raw/Water_Quality_Final.xlsx")%>%
  rename(CaseId = `{Case.ID}`)%>%
  filter(is.na(SURVEY_CONTENT))%>%
  rename(
    Q25_WB1_NAME = Q25_A1_2,
    Q25_WB2_NAME = Q25_A2_2,
    Q25_WB3_NAME = Q25_A3_2,
    Q25_WB4_NAME = Q25_A4_2,
    Q25_WB5_NAME = Q25_A5_2,
    Q25_WB1_WQ_LEVEL = Q25_B1_2,
    Q25_WB2_WQ_LEVEL = Q25_B2_2,
    Q25_WB3_WQ_LEVEL = Q25_B3_2,
    Q25_WB4_WQ_LEVEL = Q25_B4_2,
    Q25_WB5_WQ_LEVEL = Q25_B5_2,
    Q25_WB1_NEAR_TOWN = Q25_C1_2,
    Q25_WB2_NEAR_TOWN = Q25_C2_2,
    Q25_WB3_NEAR_TOWN = Q25_C3_2,
    Q25_WB4_NEAR_TOWN = Q25_C4_2,
    Q25_WB5_NEAR_TOWN = Q25_C5_2,
  )%>%
  select(CaseId,Q24_2,
         Q25_WB1_NAME,Q25_WB2_NAME,Q25_WB3_NAME,Q25_WB4_NAME,Q25_WB5_NAME,
         Q25_WB1_NEAR_TOWN,Q25_WB2_NEAR_TOWN,Q25_WB3_NEAR_TOWN,Q25_WB4_NEAR_TOWN,Q25_WB5_NEAR_TOWN)%>%
  distinct(CaseId, .keep_all = T)%>%
  left_join(basin_wb1)%>%
  left_join(basin_wb2)%>%
  left_join(basin_wb3)%>%
  left_join(basin_wb4)%>%
  left_join(basin_wb5)


#-------------------------------------------------------------------------------
#wb near town name1
# Q25_WB1_NEAR_TOWN
# Select the address column
locations <- database %>%
  select(CaseId, Q25_WB1_NEAR_TOWN)

# Split into chunks (e.g., 100 addresses each)
chunks <- split(locations, ceiling(seq_len(nrow(locations)) / 100))

# Initialize list to store results
coords_list <- list()

# Loop with delay
for (i in seq_along(chunks)) {
  message("Processing batch ", i, " of ", length(chunks))
  Sys.sleep(2)  # pause 2 seconds between batches
  
  # Geocode each batch
  result <- geocode(
    chunks[[i]],
    address = Q25_WB1_NEAR_TOWN,
    method = "osm",
    full_results = FALSE,
    verbose = TRUE
  )
  
  coords_list[[i]] <- result
}

# Combine all results
coords <- bind_rows(coords_list) %>%
  drop_na(long, lat)

# Convert to sf
coords <- st_as_sf(coords, coords = c("long", "lat"), crs = 4326)
study_area <- st_transform(study_area, crs = st_crs(coords))


basin_wb1 <- st_join(coords, study_area, join = st_within)%>%
  filter(!is.na(sub_basin))%>%
  rename(Q25_WB1_TOWN_BASIN=basin,
         Q25_WB1_TOWN_SUBBASIN=sub_basin)%>%
  as.data.frame()%>%
  select(-geometry)


#-------------------------------------------------------------------------------
#wb near town name2
# Q25_WB2_NEAR_TOWN
# Select the address column
locations <- database %>%
  select(CaseId, Q25_WB2_NEAR_TOWN)

# Split into chunks (e.g., 100 addresses each)
chunks <- split(locations, ceiling(seq_len(nrow(locations)) / 100))

# Initialize list to store results
coords_list <- list()

# Loop with delay
for (i in seq_along(chunks)) {
  message("Processing batch ", i, " of ", length(chunks))
  Sys.sleep(2)  # pause 2 seconds between batches
  
  # Geocode each batch
  result <- geocode(
    chunks[[i]],
    address = Q25_WB2_NEAR_TOWN,
    method = "osm",
    full_results = FALSE,
    verbose = TRUE
  )
  
  coords_list[[i]] <- result
}

# Combine all results
coords <- bind_rows(coords_list) %>%
  drop_na(long, lat)

# Convert to sf
coords <- st_as_sf(coords, coords = c("long", "lat"), crs = 4326)
study_area <- st_transform(study_area, crs = st_crs(coords))


basin_wb2 <- st_join(coords, study_area, join = st_within)%>%
  filter(!is.na(sub_basin))%>%
  rename(Q25_WB2_TOWN_BASIN=basin,
         Q25_WB2_TOWN_SUBBASIN=sub_basin)%>%
  as.data.frame()%>%
  select(-geometry)


#-------------------------------------------------------------------------------
#wb near town name3
# Q25_WB3_NEAR_TOWN
# Select the address column
locations <- database %>%
  select(CaseId, Q25_WB3_NEAR_TOWN)

# Split into chunks (e.g., 100 addresses each)
chunks <- split(locations, ceiling(seq_len(nrow(locations)) / 100))

# Initialize list to store results
coords_list <- list()

# Loop with delay
for (i in seq_along(chunks)) {
  message("Processing batch ", i, " of ", length(chunks))
  Sys.sleep(2)  # pause 2 seconds between batches
  
  # Geocode each batch
  result <- geocode(
    chunks[[i]],
    address = Q25_WB3_NEAR_TOWN,
    method = "osm",
    full_results = FALSE,
    verbose = TRUE
  )
  
  coords_list[[i]] <- result
}

# Combine all results
coords <- bind_rows(coords_list) %>%
  drop_na(long, lat)

# Convert to sf
coords <- st_as_sf(coords, coords = c("long", "lat"), crs = 4326)
study_area <- st_transform(study_area, crs = st_crs(coords))


basin_wb3 <- st_join(coords, study_area, join = st_within)%>%
  filter(!is.na(sub_basin))%>%
  rename(Q25_WB3_TOWN_BASIN=basin,
         Q25_WB3_TOWN_SUBBASIN=sub_basin)%>%
  as.data.frame()%>%
  select(-geometry)


#-------------------------------------------------------------------------------
#wb near town name4
# Q25_WB4_NEAR_TOWN
# Select the address column
locations <- database %>%
  select(CaseId, Q25_WB4_NEAR_TOWN)

# Split into chunks (e.g., 100 addresses each)
chunks <- split(locations, ceiling(seq_len(nrow(locations)) / 100))

# Initialize list to store results
coords_list <- list()

# Loop with delay
for (i in seq_along(chunks)) {
  message("Processing batch ", i, " of ", length(chunks))
  Sys.sleep(2)  # pause 2 seconds between batches
  
  # Geocode each batch
  result <- geocode(
    chunks[[i]],
    address = Q25_WB4_NEAR_TOWN,
    method = "osm",
    full_results = FALSE,
    verbose = TRUE
  )
  
  coords_list[[i]] <- result
}

# Combine all results
coords <- bind_rows(coords_list) %>%
  drop_na(long, lat)

# Convert to sf
coords <- st_as_sf(coords, coords = c("long", "lat"), crs = 4326)
study_area <- st_transform(study_area, crs = st_crs(coords))


basin_wb4 <- st_join(coords, study_area, join = st_within)%>%
  filter(!is.na(sub_basin))%>%
  rename(Q25_WB4_TOWN_BASIN=basin,
         Q25_WB4_TOWN_SUBBASIN=sub_basin)%>%
  as.data.frame()%>%
  select(-geometry)

#-------------------------------------------------------------------------------
#wb near town name5
# Q25_WB5_NEAR_TOWN
# Select the address column
locations <- database %>%
  select(CaseId, Q25_WB5_NEAR_TOWN)

# Split into chunks (e.g., 100 addresses each)
chunks <- split(locations, ceiling(seq_len(nrow(locations)) / 100))

# Initialize list to store results
coords_list <- list()

# Loop with delay
for (i in seq_along(chunks)) {
  message("Processing batch ", i, " of ", length(chunks))
  Sys.sleep(2)  # pause 2 seconds between batches
  
  # Geocode each batch
  result <- geocode(
    chunks[[i]],
    address = Q25_WB5_NEAR_TOWN,
    method = "osm",
    full_results = FALSE,
    verbose = TRUE
  )
  
  coords_list[[i]] <- result
}

# Combine all results
coords <- bind_rows(coords_list) %>%
  drop_na(long, lat)

# Convert to sf
coords <- st_as_sf(coords, coords = c("long", "lat"), crs = 4326)
study_area <- st_transform(study_area, crs = st_crs(coords))


basin_wb5 <- st_join(coords, study_area, join = st_within)%>%
  filter(!is.na(sub_basin))%>%
  rename(Q25_WB5_TOWN_BASIN=basin,
         Q25_WB5_TOWN_SUBBASIN=sub_basin)%>%
  as.data.frame()%>%
  select(-geometry)

#------------------------------------------------------------------

df_rec_trips <- database%>%
  left_join(basin_wb1)%>%
  left_join(basin_wb2)%>%
  left_join(basin_wb3)%>%
  left_join(basin_wb4)%>%
  left_join(basin_wb5)%>%
  mutate(
    REC_SUBBASIN_TRIP1 = case_when(
      is.na(Q25_WB1_SUBBASIN) & is.na(Q25_WB1_TOWN_SUBBASIN) ~ NA_character_,
      is.na(Q25_WB1_SUBBASIN) ~ as.character(Q25_WB1_TOWN_SUBBASIN),
      is.na(Q25_WB1_TOWN_SUBBASIN) ~ as.character(Q25_WB1_SUBBASIN),
      TRUE ~ paste(
        as.character(Q25_WB1_SUBBASIN),
        as.character(Q25_WB1_TOWN_SUBBASIN),
        sep = ", ")))%>%
  mutate(
    REC_SUBBASIN_TRIP2 = case_when(
      is.na(Q25_WB2_SUBBASIN) & is.na(Q25_WB2_TOWN_SUBBASIN) ~ NA_character_,
      is.na(Q25_WB2_SUBBASIN) ~ as.character(Q25_WB2_TOWN_SUBBASIN),
      is.na(Q25_WB2_TOWN_SUBBASIN) ~ as.character(Q25_WB2_SUBBASIN),
      TRUE ~ paste(
        as.character(Q25_WB2_SUBBASIN),
        as.character(Q25_WB2_TOWN_SUBBASIN),
        sep = ", ")))%>%
  mutate(
    REC_SUBBASIN_TRIP3 = case_when(
      is.na(Q25_WB3_SUBBASIN) & is.na(Q25_WB3_TOWN_SUBBASIN) ~ NA_character_,
      is.na(Q25_WB3_SUBBASIN) ~ as.character(Q25_WB3_TOWN_SUBBASIN),
      is.na(Q25_WB3_TOWN_SUBBASIN) ~ as.character(Q25_WB3_SUBBASIN),
      TRUE ~ paste(
        as.character(Q25_WB3_SUBBASIN),
        as.character(Q25_WB3_TOWN_SUBBASIN),
        sep = ", ")))%>%
  mutate(
    REC_SUBBASIN_TRIP4 = case_when(
      is.na(Q25_WB4_SUBBASIN) & is.na(Q25_WB4_TOWN_SUBBASIN) ~ NA_character_,
      is.na(Q25_WB4_SUBBASIN) ~ as.character(Q25_WB4_TOWN_SUBBASIN),
      is.na(Q25_WB4_TOWN_SUBBASIN) ~ as.character(Q25_WB4_SUBBASIN),
      TRUE ~ paste(
        as.character(Q25_WB4_SUBBASIN),
        as.character(Q25_WB4_TOWN_SUBBASIN),
        sep = ", ")))%>%
  mutate(
    REC_SUBBASIN_TRIP5 = case_when(
      is.na(Q25_WB5_SUBBASIN) & is.na(Q25_WB5_TOWN_SUBBASIN) ~ NA_character_,
      is.na(Q25_WB5_SUBBASIN) ~ as.character(Q25_WB5_TOWN_SUBBASIN),
      is.na(Q25_WB5_TOWN_SUBBASIN) ~ as.character(Q25_WB5_SUBBASIN),
      TRUE ~ paste(
        as.character(Q25_WB5_SUBBASIN),
        as.character(Q25_WB5_TOWN_SUBBASIN),
        sep = ", ")))%>%
  
  mutate(
    REC_BASIN_TRIP1 = case_when(
      is.na(Q25_WB1_BASIN) & is.na(Q25_WB1_TOWN_BASIN) ~ NA_character_,
      is.na(Q25_WB1_BASIN) ~ as.character(Q25_WB1_TOWN_BASIN),
      is.na(Q25_WB1_TOWN_BASIN) ~ as.character(Q25_WB1_BASIN),
      TRUE ~ paste(
        as.character(Q25_WB1_BASIN),
        as.character(Q25_WB1_TOWN_BASIN),
        sep = ", ")))%>%
  mutate(
    REC_BASIN_TRIP2 = case_when(
      is.na(Q25_WB2_BASIN) & is.na(Q25_WB2_TOWN_BASIN) ~ NA_character_,
      is.na(Q25_WB2_BASIN) ~ as.character(Q25_WB2_TOWN_BASIN),
      is.na(Q25_WB2_TOWN_BASIN) ~ as.character(Q25_WB2_BASIN),
      TRUE ~ paste(
        as.character(Q25_WB2_BASIN),
        as.character(Q25_WB2_TOWN_BASIN),
        sep = ", ")))%>%
  mutate(
    REC_BASIN_TRIP3 = case_when(
      is.na(Q25_WB3_BASIN) & is.na(Q25_WB3_TOWN_BASIN) ~ NA_character_,
      is.na(Q25_WB3_BASIN) ~ as.character(Q25_WB3_TOWN_BASIN),
      is.na(Q25_WB3_TOWN_BASIN) ~ as.character(Q25_WB3_BASIN),
      TRUE ~ paste(
        as.character(Q25_WB3_BASIN),
        as.character(Q25_WB3_TOWN_BASIN),
        sep = ", ")))%>%
  mutate(
    REC_BASIN_TRIP4 = case_when(
      is.na(Q25_WB4_BASIN) & is.na(Q25_WB4_TOWN_BASIN) ~ NA_character_,
      is.na(Q25_WB4_BASIN) ~ as.character(Q25_WB4_TOWN_BASIN),
      is.na(Q25_WB4_TOWN_BASIN) ~ as.character(Q25_WB4_BASIN),
      TRUE ~ paste(
        as.character(Q25_WB4_BASIN),
        as.character(Q25_WB4_TOWN_BASIN),
        sep = ", ")))%>%
  mutate(
    REC_BASIN_TRIP5 = case_when(
      is.na(Q25_WB5_BASIN) & is.na(Q25_WB5_TOWN_BASIN) ~ NA_character_,
      is.na(Q25_WB5_BASIN) ~ as.character(Q25_WB5_TOWN_BASIN),
      is.na(Q25_WB5_TOWN_BASIN) ~ as.character(Q25_WB5_BASIN),
      TRUE ~ paste(
        as.character(Q25_WB5_BASIN),
        as.character(Q25_WB5_TOWN_BASIN),
        sep = ", ")))%>%
  
  select(CaseId,REC_SUBBASIN_TRIP1,REC_SUBBASIN_TRIP2,REC_SUBBASIN_TRIP3,REC_SUBBASIN_TRIP4,REC_SUBBASIN_TRIP5,
         REC_BASIN_TRIP1,REC_BASIN_TRIP2,REC_BASIN_TRIP3,REC_BASIN_TRIP4,REC_BASIN_TRIP5,
         Q25_WB1_BASIN,Q25_WB2_BASIN,Q25_WB3_BASIN,Q25_WB4_BASIN,Q25_WB5_BASIN)%>%
  mutate(CaseId = as.character(CaseId))


write_csv(df_rec_trips,"data/derived/RecTrip_basininfo.csv")
