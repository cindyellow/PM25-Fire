### For merging AQS
dates <- seq(as.Date("2015-01-01"), as.Date("2021-11-11"), by=1)

# Convert dataframes to sf objects
rep_pts_sf <- st_as_sf(rep_pts) %>%
  st_set_crs(3310)

# Read AQS data and restrict to campfire week
aqs <- read.delim("/data/home/huan1766/remoteproject/PM25-Research/Data/AQS Data/AQS_PM25_2000_2021_Cali.csv", sep=",", strip.white=TRUE)
aqs <- aqs %>%
  filter(Date %in% as.character(dates))

# Create geometry object for coordinates (used for sf calculations)
aqs <- aqs %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove=FALSE) %>%
  st_transform(3310)

# Set default values for smoke indicator variables
aqs <- aqs %>%
  mutate(fire_dist=NA,closest_cl=NA)

for (d in as.list(dates)){
  # day_smoke <- smoke %>%
  #   filter(date == d)
  day_aqs <- aqs %>%
    dplyr::select(c("PM25", "Date", "Latitude", "Longitude")) %>%
    filter(Date == d)
  day_fire <- rep_pts_sf %>%
    filter(date == d)

  # Get closest cluster & distance
  closest_fire <- st_nearest_feature(day_aqs, day_fire)

  day_aqs$fire_dist <- st_distance(day_aqs$geometry, day_fire[closest_fire,], by_element=TRUE)
  day_aqs$closest_cl <- day_fire[closest_fire,]$cluster

  # Get indicator variables
  # smoke_regions <- st_intersects(day_aqs$geometry, day_smoke$geometry)
  # day_aqs$light <- lapply(smoke_regions, function(x)ifelse('Light' %in% unique(day_smoke[unlist(x),]$density), 1, 0))
  # day_aqs$med <- lapply(smoke_regions, function(x)ifelse('Medium' %in% unique(day_smoke[unlist(x),]$density), 1, 0))
  # day_aqs$heavy <- lapply(smoke_regions, function(x)ifelse('Heavy' %in% unique(day_smoke[unlist(x),]$density), 1, 0))
  # day_aqs <- as_tibble(day_aqs) %>%
  #   dplyr::select(-geometry) %>%
  #   mutate(across(c("light", "med", "heavy"), as.double))

  # Clean up everything
  aqs <- aqs %>%
    left_join(as_tibble(day_aqs), by=c("PM25", "Date", "Latitude", "Longitude")) %>%
    mutate(fire_dist = coalesce(fire_dist.y, fire_dist.x),
           closest_cl = coalesce(closest_cl.y, closest_cl.x)) %>%
    dplyr::select(-fire_dist.x, -fire_dist.y, -closest_cl.x, -closest_cl.y)
}