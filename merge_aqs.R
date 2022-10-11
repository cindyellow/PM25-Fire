### For merging AQS
## capture messages and errors to a file.
zz <- file("error_log_aqs_merge.Rout", open="wt")
sink(zz, type="message")

pkgs = c('tidyverse', 'dplyr', 'sf', 'units')
for(p in pkgs) require(p, character.only = T)
rm(p, pkgs)

# Specify directory
repo.dir = '/data/home/huan1766/PM25-Fire/'
data.fire.dir = paste0(repo.dir, 'data/fire/')
data.smoke.dir = paste0(repo.dir, 'data/smoke/')
aqs.dir = '/home/huan1766/remoteproject/PM25-Research/Data'

# Convert dataframes to sf objects
years <- seq("2003", "2015", by=1)
months <- seq("01", "12", by=1)
months[1:9] <- paste0("0",months[1:9])
dates <- seq(as.Date("2003-01-01"), as.Date("2015-12-31"), by=1)

all_files <- c()
for (year in years){
  subdir <- paste0("rep_pts/", year, "/")
  filelist = list.files(path=paste0(data.fire.dir, subdir), pattern = "*.shp", no..=FALSE)
  if (length(filelist) >0){
    all_files <- c(all_files, paste0(subdir,filelist)) 
  }
}

# Read all rep pts
datalist = lapply(all_files, function(x)st_read(paste0(data.fire.dir,x)))
rep_pts <- do.call("rbind", datalist) 

rep_pts <- rep_pts %>%
  st_as_sf() %>%
  st_set_crs(3310)

in_cali_smoke <- st_read(paste0(data.smoke.dir, "2003_2015_smoke.shp")) %>%
  st_as_sf() %>%
  st_set_crs(3310)

# Read AQS data and restrict to dates
aqs <- read.delim(paste0(aqs.dir, "AQS Data/AQS_PM25_2000_2021_Cali.csv"), sep=",", strip.white=TRUE)
# aqs <- aqs %>%
#   filter(Date %in% as.character(dates))

# Create geometry object for coordinates (used for sf calculations)
aqs <- aqs %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove=FALSE) %>%
  st_transform(3310)

# Set default values for smoke indicator variables
aqs <- aqs %>%
  mutate(fire_dist=NA,closest_cl=NA, light=NA, med=NA, heavy=NA)

for (d in as.list(dates)){
  day_smoke <- in_cali_smoke %>%
    filter(date == d)
  day_aqs <- aqs %>%
    dplyr::select(c("PM25", "Date", "Latitude", "Longitude")) %>%
    filter(Date == d)
  day_fire <- rep_pts %>%
    filter(date == d)

  # Get closest cluster & distance
  closest_fire <- st_nearest_feature(day_aqs, day_fire)

  day_aqs$fire_dist <- units::set_units(st_distance(day_aqs$geometry, day_fire[closest_fire,], by_element=TRUE), value=km)
  day_aqs$closest_cl <- day_fire[closest_fire,]$cluster

  # Get indicator variables
  smoke_regions <- st_intersects(day_aqs$geometry, day_smoke$geometry)
  day_aqs$light <- lapply(smoke_regions, function(x)ifelse('Light' %in% unique(day_smoke[unlist(x),]$density), 1, 0))
  day_aqs$med <- lapply(smoke_regions, function(x)ifelse('Medium' %in% unique(day_smoke[unlist(x),]$density), 1, 0))
  day_aqs$heavy <- lapply(smoke_regions, function(x)ifelse('Heavy' %in% unique(day_smoke[unlist(x),]$density), 1, 0))
  day_aqs <- as_tibble(day_aqs) %>%
    dplyr::select(-geometry) %>%
    mutate(across(c("light", "med", "heavy"), as.double))

  # Clean up everything
  aqs <- aqs %>%
    left_join(as_tibble(day_aqs), by=c("PM25", "Date", "Latitude", "Longitude")) %>%
    mutate(light = coalesce(light.y, light.x),
           med = coalesce(med.y, med.x),
           heavy = coalesce(heavy.y, heavy.x),
           fire_dist = coalesce(fire_dist.y, fire_dist.x),
           closest_cl = coalesce(closest_cl.y, closest_cl.x)) %>% 
    dplyr::select(-geometry,-light.x, -light.y, -med.x, -med.y, -heavy.x, -heavy.y, -fire_dist.x, -fire_dist.y, -closest_cl.x, -closest_cl.y)
}

write.csv(aqs,paste0(repo.dir, "data/merged/Merged_AQS_PM25_2000_2015_Cali.csv"), row.names = FALSE)

## reset message sink and close the file connection
sink(type="message")
close(zz)

## Display the log file
readLines("error_log_aqs_merge.Rout")