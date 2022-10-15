### For merging AQS
repo.dir = '/data/home/huan1766/PM25-Fire/'

## Capture messages and errors to a file.
zz <- file(paste0(repo.dir, "errors-logs/errors_merge_misr_aqs.Rout"), open="wt")
sink(zz, type="message")

pkgs = c('tidyverse', 'dplyr', 'sf', 'units')
for(p in pkgs) require(p, character.only = T)
rm(p, pkgs)

# Specify directory
data.fire.dir = paste0(repo.dir, 'data/fire/')
data.smoke.dir = paste0(repo.dir, 'data/smoke/')
# Need to mount project beforehand
remote.aqs.dir = '/home/huan1766/remoteproject/PM25-Research/Data/MISR/MISR_merged_data/'

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

message("==========START READING==========")

rep_pts <- rep_pts %>%
  st_as_sf() %>%
  st_set_crs(3310)

in_cali_smoke <- st_read(paste0(data.smoke.dir, "2003_2015_smoke.shp")) %>%
  st_as_sf() %>%
  st_set_crs(3310)

# Read AQS data and restrict to dates
aqs <- read.delim(paste0(remote.aqs.dir, "MISR_AQS_Matched.csv"), sep=",", strip.white=TRUE, row.names=NULL)

# aqs <- aqs %>%
#   filter(Date %in% as.character(dates))

# Create geometry object for coordinates (used for sf calculations)
aqs <- aqs %>%
  st_as_sf(coords = c("Site.Longitude", "Site.Latitude"), crs = 4326, remove=FALSE) %>%
  st_transform(3310)

# Read in California's boundaries # 
cal_bound <- st_read(paste0(repo.dir, "ca-state-boundary/CA_State_TIGER2016.shp"))

# Convert to the same coordinate system as HMS (3310)
cal_bound <- cal_bound %>%
  st_transform(3310)

# Get an array of whether each observation is in California
in_bound <- lengths(st_intersects(aqs, cal_bound))>0

# Get subset with indexing
in_cali_aqs <- aqs[in_bound,]

# Set default values for smoke indicator variables
in_cali_aqs <- in_cali_aqs %>%
  mutate(fire_dist=NA,closest_cl=NA, light=NA, med=NA, heavy=NA)

message(dim(in_cali_aqs)[1])
message("==========FINISHED READING==========")

message("==========START MERGING==========")

for (d in as.list(dates)){
  day_smoke <- in_cali_smoke %>%
    filter(date == d)
  day_aqs <- in_cali_aqs %>%
    dplyr::select(c("Date", "Site.Latitude", "Site.Longitude")) %>%
    filter(Date == d) %>%
    distinct() %>%
    st_as_sf(coords = c("Site.Longitude", "Site.Latitude"), crs = 4326, remove=FALSE) %>%
    st_transform(3310)
  day_fire <- rep_pts %>%
    filter(date == d)
  
  if (dim(day_aqs)[1] == 0) next
  
  # Get closest cluster & distance
  if (dim(day_fire)[1] != 0){
    closest_fire <- st_nearest_feature(day_aqs, day_fire)
    day_aqs$fire_dist <- units::set_units(st_distance(day_aqs$geometry, day_fire[closest_fire,]$geometry, by_element = TRUE), value=km)
    day_aqs$closest_cl <- day_fire[closest_fire,]$cluster
  } else{
    day_aqs$fire_dist <- NA
    day_aqs$closest_cl <- NA    
  }
  # Get indicator variables
  if (dim(day_smoke)[1] != 0){
    smoke_regions <- st_intersects(day_aqs$geometry, day_smoke$geometry)
    day_aqs$light <- lapply(smoke_regions, function(x)ifelse('Light' %in% unique(day_smoke[unlist(x),]$density), 1, 0))
    day_aqs$med <- lapply(smoke_regions, function(x)ifelse('Medium' %in% unique(day_smoke[unlist(x),]$density), 1, 0))
    day_aqs$heavy <- lapply(smoke_regions, function(x)ifelse('Heavy' %in% unique(day_smoke[unlist(x),]$density), 1, 0))
  } else{
    day_aqs$light <- NA
    day_aqs$med <- NA
    day_aqs$heavy <- NA    
  }
  
  day_aqs <- day_aqs %>%
    st_drop_geometry() %>%
    as_tibble() %>%
    mutate(across(c("light", "med", "heavy"), as.double))
  
  # Clean up everything
  in_cali_aqs <- in_cali_aqs %>%
    left_join(as_tibble(day_aqs), by=c("Date", "Site.Latitude", "Site.Longitude")) %>%
    mutate(light = coalesce(light.y, light.x),
           med = coalesce(med.y, med.x),
           heavy = coalesce(heavy.y, heavy.x),
           fire_dist = coalesce(fire_dist.y, fire_dist.x),
           closest_cl = coalesce(closest_cl.y, closest_cl.x)) %>% 
    dplyr::select(-light.x, -light.y, -med.x, -med.y, -heavy.x, -heavy.y, -fire_dist.x, -fire_dist.y, -closest_cl.x, -closest_cl.y)
  
}

message("==========FINISHED MERGING==========")
message(dim(in_cali_aqs)[1])
write.csv(in_cali_aqs,paste0(repo.dir, "data/merged/Merged_MISR_AQS_Matched_2003_2015.csv"), row.names = FALSE)

## reset message sink and close the file connection
sink(type="message")
close(zz)

## Display the log file
readLines(paste0(repo.dir, "errors-logs/errors_merge_misr_aqs.Rout"))
