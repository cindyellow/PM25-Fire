### For merging csn
repo.dir = '/data/home/huan1766/PM25-Fire/'

## Capture messages and errors to a file.
zz <- file(paste0(repo.dir, "errors-logs/errors_merge_csn.Rout"), open="wt")
sink(zz, type="message")

pkgs = c('tidyverse', 'dplyr', 'sf', 'units')
for(p in pkgs) require(p, character.only = T)
rm(p, pkgs)

# Specify directory
data.fire.dir = paste0(repo.dir, 'data/fire/')
data.smoke.dir = paste0(repo.dir, 'data/smoke/')
# Need to mount project beforehand
remote.csn.dir = '/home/huan1766/remoteproject/PM25-Research/Data/CSN Data/'

# Convert dataframes to sf objects
years <- seq("2000", "2021", by=1)
months <- seq("01", "12", by=1)
months[1:9] <- paste0("0",months[1:9])

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
shpfiles <- lapply(datalist, function(x) {st_crs(x) <- 3310; x})
rep_pts <- do.call("rbind", shpfiles) 

message("==========START READING==========")

rep_pts <- rep_pts %>%
  st_as_sf() 

in_cali_smoke <- st_read(paste0(data.smoke.dir, "2003_2022_smoke.shp")) %>%
  st_as_sf() %>%
  st_set_crs(3310)

# Read csn data and restrict to dates
csn <- read.delim(paste0(remote.csn.dir, "CSN_PM25_SPEC_2000_2021_Cali.csv"), sep=",", strip.white=TRUE)

# Set default values for smoke indicator variables
csn <- csn %>%
  mutate(fire_dist=NA,closest_cl=NA, light=NA, med=NA, heavy=NA)

message("==========FINISHED READING==========")
message("Original dataset dimension: ", dim(csn)[1], " observations and ", dim(csn)[2], " features.")

message("==========START MERGING==========")
csn.annual <- vector("list", length = length(years))

for(i in 1:length(years)){
  y <- years[i]
  message("==========PROCESSING: ", y, "==========")
  dates <- seq(as.Date(paste0(y, "-01-01")), as.Date(paste0(y, "-12-31")), by=1)
  year_csn <- csn %>%
    filter(format(as.POSIXct(Date, format="%Y-%m-%d"), format="%Y") == y)
  for (d in as.list(dates)){
    day_smoke <- in_cali_smoke %>%
      filter(date == d)
    day_csn <- year_csn %>%
      dplyr::select(c("Date", "Latitude", "Longitude")) %>%
      filter(Date == d) %>%
      distinct() %>%
      st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove=FALSE) %>%
      st_transform(3310)
    day_fire <- rep_pts %>%
      filter(date == d)
    
    if (dim(day_csn)[1] == 0) next
    
    # Get closest cluster & distance
    if (dim(day_fire)[1] != 0){
      closest_fire <- st_nearest_feature(day_csn, day_fire)
      day_csn$fire_dist <- units::set_units(st_distance(day_csn$geometry, day_fire[closest_fire,], by_element=TRUE), value=km)
      day_csn$closest_cl <- day_fire[closest_fire,]$cluster
    } else{
      day_csn$fire_dist <- NA
      day_csn$closest_cl <- NA
    }
    # Get indicator variables
    if (dim(day_smoke)[1] != 0){
      smoke_regions <- st_intersects(day_csn$geometry, day_smoke$geometry)
      day_csn$light <- lapply(smoke_regions, function(x)ifelse('Light' %in% unique(day_smoke[unlist(x),]$density), 1, 0))
      day_csn$med <- lapply(smoke_regions, function(x)ifelse('Medium' %in% unique(day_smoke[unlist(x),]$density), 1, 0))
      day_csn$heavy <- lapply(smoke_regions, function(x)ifelse('Heavy' %in% unique(day_smoke[unlist(x),]$density), 1, 0))
    } else{
      day_csn$light <- NA
      day_csn$med <- NA
      day_csn$heavy <- NA
    }
    
    day_csn <- day_csn %>%
      st_drop_geometry() %>%
      as_tibble() %>%
      mutate(across(c("light", "med", "heavy"), as.double))
    
    # Clean up everything
    year_csn <- year_csn %>%
      st_drop_geometry() %>%
      left_join(day_csn, by=c("Date", "Latitude", "Longitude")) %>%
      mutate(light = coalesce(light.y, light.x),
             med = coalesce(med.y, med.x),
             heavy = coalesce(heavy.y, heavy.x),
             fire_dist = coalesce(fire_dist.y, fire_dist.x),
             closest_cl = coalesce(closest_cl.y, closest_cl.x)) %>% 
      dplyr::select(-light.x, -light.y, -med.x, -med.y, -heavy.x, -heavy.y, -fire_dist.x, -fire_dist.y, -closest_cl.x, -closest_cl.y)
  }
  csn.annual[[i]] <- year_csn
  message("==========FINISHED: ", y, "==========")
}

csn.merged <- do.call("rbind", csn.annual)
message("==========FINISHED MERGING==========")
message("Merged dataset dimension: ", dim(csn.merged)[1], " observations and ", dim(csn.merged)[2], " features.")
write.csv(csn.merged, paste0(repo.dir, "data/merged/Merged_CSN_PM25_SPEC_2000_2021_Cali.csv"), row.names = FALSE)

## reset message sink and close the file connection
sink(type="message")
close(zz)

## Display the log file
readLines(paste0(repo.dir, "errors-logs/errors_merge_csn.Rout"))
