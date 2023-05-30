#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

# test if there is at least one argument: if not, return an error
if (length(args)==0) {
  stop("At least one argument must be supplied (input file).n", call.=FALSE)
}

repo.dir = '/data/home/huan1766/PM25-Fire/'

## Capture messages and errors to a file.
zz <- file(paste0(repo.dir, "errors-logs/errors_merge_", args[2],".Rout"), open="wt")
sink(zz, type="message")

pkgs = c('tidyverse', 'dplyr', 'sf', 'units')
for(p in pkgs) require(p, character.only = T)
rm(p, pkgs)

if (args[1]=='cali'){
  # California boundary
  bound <- st_read(paste0(repo.dir, "data/ca-state-boundary/CA_State_TIGER2016.shp"))
  if (args[2]=='aqs'){
    remote.name <- 'AQS_PM25_2000_2021_Cali.csv'
    remote.subdir <- 'AQS Data/'
  } else if (args[2] == 'csn'){
    remote.name <- 'CSN_PM25_SPEC_2000_2021_Cali.csv'
    remote.subdir <- 'CSN Data/'
  } else if (args[2] == 'aqs_csn'){
    remote.name <- 'AQS_CSN_Data_2000_2021.csv'
    remote.subdir <- 'AQS-CSN Merging/'
  } else if (args[2] == 'misr_csn'){
    remote.name <- 'MISR_CSN_Matched.csv'
    remote.subdir <- 'MISR/MISR_merged_data/'
  } else{
    remote.name <- 'MISR_AQS_Matched.csv'
    remote.subdir <- 'MISR/MISR_merged_data/'
  }
} else{
  # CMAQ boundary
  bound <- st_read(paste0(repo.dir, "data/CMAQ-boundary/CMAQboundary.shp"))
  if (args[2]=='aqs'){
    remote.name <- 'AQS_PM25_2000_2021_USA.csv'
    remote.subdir <- 'AQS Data/'
  } else if (args[2] == 'csn'){
    remote.name <- 'CSN_PM25_SPEC_2000_2021_USA.csv'
    remote.subdir <- 'CSN Data/'
  } else if (args[2] == 'aqs_csn'){
    remote.name <- 'AQS_CSN_Data_2000_2021.csv'
    remote.subdir <- 'AQS-CSN Merging/'
  } else if (args[2] == 'misr_csn'){
    remote.name <- 'MISR_CSN_Matched.csv'
    remote.subdir <- 'MISR_Large/MISR_merged_data'
  } else{
    remote.name <- 'MISR_AQS_Matched.csv'
    remote.subdir <- 'MISR_Large/MISR_merged_data'
  }
}

# Specify directory
data.fire.dir = paste0(repo.dir, 'data/fire/')
data.smoke.dir = paste0(repo.dir, 'data/smoke/')
# Need to mount project beforehand
remote.data.dir = paste0('/home/huan1766/remoteproject/PM25-Research/Data/', remote.subdir)

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

# Read all rep pts and set CRS
datalist = lapply(all_files, function(x)st_read(paste0(data.fire.dir,x)))
shpfiles <- lapply(datalist, function(x) {st_crs(x) <- 3310; x})
rep_pts <- do.call("rbind", shpfiles) 

message("==========START READING==========")

rep_pts <- rep_pts %>%
  st_as_sf()

in_bound_smoke <- st_read(paste0(data.smoke.dir, "2003_2022_smoke.shp")) %>%
  st_as_sf() %>%
  st_set_crs(3310)

# Read CSN data and restrict to dates
data <- read.delim(paste0(remote.data.dir, remote.name), sep=",", strip.white=TRUE, row.names=NULL)

if ("Site.Longitude" %in% colnames(data) && "Site.Latitude" %in% colnames(data)){
  coords_name <- c("Site.Longitude", "Site.Latitude")
} else {
  coords_name <- c("Longitude", "Latitude")
}

# Create geometry object for coordinates (used for sf calculations)
data <- data %>%
  st_as_sf(coords = coords_name, crs = 4326, remove=FALSE) %>%
  st_transform(3310)

# Convert to the same coordinate system as HMS (3310)
bound <- bound %>%
  st_transform(3310)

# Get an array of whether each observation is in California
in_bound <- lengths(st_intersects(data, bound))>0

# Get subset with indexing
in_bound_data <- data[in_bound,]
in_bound_smoke <- in_bound_smoke[in_bound,]
rep_pts <- rep_pts[in_bound,]

# Set default values for smoke indicator variables
in_bound_data <- in_bound_data %>%
  mutate(fire_dist=NA,closest_cl=NA, frp_avg=NA, frp_vars=NA, num_pts=NA, light=NA, med=NA, heavy=NA)

message("==========FINISHED READING==========")
message("Original dataset dimension: ", dim(in_bound_data)[1], " observations and ", dim(in_bound_data)[2], " features.")

message("==========START MERGING==========")
data.annual <- vector("list", length = length(years))

for(i in 1:length(years)){
  y <- years[i]
  message("==========PROCESSING: ", y, "==========")
  dates <- seq(as.Date(paste0(y, "-01-01")), as.Date(paste0(y, "-12-31")), by=1)
  year_data <- in_bound_data %>%
    filter(format(as.POSIXct(Date, format="%Y-%m-%d"), format="%Y") == y)
  for (d in as.list(dates)){
    day_smoke <- in_bound_smoke %>%
      filter(date == d)
    day_data <- year_data %>%
      dplyr::select(c("Date", coords_name)) %>%
      filter(Date == d) %>%
      distinct() %>%
      st_as_sf(coords = coords_names, crs = 4326, remove=FALSE) %>%
      st_transform(3310)
    day_fire <- rep_pts %>%
      filter(date == d)
    
    if (dim(day_data)[1] == 0) next
    
    # Get closest cluster info and distance
    if (dim(day_fire)[1] != 0){
      closest_fire <- st_nearest_feature(day_data, day_fire)
      day_data$fire_dist <- units::set_units(st_distance(day_data$geometry, day_fire[closest_fire,]$geometry, by_element = TRUE), value=km)
      day_data$closest_cl <- day_fire[closest_fire,]$cluster
      day_data$frp_avg <- day_fire[closest_fire,]$frp_avg
      day_data$frp_vars <- day_fire[closest_fire,]$frp_vrs
      day_data$num_pts <- day_fire[closest_fire,]$num_pts
    } else{
      day_data$fire_dist <- NA
      day_data$closest_cl <- NA   
      day_data$frp_avg <- NA
      day_data$frp_vars <- NA
      day_data$num_pts <- NA
    }
    # Get indicator variables
    if (dim(day_smoke)[1] != 0){
      smoke_regions <- st_intersects(day_data$geometry, day_smoke$geometry)
      day_data$light <- lapply(smoke_regions, function(x)ifelse('Light' %in% unique(day_smoke[unlist(x),]$density), 1, 0))
      day_data$med <- lapply(smoke_regions, function(x)ifelse('Medium' %in% unique(day_smoke[unlist(x),]$density), 1, 0))
      day_data$heavy <- lapply(smoke_regions, function(x)ifelse('Heavy' %in% unique(day_smoke[unlist(x),]$density), 1, 0))
    } else{
      day_data$light <- NA
      day_data$med <- NA
      day_data$heavy <- NA    
    }
    
    day_data <- day_data %>%
      st_drop_geometry() %>%
      as_tibble() %>%
      mutate(across(c("light", "med", "heavy"), as.double))
    
    # Clean up everything
    year_data <- year_data %>%
      left_join(day_data, by=c("Date", coords_name)) %>%
      mutate(light = coalesce(light.y, light.x),
             med = coalesce(med.y, med.x),
             heavy = coalesce(heavy.y, heavy.x),
             fire_dist = coalesce(fire_dist.y, fire_dist.x),
             closest_cl = coalesce(closest_cl.y, closest_cl.x),
             frp_avg=coalesce(frp_avg.y, frp_avg.x),
             frp_vars=coalesce(frp_vars.y, frp_vars.x),
             num_pts=coalesce(num_pts.y, num_pts.x)) %>% 
      dplyr::select(-light.x, -light.y, -med.x, -med.y, -heavy.x, -heavy.y, 
                    -fire_dist.x, -fire_dist.y, -closest_cl.x, -closest_cl.y,
                    -frp_avg.y, -frp_avg.x, -frp_vars.y, -frp_vars.x, -num_pts.y, -num_pts.x)
  }
  year_data <- year_data %>%
    st_drop_geometry()
  data.annual[[i]] <- year_data
  message("==========FINISHED: ", y, " ==========")  
}

data.merged <- do.call("rbind", data.annual)
message("==========FINISHED MERGING==========")
message("Merged dataset dimension: ", dim(data.merged)[1], " observations and ", dim(data.merged)[2], " features.")
write.csv(data.merged,paste0(repo.dir, "data/merged/", args[1], "/Merged_", remote.name), row.names = FALSE)

## reset message sink and close the file connection
sink(type="message")
close(zz)

## Display the log file
readLines(paste0(repo.dir, "errors-logs/errors_merge_", args[2],".Rout"))