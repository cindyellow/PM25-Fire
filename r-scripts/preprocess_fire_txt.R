# R script for preprocessing fire txt files
# Downloads: .csv for daily cluster information and representative points for fire in California from 2015-2022
# Find files in cosmos
# Directory dependencies:
  # ./ca-state-boundary/CA_State_TIGER2016.shp
repo.dir = '/data/home/huan1766/PM25-Fire/'

## Capture messages and errors to a file.
zz <- file(paste0(repo.dir, "errors-logs/errors_fire_prep.Rout"), open="wt")
sink(zz, type="message")

pkgs = c('tidyverse', 'dplyr', 'sf', 'dbscan', 'fpc', 'concaveman')
for(p in pkgs) require(p, character.only = T)
rm(p, pkgs)

# Specify directory
data.fire.dir = paste0(repo.dir, 'data/fire/')
data.fire.outdir = paste0(repo.dir, 'data/fire_large/')

# Specify the time range to examine
years <- seq("2003", "2022", by=1)
months <- seq("01", "12", by=1)
months[1:9] <- paste0("0",months[1:9])

minpts <- seq(5,100,by=5)

all_files <- c()
for (year in years){
  for (month in months){
    subdir <- paste0(year, "/", month, "/")
    filelist = list.files(path=paste0(data.fire.dir, subdir), pattern = "*.txt", no..=FALSE)
    if (length(filelist) >0){
      all_files <- c(all_files, paste0(subdir,filelist)) 
    }
  }
}

#assuming tab separated values with a header    
datalist = lapply(all_files, function(x)read.delim(paste0(data.fire.dir,x), sep=",", strip.white=TRUE))

#assuming the same header/columns for all files
fire <- do.call("rbind", datalist) 

# Get sf geometry of coordinates
fire <- fire %>%
  st_as_sf(coords = c("Lon", "Lat"), crs = 4326, remove=FALSE) %>%
  st_transform(3310)

# Read in California's boundaries # 

cal_bound <- st_read(paste0(repo.dir, "data/CMAQ-boundary/CMAQboundary.shp"))

# Convert to the same coordinate system as HMS (3310)
cal_bound <- cal_bound %>%
  st_transform(3310)

# Get an array of whether each observation is in California
in_bound <- lengths(st_intersects(fire, cal_bound))>0

# Get subset with indexing
in_cali_fire <- fire[in_bound,]

in_cali_fire <- in_cali_fire %>%
  rename(
    longitude = Lon,
    latitude = Lat,
    date = YearDay,
    time = Time, 
    satellite = Satellite,
    method_of_detect = Method,
    ecosys = Ecosystem,
    frp = FRP
  ) %>%
  mutate(
    date = as.Date(as.character(date),          
                   format = "%Y%j"),
    method_of_detect = as.factor(method_of_detect),
    satellite = as.factor(satellite),
    ecosys = as.factor(ecosys),
    time = substr(as.POSIXct(sprintf("%04.0f", time), format='%H%M'), 12, 16)
  )
# Add a new column for date and time
in_cali_fire <- in_cali_fire %>%
  mutate(
    date_comp = as.POSIXct(paste(date, time), 
                           format = "%Y-%m-%d %H:%M"),
    frp = na_if(frp, -999.000)
  ) 

# Function for finding best minpts for a day and building clusters based on that
build_best_cl <- function(day){
  if (dim(day)[1] == 0){
    # No observations for the day
    return (NULL)
  }
  coords <- as_tibble(st_coordinates(day$geometry))
  scores <- numeric(length(minpts))
  for (x in minpts){
    # check if there are enough points
    if(dim(coords)[1] <= x){
      # exit loop because x will only get bigger
      break
    } else{
      cl <- hdbscan(coords, minPts = x)
      # check if there are cluster scores available
      if (all(is.na(cl$cluster_scores))){
        scores[which(minpts==x)] = 0
      } else{
        score <- cdbw(coords, cl$cluster)$cdbw
        if (is.nan(score)){
          # Score is invalid; it means that clusters cannot be distinguished and should not even be considered
          scores[which(minpts==x)] = -1
        } else{
          scores[which(minpts==x)] = score
        }
      }
    }
  }
  
  if (max(scores) == 0){
    # No best cluster for the day
    return(NULL)
  }
  best_param <- minpts[which.max(scores)]
  return (hdbscan(coords, minPts=best_param))
}

# Function to get a dataframe of each cluster's polygon, area, FRP summaries, and number of points for a day
get_cluster_info <- function(cl, day){
  # Assign cluster to all observations 
  d <- day$date[1]
  day <- day %>%
    mutate(cluster = paste0(date, "_", cl$cluster)) %>%
    filter(cluster != paste0(date, "_", "0"))
  
  # Check if this day has 0 non-outlier observations
  if (dim(day)[1] == 0){
    # Return placeholder for this date
    day_cl <- data.frame(date = c(d), 
                         cluster=c(NA),
                         polygon=c(st_polygon()))
  } else{
    # Create polygon and FRP summaries for each cluster
    day_cl <- day %>%
      group_by(cluster) %>%
      group_modify(function(x,y) bind_rows(tibble(date=d,
                                                  polygon = concaveman(x)$polygons,
                                                  area_km2 = round(units::set_units(st_area(polygon), value=km^2), 3),
                                                  frp_avg = round(mean(x$frp, na.rm=TRUE),4),
                                                  frp_vars = round(var(x$frp, na.rm=TRUE),4),
                                                  num_pts = count(x)$n)))    
  }
  
  return (day_cl)
}

# Function for getting k representative points for a day
get_rep_pts <- function(cl, day, cluster_info, k){
  reps <- day %>%
    # Get membership probability for each observation
    mutate(cluster = paste0(date, "_", cl$cluster), mem_prob = cl$membership_prob) %>%
    arrange(desc(mem_prob)) %>%
    group_by(cluster) %>%
    # Filter by distinct locations
    distinct(geometry, .keep_all=TRUE) %>%
    # Get the top k most probable observations for each cluster
    slice(1:k) %>%
    # Append with cluster information to get complete info
    right_join(cluster_info, by=c("cluster", "date")) %>%
    select(-c(time,date_comp))
  return(reps)
}

message("==========START MERGING==========")

for (y in years){
  message("==========PROCESSING: ", y, "==========")
  # Get representative points and cluster information for all dates within range
  # Dataframes for storing all the info
  rep_pts <- data.frame(date=as.Date(character()),
                        longitude=double(),
                        latitude=double(),
                        satellite=character(),
                        method_of_detect = character(),
                        ecosys=integer(),
                        frp=double(),
                        cluster=character(),
                        mem_prob=double(),
                        geometry=st_sfc(list()),
                        polygon=st_sfc(list()),
                        area_km2=double(),
                        frp_avg=double(),
                        frp_vars=double(),
                        num_pts=integer()) %>%
    rename(polygon=geometry.1)
  cluster_info <- data.frame(date=as.Date(character()),
                             cluster=character(),
                             polygon=st_sfc(list()),
                             area_km2=double(),
                             frp_avg=double(),
                             frp_vars=double(),
                             num_pts=integer()) %>%
    rename(polygon=geometry)

  dates <- seq(as.Date(paste0(y, "-01-01")), as.Date(paste0(y, "-12-31")), by=1)
  for (d in as.list(dates)){
    day <- in_cali_fire %>%
      st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove=FALSE) %>%
      st_transform(3310) %>%
      filter(date == d)
    
    cl <- build_best_cl(day)
    if (is.null(cl)){
      # No best cluster for the day
      cluster_info[nrow(cluster_info)+1,"date"] <- d
      rep_pts[nrow(rep_pts)+1,"date"] <- d  
    } else{
      ci <- get_cluster_info(cl,day)
      reps <- get_rep_pts(cl, day, ci, 1)
      cluster_info <- rbind(cluster_info, ci)
      rep_pts <- rbind(rep_pts, reps)    
    }
  }
  
  # Convert to correct type
  rep_pts$polygon <- st_sfc(rep_pts$polygon)
  rep_pts <- rep_pts %>%
    select(-geometry)
  cluster_info$polygon <- st_sfc(cluster_info$polygon)
  
  # Retain polygons with shapefile
  # Check if directory exists and create a new folder if nonexistent
  ifelse(!dir.exists(file.path(data.fire.outdir, "cluster_info", y)), dir.create(file.path(data.fire.outdir, "cluster_info", y),recursive=TRUE), FALSE)
  ifelse(!dir.exists(file.path(data.fire.outdir, "rep_pts", y)), dir.create(file.path(data.fire.outdir, "rep_pts", y),recursive=TRUE), FALSE)
  st_write(cluster_info, paste0(data.fire.outdir, "cluster_info/", y, paste0("/", y,"_fire_cluster_info.shp")), append=FALSE)
  st_write(rep_pts, paste0(data.fire.outdir, "rep_pts/", y, paste0("/", y,"_fire_rep_pts.shp")), append=FALSE)
  message("==========FINISHED: ", y, " ==========")  
}
## reset message sink and close the file connection
sink(type="message")
close(zz)

## Display the log file
readLines(paste0(repo.dir, "errors-logs/errors_fire_prep.Rout"))



