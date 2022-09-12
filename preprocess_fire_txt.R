# R script for preprocessing fire txt files
pkgs = c('tidyverse', 'dplyr', 'sf')
for(p in pkgs) require(p, character.only = T)
rm(p, pkgs)

# Specify directory
data.fire.dir = paste0(dirname(getwd()), '/data/fire/')

# Specify the time range to examine
years <- seq("2015", "2015", by=1)
months <- seq("01", "12", by=1)
months[1:9] <- paste0("0",months[1:9])
dates <- seq(as.Date("2015-01-01"), as.Date("2015-01-29"), by=1)

minpts <- seq(5,100,by=5)

all_files <- c()
for (year in years){
  for (month in months){
    subdir <- paste0(year, "/", month, "/")
    filelist = list.files(path=paste0(data.fire.dir, subdir), pattern = "*.txt")
    all_files <- c(all_files, paste0(subdir,filelist)) 
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
cal_bound <- st_read("../ca-state-boundary/CA_State_TIGER2016.shp")

# Convert to the same coordinate system as HMS (4326)
cal_bound <- cal_bound %>%
  st_transform(3310)

# Get an array of whether each observation is in California
in_bound <- lengths(st_intersects(fire, cal_bound))>0

# Get subset with indexing
in_cali <- fire[in_bound,]

in_cali <- in_cali %>%
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
in_cali <- in_cali %>%
  mutate(
    date_comp = as.POSIXct(paste(date, time), 
                           format = "%Y-%m-%d %H:%M"),
    frp = na_if(frp, -999.000)
  ) 

# Function for finding best minpts for a day and building clusters based on that
build_best_cl <- function(day){
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
        scores[which(minpts==x)] = cdbw(coords, cl$cluster)$cdbw
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
                         cluster=c("0"))
  } else{
    # Create polygon and FRP summaries for each cluster
    day_cl <- day %>%
      group_by(cluster) %>%
      group_modify(function(x,y) bind_rows(tibble(date=d,
                                                  polygon = concaveman(x)$polygons,
                                                  area = round(st_area(polygon)/ 1e4, 3),
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
                      area=double(),
                      frp_avg=double(),
                      frp_vars=double(),
                      num_pts=integer()
) %>%
  rename(polygon=geometry.1)
cluster_info <- data.frame(date=as.Date(character()),
                           cluster=character(),
                           polygon=st_sfc(list()),
                           area=double(),
                           frp_avg=double(),
                           frp_vars=double(),
                           num_pts=integer()) %>%
  rename(polygon=geometry)

for (d in as.list(dates)){
  day <- in_cali %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove=FALSE) %>%
    st_transform(4326) %>%
    filter(date == d)
  
  cl <- build_best_cl(day)
  if (is.null(cl)){
    # No best cluster for the day
    placeholder <- data.frame(date = d,
                              cluster=NA)
    cluster_info <- cluster_info %>%
      merge(placeholder, by=c("cluster","date"), all=TRUE)
    rep_pts <- rep_pts %>%
      merge(placeholder, by=c("cluster", "date"), all=TRUE)
  } else{
    ci <- get_cluster_info(cl,day)
    reps <- get_rep_pts(cl, day, ci, 1)
    cluster_info <- rbind(cluster_info, ci)
    rep_pts <- rbind(rep_pts, reps)    
  }
  
}

