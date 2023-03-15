### For merging ecosys information
repo.dir = '/data/home/huan1766/PM25-Fire/'

## Capture messages and errors to a file.
zz <- file(paste0(repo.dir, "errors-logs/errors_merge_ecosys.Rout"), open="wt")
sink(zz, type="message")

pkgs = c('dplyr', 'sf', 'raster')
new.packages <- pkgs[!(pkgs %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
for(p in pkgs) require(p, character.only = T)
new.packages <- pkgs[!(pkgs %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
rm(p, pkgs)

# Specify directory
data.merged.dir = paste0(repo.dir, 'data/merged/')
data.land.dir = paste0(repo.dir, 'data/land-cover/')

# List of years with available data 
coverage_yrs <- list(2001, 2004, 2006, 2008, 2011, 2013, 2016, 2019)
# Create intervals used for each coverage year
years <- list(seq("2000", "2002", by=1), seq("2003", "2005", by=1), 
              seq("2006", "2007", by=1), seq("2008", "2009", by=1),
              seq("2010", "2012", by=1), seq("2013", "2014", by=1),
              seq("2015", "2017", by=1), seq("2018", "2022", by=1))

# Read in California's boundaries
cal_bound <- st_read(paste0(repo.dir, "ca-state-boundary/CA_State_TIGER2016.shp"))

message("==========START READING==========")
data <- read.delim(paste0(data.merged.dir, "WithCL_AQS_PM25_2000_2021_Cali.csv"), sep=",", strip.white=TRUE)
dest_file <- paste0(data.merged.dir, "MergedAll_AQS_PM25_2000_2021_Cali.csv")

message("==========FINISHED READING==========")
message("Original dataset dimension: ", dim(data)[1], " observations and ", dim(data)[2], " features.")

message("==========START MERGING==========")
data.annual <- vector("list", length = length(unlist(years, recursive=FALSE)))

# Tracker for iterated years
k <- 1

for(i in 1:length(coverage_yrs)){
  cov <- coverage_yrs[i]
  message("==========NLCD Data Year : ", cov, "==========")
  nlcd_file_path <- paste0(data.land.dir, sprintf("nlcd_%s_land_cover_l48_20210604/nlcd_%s_land_cover_l48_20210604.img", cov, cov))
  nlcd <- raster(nlcd_file_path)
  for(j in 1:length(years[[i]])){
    y <- years[[i]][[j]]
    message("==========PROCESSING: ", y, "==========")
    year_data <- data %>%
      filter(format(as.POSIXct(Date, format="%Y-%m-%d"), format="%Y") == y)
    if (dim(year_data)[1] == 0) next
    # Get the unique locations of data
    locations <- year_data %>%
      dplyr::select(Longitude, Latitude) %>%
      unique() %>%
      st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove=FALSE)
    # Extract ecosys values for locations; CRS automatically transformed
    all_ecosys <- extract(nlcd, locations, sp=TRUE)
    all_ecosys <- all_ecosys %>%
      st_drop_geometry() %>%
      as_tibble() %>%
      dplyr::select(-coords.x1, -coords.x2)
    # Merge ecosys with other information
    year_data <- year_data %>%
      st_drop_geometry() %>%
      left_join(all_ecosys, by=c("Latitude", "Longitude")) %>%
      rename(ecosys = "NLCD.Land.Cover.Class")
    data.annual[[k]] <- year_data
    message("==========FINISHED: ", y, "==========")
    message("dim:", dim(year_data[2]))
    k <- k + 1
  }
}

data.merged <- do.call("rbind", data.annual)
unique_ecosys <- unique(data.merged$ecosys)

message("==========FINISHED MERGING==========")
message("Merged dataset dimension: ", dim(data.merged)[1], " observations and ", dim(data.merged)[2], " features.")
message("Unique ecosystems: ", paste(unique_ecosys, collapse=", "))
write.csv(data.merged, dest_file, row.names = FALSE)

## reset message sink and close the file connection
sink(type="message")
close(zz)

## Display the log file
readLines(paste0(repo.dir, "errors-logs/errors_merge_ecosys.Rout"))








