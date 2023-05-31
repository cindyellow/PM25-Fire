#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

# test if there is at least one argument: if not, return an error
if (length(args)==0) {
  stop("At least one argument must be supplied (input file).n", call.=FALSE)
}

repo.dir = './'

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

if (args[1] == "cali"){
  if (args[2]=='aqs'){
    remote.name <- 'AQS_PM25_2000_2021_Cali.csv'
  } else if (args[2] == 'csn'){
    remote.name <- 'CSN_PM25_SPEC_2000_2021_Cali.csv'
  } 
} else if (args[1] == "cmaq") {
  if (args[2]=='aqs'){
    remote.name <- 'AQS_PM25_2000_2021_USA.csv'
  } else if (args[2] == 'csn'){
    remote.name <- 'CSN_PM25_SPEC_2000_2021_USA.csv'
  }
}

if (args[2] == 'aqs_csn'){
  remote.name <- 'AQS_CSN_Data_2000_2021.csv'
} else if (args[2] == 'misr_csn'){
  remote.name <- 'MISR_CSN_Matched.csv'
} else if (args[2] == 'misr_aqs'){
  remote.name <- 'MISR_AQS_Matched.csv'
}

# Specify directory
data.merged.dir = paste0(repo.dir, 'data/merged/', args[1])
data.land.dir = paste0(repo.dir, 'data/land-cover/')

# List of years with available data 
coverage_yrs <- list(2001, 2004, 2006, 2008, 2011, 2013, 2016, 2019)
# Create intervals used for each coverage year
years <- list(seq("2000", "2002", by=1), seq("2003", "2005", by=1), 
              seq("2006", "2007", by=1), seq("2008", "2009", by=1),
              seq("2010", "2012", by=1), seq("2013", "2014", by=1),
              seq("2015", "2017", by=1), seq("2018", "2022", by=1))


message("==========START READING==========")
message("==========Reading from: ", data.merged.dir, "==========")
data <- read.delim(paste0(data.merged.dir, "/Merged_", remote.name), sep=",", strip.white=TRUE)
dest_file <- paste0(data.merged.dir, "/MergedAll_", remote.name)

message("==========FINISHED READING==========")
message("Original dataset dimension: ", dim(data)[1], " observations and ", dim(data)[2], " features.")

message("==========START MERGING==========")
data.annual <- vector("list", length = length(unlist(years, recursive=FALSE)))

if ("Site.Longitude" %in% colnames(data) && "Site.Latitude" %in% colnames(data)){
  coords_name <- c("Site.Longitude", "Site.Latitude")
} else {
  coords_name <- c("Longitude", "Latitude")
}

# Tracker for iterated years
k <- 1
rem <- dim(data)[1]

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
      dplyr::select(all_of(coords_name)) %>%
      unique() %>%
      st_as_sf(coords = coords_name, crs = 4326, remove=FALSE)
    # Extract ecosys values for locations; CRS automatically transformed
    all_ecosys <- extract(nlcd, locations, sp=TRUE)
    all_ecosys <- all_ecosys %>%
      st_drop_geometry() %>%
      as_tibble() %>%
      dplyr::select(-coords.x1, -coords.x2)
    # Merge ecosys with other information
    year_data <- year_data %>%
      st_drop_geometry() %>%
      left_join(all_ecosys, by=coords_name) %>%
      rename(ecosys = "NLCD.Land.Cover.Class")
    data.annual[[k]] <- year_data
    message("==========FINISHED: ", y, "==========")
    message("dim: ", dim(year_data)[1])
    rem <- rem - dim(year_data)[1]
    message("Data to be processed: ", rem)
    k <- k + 1
  }
}

data.merged <- do.call("rbind", data.annual)
unique_ecosys <- unique(data.merged$ecosys)

message("==========FINISHED MERGING==========")
message("Data to be processed: ", rem)
message("Merged dataset dimension: ", dim(data.merged)[1], " observations and ", dim(data.merged)[2], " features.")
message("Unique ecosystems: ", paste(unique_ecosys, collapse=", "))
write.csv(data.merged, dest_file, row.names = FALSE)

## reset message sink and close the file connection
sink(type="message")
close(zz)

## Display the log file
readLines(paste0(repo.dir, "errors-logs/errors_merge_ecosys.Rout"))








