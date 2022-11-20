### For merging in cluster information
repo.dir = '/data/home/huan1766/PM25-Fire/'

## Capture messages and errors to a file.
zz <- file(paste0(repo.dir, "errors-logs/errors_merge_cl.Rout"), open="wt")
sink(zz, type="message")

pkgs = c('tidyverse', 'dplyr', 'sf', 'units')
for(p in pkgs) require(p, character.only = T)
rm(p, pkgs)

# Specify directory
data.fire.dir = paste0(repo.dir, 'data/fire/')
data.smoke.dir = paste0(repo.dir, 'data/smoke/')

# Convert dataframes to sf objects
years <- seq("2000", "2021", by=1)
months <- seq("01", "12", by=1)
months[1:9] <- paste0("0",months[1:9])

message("==========START READING==========")
aqs <- read.delim(paste0(repo.dir, "data/merged/Merged_AQS_PM25_2000_2021_Cali.csv"), sep=",", strip.white=T)

message("==========FINISHED READING==========")
message("Original dataset dimension: ", dim(aqs)[1], " observations and ", dim(aqs)[2], " features.")

message("==========START MERGING==========")
aqs.annual <- vector("list", length = length(years))

# Define custom read file function to skip over errors
read_file <- function (file_path) {
  return(tryCatch(st_read(file_path), error=function(e) {NULL}))
}

for(i in 1:length(years)){
  y <- years[i]
  message("==========PROCESSING: ", y, "==========")
  year_aqs <- aqs %>%
    filter(format(as.POSIXct(Date, format="%Y-%m-%d"), format="%Y") == y)
  cl_filename <- paste0(y, "_fire_cluster_info.shp")
  year_cl <- read_file(paste0(data.fire.dir, "cluster_info/", y, "/", cl_filename))
  if (is.null(year_cl)){
    year_aqs$frp_avg <- NA
    year_aqs$frp_vars <- NA
    year_aqs$num_pts <- NA
    aqs.annual[[i]] <- year_aqs
    next
  }
  year_cl <- year_cl %>%
    st_drop_geometry() %>%
    dplyr::select(-area_km2)
  year_aqs <- year_aqs %>%
    st_drop_geometry() %>%
    mutate(Date = as.Date(as.character(Date),          
                          format = "%Y-%m-%d")) %>%
    left_join(year_cl, by=c("closest_cl"="cluster", "Date" = "date"))
  aqs.annual[[i]] <- year_aqs
  message("==========FINISHED: ", y, "==========")
}

aqs.merged <- do.call("rbind", aqs.annual)
message("==========FINISHED MERGING==========")
message("Merged dataset dimension: ", dim(aqs.merged)[1], " observations and ", dim(aqs.merged)[2], " features.")
write.csv(aqs.merged, paste0(repo.dir, "data/merged/WithCL_AQS_PM25_2000_2021_Cali.csv"), row.names = FALSE)

## reset message sink and close the file connection
sink(type="message")
close(zz)

## Display the log file
readLines(paste0(repo.dir, "errors-logs/errors_merge_cl.Rout"))
    
    
    
    