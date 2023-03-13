### For merging ecosys information
repo.dir = '/data/home/huan1766/PM25-Fire/'

## Capture messages and errors to a file.
zz <- file(paste0(repo.dir, "errors-logs/errors_merge_ecosys.Rout"), open="wt")
sink(zz, type="message")

pkgs = c('dplyr', 'FedData', 'sf', 'raster')
for(p in pkgs) require(p, character.only = T)
rm(p, pkgs)

# Specify directory
data.merged.dir = paste0(repo.dir, 'data/merged/')

# List of years with available data 
coverage_yrs <- list(2001, 2004, 2006, 2008, 2011, 2016, 2019)
# Create intervals used for each coverage year
years <- list(seq("2000", "2002", by=1), seq("2003", "2005", by=1), 
              seq("2006", "2007", by=1), seq("2008", "2009", by=1),
              seq("2010", "2013", by=1), seq("2014", "2017", by=1),
              seq("2018", "2022", by=1))

# Read in California's boundaries
cal_bound <- st_read(paste0(repo.dir, "ca-state-boundary/CA_State_TIGER2016.shp"))

message("==========START READING==========")
csn <- read.delim(paste0(data.merged.dir, "WithCL_MISR_CSN_2000_2021.csv"), sep=",", strip.white=TRUE)

message("==========FINISHED READING==========")
message("Original dataset dimension: ", dim(csn)[1], " observations and ", dim(csn)[2], " features.")

message("==========START MERGING==========")
csn.annual <- vector("list", length = length(unlist(years, recursive=FALSE)))

# Tracker for iterated years
k <- 1

for(i in 1:length(coverage_yrs)){
  cov <- coverage_yrs[i]
  message("==========NLCD Data Year : ", cov, "==========")
  NLCD <- get_nlcd(
      template = cal_bound,
      label = "California",
      year = cov
    )
  for(j in 1:length(years[[i]])){
    y <- years[[i]][[j]]
    message("==========PROCESSING: ", y, "==========")
    year_csn <- csn %>%
      filter(format(as.POSIXct(Date, format="%Y-%m-%d"), format="%Y") == y) %>%
      dplyr::select(-ecosys)
    # Get the unique locations of CSN data
    locations <- year_csn %>%
      dplyr::select(Longitude, Latitude) %>%
      unique() %>%
      st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove=FALSE)
    # Extract ecosys values for locations; CRS automatically transformed
    all_ecosys <- extract(NLCD, locations, sp=TRUE)
    all_ecosys <- all_ecosys %>%
      st_drop_geometry() %>%
      as_tibble() %>%
      dplyr::select(-coords.x1, -coords.x2)
    # Merge ecosys with other information
    year_csn <- year_csn %>%
      st_drop_geometry() %>%
      left_join(all_ecosys, by=c("Latitude", "Longitude")) %>%
      rename(ecosys = paste0("California_NLCD_Land_Cover_", cov))
    csn.annual[[k]] <- year_csn
    message("==========FINISHED: ", y, "==========")
    message("dim:", dim(year_csn[2]))
    k <- k + 1
  }
}

csn.merged <- do.call("rbind", csn.annual)
unique_ecosys <- unique(csn.merged$ecosys)

message("==========FINISHED MERGING==========")
message("Merged dataset dimension: ", dim(csn.merged)[1], " observations and ", dim(csn.merged)[2], " features.")
message("Unique ecosystems: ", unique_ecosys)
write.csv(csn.merged, paste0(data.merged.dir, "MergedAll_MISR_CSN_2000_2021.csv"), row.names = FALSE)

## reset message sink and close the file connection
sink(type="message")
close(zz)

## Display the log file
readLines(paste0(repo.dir, "errors-logs/errors_merge_ecosys.Rout"))








