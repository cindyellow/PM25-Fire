# R script for preprocessing smoke shp files
# Downloads: .csv for smoke in California from 2015-2022
# Find files in cosmos

## capture messages and errors to a file.
zz <- file("error_log_smoke_prep.Rout", open="wt")
sink(zz, type="message")

pkgs = c('tidyverse', 'dplyr', 'sf', 'units')
for(p in pkgs) require(p, character.only = T)
rm(p, pkgs)
sf_use_s2(FALSE)

# Specify directory
data.smoke.dir = '/data/home/huan1766/PM25-Fire/data/smoke/'

# Specify the time range to examine
years <- seq("2015", "2022", by=1)
months <- seq("01", "12", by=1)
months[1:9] <- paste0("0",months[1:9])

all_files <- c()
for (year in years){
  for (month in months){
    subdir <- paste0(year, "/", month, "/")
    dirlist = list.dirs(path = paste0(data.smoke.dir, subdir), full.names = FALSE, recursive = FALSE)
    all_files <- c(all_files, paste0(subdir,dirlist)) 
  }
}

datalist <- lapply(all_files, function(x)read_sf(dsn = paste0(data.smoke.dir, x)))
smoke <- do.call("rbind", datalist) 

# Clean time and add smoke area
smoke <- smoke %>%
  st_transform(3310) %>%
  mutate(
    # start_time = as.POSIXct(Start,
    #                              format = "%Y%j %H%M"),
    #      end_time = as.POSIXct(End,
    #                            format = "%Y%j %H%M"), 
         date = as.POSIXct(Start,
                           format = "%Y%j"),
         area = units::set_units(st_area(smoke$geometry), value=km^2)) %>%
  rename(satellite = Satellite,
         density = Density) %>%
  dplyr::select(-Start, -End) %>%
  st_as_sf() 

# Subset to California
cal_bound <- st_read("/data/home/huan1766/PM25-Fire/ca-state-boundary/CA_State_TIGER2016.shp")
cal_bound <- cal_bound %>%
  st_transform(3310)

smoke <- smoke %>%
  st_as_sf(coords = c("Lon", "Lat"), crs = 4326, remove=FALSE) %>%
  st_transform(3310) 
smoke$as_line <- st_cast(smoke$geometry, "MULTILINESTRING")
smoke$count <- stringr::str_count(smoke$as_line, ",")
smoke <- smoke %>%
  filter(count > 1)
in_bound <- lengths(st_intersects(smoke$as_line, cal_bound))>0
in_cali_smoke <- smoke[in_bound,] %>%
  select(-as_line, -count) %>%
  mutate(geometry = replace(geometry, is.na(st_is_valid(geometry)), NA))

st_write(in_cali_smoke, paste0(data.smoke.dir,"2015_2022_smoke.shp"), append=FALSE)

## reset message sink and close the file connection
sink(type="message")
close(zz)

## Display the log file
readLines("error_log_smoke_prep.Rout")

  
  
  
  
