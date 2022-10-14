# R script for preprocessing smoke shp files
# Downloads: .csv for smoke in California from 2015-2022
# Find files in cosmos

## capture messages and errors to a file.
zz <- file(paste0(repo.dir, "errors-logs/errors_smoke_prep.Rout"), open="wt")
sink(zz, type="message")

pkgs = c('tidyverse', 'dplyr', 'sf', 'units', 'lwgeom')
for(p in pkgs) require(p, character.only = T)
rm(p, pkgs)
sf_use_s2(FALSE)

# Specify directory
repo.dir = '/data/home/huan1766/PM25-Fire/'
data.smoke.dir = paste0(repo.dir, 'data/smoke/')

# Specify the time range to examine
years <- seq("2015", "2015", by=1)
months <- seq("01", "12", by=1)
months[1:9] <- paste0("0",months[1:9])

all_files <- c()
for (year in years){
  for (month in months){
    subdir <- paste0(year, "/", month, "/")
    dirlist = list.dirs(path = paste0(data.smoke.dir, subdir), full.names = FALSE, recursive = FALSE)
    if (length(dirlist) >0){
      all_files <- c(all_files, paste0(subdir,dirlist)) 
    }
  }
}

datalist <- lapply(all_files, function(x)read_sf(dsn = paste0(data.smoke.dir, x)))
smoke <- do.call("rbind", datalist) 

# Clean variables
smoke <- smoke %>%
  st_transform(3310) %>%
  mutate(date = as.POSIXct(Start,
                           format = "%Y%j")) %>%
  rename(satellite = Satellite,
         density = Density) %>%
  dplyr::select(-Start, -End) %>%
  st_as_sf() 

# Subset to California
cal_bound <- st_read(paste0(repo.dir, "ca-state-boundary/CA_State_TIGER2016.shp"))
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
  mutate(geometry = replace(geometry, is.na(st_is_valid(geometry)), NA),
         area = units::set_units(st_area(geometry), value=km^2))

st_write(in_cali_smoke, paste0(data.smoke.dir,"2015_smoke.shp"), append=FALSE)

## reset message sink and close the file connection
sink(type="message")
close(zz)

## Display the log file
readLines(paste0(repo.dir, "errors-logs/errors_smoke_prep.Rout"))

  
  
  
  
