# This R Script is for preprocessing KML fire data, including clipping it to Californian borders and cleaning the data
# Combining all California Campfire Data #

# Combine data from different days
filelist = list.files(path="./data/fire/", pattern = "*.kml")

#assuming tab separated values with a header    
datalist = lapply(filelist, function(x)st_read(paste("./data/fire/", x, sep="")))

#assuming the same header/columns for all files
fire <- do.call("rbind", datalist) 

# Read in California's boundaries # 
cal_bound <- st_read("ca-state-boundary/CA_State_TIGER2016.shp")

# Convert to the same coordinate system as HMS (4326)
cal_bound <- cal_bound %>%
  st_set_crs(3857) %>% 
  st_transform(4326)

# Get an array of whether each observation is in California
in_bound <- lengths(st_intersects(fire, cal_bound))>0

# Get subset with indexing
in_cali <- fire[in_bound,]

# Cleaning #
in_cali <- as.data.frame(in_cali) %>%
  dplyr::select(-Name) %>%
  # Remove prefix
  mutate(Description = gsub('Fire Attributes: YearDay: ', '', Description)) %>%
  # Replace column name strings with comma
  mutate(Description = gsub('[a-zA-Z]*: ', ',', Description)) %>%
  # Separate based on comma
  separate(., col=Description, 
           into = c('date', 'time', 'satellite','method_of_detect'),
           sep = ',') 

in_cali <- in_cali %>% 
  # Remove unnecessary substrings
  mutate(satellite = (str_remove_all(satellite,"Method of"))) %>%
  # Convert into correct datetime
  mutate( time = substr(as.POSIXct(sprintf("%04.0f", as.integer(time)), format='%H%M'), 12, 16),
          start_time = as.POSIXct(paste(date, time),
                                  format = "%Y%j %H:%M"),
          date = as.POSIXct(date, format = "%Y%j")) %>%
  dplyr::select(-time)