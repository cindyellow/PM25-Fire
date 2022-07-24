# Combine data from different days
filelist = list.files(path="../data/fire/", pattern = "*.txt")

#assuming tab separated values with a header    
datalist = lapply(filelist, function(x)read.delim(paste("../data/fire/",x,sep=""), sep=",", strip.white=TRUE))

#assuming the same header/columns for all files
fire <- do.call("rbind", datalist) 

# Get sf geometry of coordinates
fire <- fire %>%
  st_as_sf(coords = c("Lon", "Lat"), crs = 4326, remove=FALSE) %>%
  st_set_crs(4326)

# Read in California's boundaries # 
cal_bound <- st_read("../ca-state-boundary/CA_State_TIGER2016.shp")

# Convert to the same coordinate system as HMS (4326)
cal_bound <- cal_bound %>%
  st_set_crs(3857) %>% 
  st_transform(4326)

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
    method_of_detect = Method.of.Detect,
    ecosys = Ecosys,
    frp = Fire.RadPower
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