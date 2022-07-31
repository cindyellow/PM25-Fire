# This R script is for preprocessing smoke KML data

sf_use_s2(FALSE)
# Combine data from different days
filelist = list.files(path="../data/smoke/", pattern = "*.kml")

# Define custom read file function to skip over errors
read_file <- function (file_path, l) {
  return(tryCatch(st_read(file_path, layer=l), error=function(e) {NULL}))
}

# Define the layer names we want to check for
layers <- c("Smoke (Light)", "Smoke (Medium)", "Smoke (Heavy)")

# Assuming tab separated values with a header    
datalist = lapply(filelist, function(x)read_file(paste("../data/smoke/", x, sep=""), l=layers[1]))
smoke_light <- do.call("rbind", datalist) 
datalist = lapply(filelist, function(x)read_file(paste("../data/smoke/", x, sep=""), l=layers[2]))
smoke_med <- do.call("rbind", datalist) 
datalist = lapply(filelist, function(x)read_file(paste("../data/smoke/", x, sep=""), l=layers[3]))
smoke_heavy <- do.call("rbind", datalist) 

# alternative way to read all three layers simultaneously
# list_of_features<-purrr::map(layers$name,~st_read(dsn="../data/smoke/smoke20181107.kml",layer=.))

# Read light, medium, heavy separately
smoke_light <- as.data.frame(smoke_light) %>%
  mutate(type="light")

smoke_med <- as.data.frame(smoke_med) %>%
  mutate(type="medium")

smoke_heavy <- as.data.frame(smoke_heavy) %>%
  mutate(type="heavy")

smoke <- list(smoke_light, smoke_med, smoke_heavy)

smoke <- smoke %>%
  reduce(full_join, by=c('Name', 'Description', 'type','geometry')) 

smoke <- smoke %>%
  dplyr::select(-Name) %>%
  # Remove any HTML tags
  mutate(Description = gsub('<[^\r\n\t\f\v ]+>', ' ', Description)) %>% 
  mutate(Description = gsub('.*Start Time: ', '', Description)) %>%
  mutate(Description = gsub('[a-zA-Z]*: ', ',', Description)) %>%
  separate(., col=Description, 
           into = c('start_time', 'end_time', 'density', 'satellite'),
           sep = ',') 

# Clean time and add smoke area
smoke <- smoke %>% 
  # Remove unnecessary substrings
  mutate(start_time = (str_remove_all(start_time,"[a-zA-Z]")),
         end_time = (str_remove_all(end_time,"[a-zA-Z]"))) %>%
  # Separate date and time based on space
  separate(., col=start_time, 
           into=c('start_date', 'st'),
           sep=' ') %>%
  separate(., col=end_time, 
           into=c('end_date','et'),
           sep=' ') %>%
  # Convert into datetime object
  mutate( st = substr(as.POSIXct(sprintf("%04.0f", as.integer(st)), format='%H%M'), 12, 16),
          et = substr(as.POSIXct(sprintf("%04.0f", as.integer(et)), format='%H%M'), 12, 16),
          start_time = as.POSIXct(paste(start_date, st),
                                  format = "%Y%j %H:%M"),
          end_time = as.POSIXct(paste(end_date, et),
                                format = "%Y%j %H:%M"),
          area = st_area(smoke$geometry)) %>%
  dplyr::select(-start_date, -end_date, -st, -et)

# Doublecheck
smoke <- smoke %>%
  mutate(density = ifelse(type == 'light', 5, ifelse(type == 'medium', 16, 27)))