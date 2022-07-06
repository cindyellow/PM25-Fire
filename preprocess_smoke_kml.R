# Read light, medium, heavy separately
smoke_light <- st_read("../data/smoke/smoke20181108.kml", layer="Smoke (Light)")
smoke_light <- as.data.frame(smoke_light) %>%
  mutate(type="light")

smoke_med <- st_read("../data/smoke/smoke20181108.kml", layer="Smoke (Medium)")
smoke_med <- as.data.frame(smoke_med) %>%
  mutate(type="medium")

smoke_heavy <- st_read("../data/smoke/smoke20181108.kml", layer="Smoke (Heavy)")
smoke_heavy <- as.data.frame(smoke_heavy) %>%
  mutate(type="heavy")

smoke <- list(smoke_light, smoke_med, smoke_heavy)

smoke <- smoke %>%
  reduce(full_join, by=c('Name', 'Description', 'type','geometry'))

smoke <- smoke %>%
  dplyr::select(-Name) %>%
  mutate(Description = gsub('Smoke Attributes: Start Time: ', '', Description)) %>%
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