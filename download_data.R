# R script for downloading data in bulk
# Downloads: .txt for active fire detection datasets and kml for smoke distribution datasets
# Find files in cosmos

pkgs = c('curl', 'RCurl', 'stringr', 'downloader')
for(p in pkgs) require(p, character.only = T)
rm(p, pkgs)

# Specify the time range to examine
years <- seq("2003", "2022", by=1)
months <- seq("01", "12", by=1)

# Add leading zeroes for single-digit months
months[1:9] <- paste0("0",months[1:9])

# Specify directory for fire and smoke data
repo.dir = '/data/home/huan1766/PM25-Fire/'
data.fire.dir = paste0(repo.dir, 'data/fire/')
data.fire.dir = paste0(repo.dir, 'data/smoke/')

# Base URL containing all data 
fire_base_url <- "https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Fire_Points/Text/"
smoke_base_url <- "https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/Shapefile/"

# Loop over months of each year within range
for (year in years){
  for (month in months){
    # Str for subdirectory
    subdir <- paste0(year, "/", month, "/")
    
    # Check if directory exists and create a new folder if nonexistent
    ifelse(!dir.exists(file.path(data.fire.dir, subdir)), dir.create(file.path(data.fire.dir, subdir),recursive=TRUE), FALSE)

    # URL for a specific year-month
    fire_url <- paste0(fire_base_url, subdir)

    # List out all the filenames displayed in that URL page
    fire_filenames <- getURL(fire_url, userpwd="user:password", ftp.use.epsv = FALSE, dirlistonly = TRUE)

    # Create a list of filenames, extract, and remove NA entries; check if month has files
    fire_files <- unlist(strsplit(fire_filenames, '\n'))
    fire_files <- na.omit(str_extract(fire_files, "[a-zA-Z0-9_]+.txt"))
    if (length(fire_files) == 0){
      unlink(paste0(data.fire.dir,subdir), recursive = TRUE)
    } else{
      # Supply auth info for every download request.
      h <- new_handle()
      handle_setopt(h, userpwd = "user:pwd")
      lapply(fire_files, function(filename){
        curl_download(paste(fire_url, filename, sep = ""), destfile = paste0(data.fire.dir, subdir, filename), handle = h)})
    }
    # Do the same for smoke shapefiles
    smoke_url <- paste0(smoke_base_url, subdir)
    ifelse(!dir.exists(file.path(data.smoke.dir, subdir)), dir.create(file.path(data.smoke.dir, subdir),recursive=TRUE), FALSE)
    smoke_filenames <- getURL(smoke_url, userpwd="user:password", ftp.use.epsv = FALSE, dirlistonly = TRUE)
    smoke_files <- unlist(strsplit(smoke_filenames, '\n'))
    smoke_files <- na.omit(str_extract(smoke_files, "[a-zA-Z0-9_]+.zip"))
    if (length(smoke_files) == 0){
      unlink(paste0(data.smoke.dir,subdir), recursive = TRUE)
    } else{
      lapply(smoke_files, function(filename){
        downloader::download(paste(smoke_url, filename, sep = ""), dest=paste0(data.smoke.dir, subdir, filename), mode="wb")
        unzip(paste0(data.smoke.dir, subdir, filename), exdir = paste0(data.smoke.dir, subdir,str_replace(filename, ".zip", "")))
        # remove zip files
        file.remove(paste0(data.smoke.dir, subdir, filename))
      })      
    }
  }
}