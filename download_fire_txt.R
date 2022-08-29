# R script for downloading data in bulk; applied on fire data for now
pkgs = c('curl')
for(p in pkgs) require(p, character.only = T)
rm(p, pkgs)

# Specify the time range to examine
years <- seq("2015", "2022", by=1)
months <- seq("01", "12", by=1)

# Add leading zeroes for single-digit months
months[1:9] <- paste0("0",months[1:9])

# Specify directory for fire data
data.dir = paste0(dirname(getwd()), '/data/fire/')

# Base URL containing all data 
base_url <- "https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Fire_Points/Text/"

# Loop over months of each year within range
for (year in years){
  for (month in months){
    # Str for subdirectory
    subdir <- paste0(year, "/", month, "/")
    
    # Check if directory exists and create a new folder if nonexistent
    ifelse(!dir.exists(file.path(data.dir, subdir)), dir.create(file.path(data.dir, subdir),recursive=TRUE), FALSE)
    
    # URL for a specific year-month
    url <- paste0(base_url, subdir)
    
    # List out all the filenames displayed in that URL page
    filenames <- getURL(url, userpwd="user:password", ftp.use.epsv = FALSE, dirlistonly = TRUE)
    
    # Create a list of filenames
    files <- unlist(strsplit(filenames, '\n'))
    
    # Extract the filenames, remove NA entries
    files <- na.omit(str_extract(files, "[a-zA-Z0-9_]+.txt"))
    
    # Supply auth info for every download request.
    h <- new_handle()
    handle_setopt(h, userpwd = "user:pwd")
    lapply(files, function(filename){
      curl_download(paste(url, filename, sep = ""), destfile = paste0(data.dir, subdir, filename), handle = h)})
  }
}