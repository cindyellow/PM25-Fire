# Data Access and Preprocessing Procedures

Currently, the repository directory has been set to `/data/home/huan1766/PM25-Fire/`. Please adjust accordingly.

## Downloading Data

Download up-to-date fire and smoke data from NOAA using the script [download_data.R](https://github.com/cindyellow/PM25-Fire/blob/main/r-scripts/download_data.R). This will download daily information for fire as text files (titled `hms_fireYYYYMMDD.txt`) and for smoke as a folder (titled `hms_smokeYYYYMMDD`) with shapefiles. Files are stored by year & month, i.e. (`fire/YYYY/MM`). Standard output & errors are accessible at `errors-logs/errors_download.Rout` in the repo.

## Preprocess Fire

Next, we will run the script [preprocess_fire_txt.R](https://github.com/cindyellow/PM25-Fire/blob/main/r-scripts/preprocess_fire_txt.R) that uses the raw fire data to generate annual shapefiles for daily fire cluster (calculated using HDBSCAN) and their representative points. In this script, you can choose the area you'd like to process (i.e. California, CMAQ area). The cluster information file will have the following variables:
- date
- cluster: unique string representing a cluster for the day
- geometry: cluster polygon
- area_km2: area of the cluster in km^2
- frp_avg: FRP average for fire points in the cluster
- frp_vars: FRP variance for fire points in the cluster
- num_pts: number of fire points in the cluster

The representative points file contains the following variables:
- date
- longitude, latitude: location of the representative point for a particular fire cluster
- satellite: device that recorded this point
- method_of_detect: method of detection for this point
- frp: FRP value for this point (could be NA)
- cluster: unique string identifying the cluster this point belongs to
- mem_prob: probability that this point belongs to the cluster
- geometry, polygon: sfc objects representing the point and cluster polygon
- area_km2, frp_avg, frp_vars, num_pts: information of the cluster this point belongs to

Since this process is timely, it is recommended to process fire data year by year to avoid timeouts. Files are stored by year at `fire/cluster_info/YYYY` and `fire/rep_pts/YYYY` for fire data in California, and at `fire_large/cluster_info/YYYY` and `fire_large/rep_pts/YYYY` for the CMAQ area. Standard output & errors are accessible at `errors-logs/errors_fire_prep.Rout` in the repo.

## Preprocess Smoke

Next, we preprocess the smoke shapefiles using the script [preprocess_smoke_shp.R](https://github.com/cindyellow/PM25-Fire/blob/main/r-scripts/preprocess_smoke_shp.R). In this script, you can choose the area you'd like to process (i.e. California, CMAQ area). The output shapefile contains the following variables:
- date
- area: area of smoke plume in km^2 
- satellite: device that recorded this smoke plume
- density: whether the smoke is "light", "medium", or "heavy"

One file is generated per area at `smoke/2003_2022_smoke.shp` for California and `smoke_large/2003_2022_smoke.shp` for CMAQ region. Standard output & errors are accessible at `errors-logs/errors_smoke_prep.Rout` in the repo.

## Merge with MISR Data

Next, we merge the preprocessed fire and smoke information with the relevant MISR file using [merge_data.R](https://github.com/cindyellow/PM25-Fire/blob/main/r-scripts/merge_data.R). MISR files are currently mounted from `/home/huan1766/remoteproject/PM25-Research/Data/` on cosmos, so please adjust accordingly. Please call this script in the commandline with the format `Rscript merge_data.R AREA FILE`, where you replace the arguments with one of the following options:
- AREA: 
    1. `cali` for merging within the California region
    2. `cmaq` for merging within the CMAQ region.
- FILE: the relevant MISR file to merge with, which could be 
    1. `aqs` for `AQS_PM25_2000_2021_AREA.csv`
    2. `csn` for `CSN_PM25_SPEC_2000_2021_AREA.csv`
    3. `aqs_csn` for `AQS_CSN_Data_2000_2021.csv`
    4. `misr_csn` for `MISR_CSN_Matched.csv`
    5. `misr_aqs` for `MISR_AQS_Matched.csv`

This script will create additional variables for the MISR data:
- fire_dist: distance to the closest fire cluster representative point in KM
- closest_cl: unique string identifying the closest fire cluster
- frp_avg, frp_vars, num_pts: information of the closest fire cluster
- light, medium, heavy: binary variables indicating if the data point is in a light, medium, or heavy smoke plume

Files are stored at `data/merged/AREA/Merged_FILE.csv`. Standard output & errors are accessible at `errors-logs/errors_FILE.Rout` in the repo.

## Add information about ecosystem/land cover index

Lastly, let's merge in a variable representing the ecosystem/land cover region that the MISR point is in. The script [merge_ecosys.R](https://github.com/cindyellow/PM25-Fire/blob/main/r-scripts/merge_ecosys.R) will also take in the same arguments as `merge_data.R` above, i.e. call in commandline as `Rscript merge_data.R AREA FILE`. First, land cover data should be downloaded from [here](https://www.mrlc.gov/data/nlcd-land-cover-conus-all-years), which will contain land cover data for the years 2001, 2004, 2006, 2008, 2011, 2013, 2016, 2019 (be sure to unzip all of the files). To understand what ecosystem each index corresponds to, refer to [this legend](https://www.mrlc.gov/data/legends/national-land-cover-database-class-legend-and-description). Files are stored at `data/merged/AREA/MergedAll_FILE.csv`, which is later used for modelling. Standard output & errors are accessible at `errors-logs/errors_merge_ecosys.Rout` in the repo.

