#------------------------------------------------------------------------------------
# 4_2_extract_3D_RasterPoint.R   Extract 3D data from raster to points   
#------------------------------------------------------------------------------------

# import data
setwd(output_data)
#data <- read.csv("data_env.csv", sep = ";")
data <- read.csv("data_env.csv", sep = ";") #remember having date format in your .csv (actively change it)
summary(data)
head(data)

# explore temporal and spatial range
# use same temporal resolution (day) and numeric for lon and lat
colnames(data)[colnames(data) == "time"] <- "date"
data$date <- as.Date(data$date) #if your time scale has not hours
data$time_hours <- as.POSIXct(data$time_hours, format = "%d-%m-%y %H:%M", tz = "UTC")
data$lon <- as.numeric(gsub(",", ".", data$lon))
data$lat <- as.numeric(gsub(",", ".", data$lat))
colnames(data)[colnames(data) == "bathy"] <- "depth"
data$depth <- as.numeric(gsub(",", ".", data$depth))
range(data$date)
range(data$time_hours)
range(data$lon)
range(data$lat)
range(data$depth)

#open catalog
wd<-paste0(main_dir)
setwd(wd)
catalog <- read.csv("Catalog_CMEMS.csv", sep=";")
head(catalog)
#View(catalog)

#productid=7
#repo <- paste0(output_data, "/cmems")

# create new extract function
#--------------------------------------------------------------------------------------
# cmems3dmat       Extract vertical profiles into matrix from 3D ROMS numerical models netcdfs along the path
#--------------------------------------------------------------------------------------
#adapted from dmarch github: https://github.com/dmarch/ocean3d/blob/8b525bd1b13bea93f608e89f40ae9a561ca49e64/R/cmems2track_v2.R#L138

cmems3dmat <- function(lon, lat, date, productid, repo, maxZ){
  # Description
  # Extracts oceanographic information from 3D numerical models dowloaded from CMEMS
  #
  # Arguments
  # lon         longitude
  # lat         latitude
  # date        POSIXct date time or Date
  # depth       depth value, in meters (positive)
  # productid   id of the product from catalog table. this is used to find the netcdf file from the repository (repo)
  # repo        repository path with netcdf files. it follows the same structure as the CMEMS FTP server
  # maxZ        optional. Maximum Z level to extract the data from. ROMS has 35 levels. (level 23=300m)
  #
  # Value
  # A data frame with: varname, var0(optional), zmax(optional)
  #
  # Description
  # Extraction of values is done using the nearest neighbour 3d point.
  #
  # Note
  # This function currently uses a loop, which could be slower than other approaches.
  #
  
  # Load libraries
  library(lubridate)
  library(ncdf4)
  library(dplyr)
  
  # Get information and variable name for a given product
  product_info <- filter(catalog, id_product == productid)
  var <- as.character(product_info$variable)
  
  # get all product files
  # product_files <- list.files(paste(repo, product_info$product1, product_info$product2, sep="/"), full.names=TRUE, recursive=TRUE)
  product_files <- list.files(paste(repo, product_info$service, product_info$layer, sep="/"), full.names=TRUE, recursive=TRUE)
  
  # select first file and get dimensions
  ncfile <- product_files[1]
  nc <- nc_open(ncfile)
  nclon <- nc$dim$lon$vals#ncvar_get(nc, varid="lon") # nc$dim$lon$vals => same or faster?
  nclat <- nc$dim$lat$vals#ncvar_get(nc, varid="lat") 
  ncdepth <- nc$dim$depth$vals
  nctime <- nc$dim$time$vals
  ncday <- as.POSIXct(nctime, origin = "1970-01-01", tz = "UTC") 
  #you will have to adecuate this to transform properly the date
  #reference_time <- as.POSIXct("1900-01-01", tz = "UTC")
  #ncday <- reference_time + as.difftime(nctime, units = "mins")
  maxZ <- nc$dim$depth$len 
  nc_close(nc)
 
  # output matrix
  #out <- matrix(data=NA, nrow=length(lon), ncol=length(var))#, dimnames=as.list(var))
  
  out <- matrix(data = NA, nrow = maxZ, ncol = length(date), byrow = FALSE,  
                dimnames = list(ncdepth[1:maxZ], date)) #dinames = NULL or dimnames = list(ncdepth, data$date)
  
  # get data for each observation
  for (i in 1:length(date)){
    print(i)
    
    ## get day, lon, lat, depth
    iday <- as.POSIXct(data$time_hours[i], format = "%d-%m-%y %H:%M", tz = "UTC")# convert to ROMS file date format
    ilon <- data$lon[i]
    ilat <- data$lat[i]
    idepth <- data$depth[i]
    
    ## open netcdf matching(d)
    product_files <- list.files(paste(repo, product_info$service, product_info$layer, sep="/"), full.names=TRUE, recursive=TRUE)
    ncfile <- product_files[1]
    nc <- nc_open(ncfile)
    
    ## identify nearest neighbour locations
    minlon <- which.min(abs(nclon - ilon)) # search for nearest longitude dimension element
    minlat <- which.min(abs(nclat - ilat)) # search for nearest latitude dimension element
    mindepth <- which.min(abs(ncdepth - idepth)) # search for nearest depth dimension element
    mintime <- which.min(abs(ncday - iday)) #or ncday depending # search for nearest date dimension element
    
    ## get variable
    ncdata <- ncvar_get(nc, varid=var, start=c(minlon, minlat, 1, mintime), count=c(1,1,maxZ,1)) #[lon,lat,depth,time]
    
    # This means that the parameters lon, lat, depth and time are approximated to the one of the netCDF to which they are closer
    # this happens even if the parameters is very far away from the closer netCDF point. For example, if you .nc has data from 
    # 2021-01-01 to 2023-07-01 and you want to extact data on 1990-01-01, it will extract for the first day of the .nc (2021-01-01)
    # and the same happens for depth, lon and lat.
    
    out[, i] <- ncdata
    
    ## close nc
    nc_close(nc)
  }
  
  # return data.frame
  return(out)
}

#--------------------------------------------------------------------------------------
# Extract your own data
#--------------------------------------------------------------------------------------
#You will have to repeat the process for each raster:

# Repository to folder where netCDFs are:
repo <- paste0(output_data, "/cmems") #You will have to reorganise your netCDFs within subfolders named as: "product_1" and "product_2" as in catalog.
View(catalog)
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# (1.1) Dissolved Oxygen (SSO) ANALYSIS 
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#Extract data:
SSO_analysis <- cmems3dmat(lon=data$lon, lat=data$lat, date=data$date, productid=4, repo=repo) #each column name is the date of the observation: e.g. as.Date(18691, origin = "1970-01-01") = 2021-05-03
head(SSO_analysis)
#View(SSO_analysis)

# The extracted data regards to the parameters lon, lat, depth and time that are closer to the one of the netCDF 
# this happens even if the parameters are very far away from the closer netCDF point. For example, if you .nc has data from 
# 2021-01-01 to 2023-07-01 and you want to extact data on 1990-01-01, it will extract for the first day of the .nc (2021-01-01)
# and the same happens for depth, lon and lat.

# In terms of depth, it extracts all the values at the closer lat, lon and date to the netCDF but at all depths.
# Thus, you have to later select which one is the value you are interested in (in my case the deepest and the shallower).

#First re-arrange the extracted data set by: 

# (1) Make list of each of the columns you will use to create the dataframe:
#Create a column for depth categories:
depth_layer <- as.numeric(dimnames(SSO_analysis)[[1]])

# Initialize a list to store the selected columns
all_columns_o2 <- list()
# Loop through each column and select all values
for (i in 1:ncol(SSO_analysis)) {
  all_columns_o2[[i]] <- SSO_analysis[, i]
}

# (2) Change the name of the columns to date format to understand better which dates are outside the range of the particular
# netCDF you are obtaining the data from. In my case half of the data comes from Reanalysis netCDF and part from Analysis.

# Generate a sequence of names regarding to the dates
col_names <- colnames(SSO_analysis)
# Assign the names to the list
names(all_columns_o2) <- as.Date(as.numeric(col_names), origin = "1970-01-01")

# (3) Create a dataframe with 1 column regarding to depth and the rest to each of the points (3078 points):

# Create a new DataFrame with the "depth_layer" and "o2_level" columns
SSO_analysis_df <- data.frame(depth_layer, all_columns_o2)
head(SSO_analysis_df)
#View(SSO_analysis_df)

# (4) Select the first and last datum regarding to the shallowest (surface) and deepest possition within each location point (lon, lat, depth)

# Initialize empty vectors to store the unique deepest and surface values
unique_deepest <- vector("numeric", length(SSO_analysis_df) - 1)
unique_surface <- vector("numeric", length(SSO_analysis_df) - 1)

# Loop through the columns of SSO_analysis_df starting from the second column (index 2)
for (i in 2:length(SSO_analysis_df)) {
  col <- SSO_analysis_df[[i]]
  last_non_na_row <- max(which(!is.na(col)))
  
  # Store the unique deepest and surface values in the respective vectors
  unique_deepest[i - 1] <- col[last_non_na_row]
  unique_surface[i - 1] <- col[1]
}

# (5) Add the data to your dataframe:

# Add the unique values to the "data" dataframe
data$seabottom_o2 <- unique_deepest
data$seasurface_o2 <- unique_surface

View(data)

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# (1.2.) Dissolved Oxygen (SSO) REANALYSIS
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

repo <- paste0(output_data, "/cmems")

# Extract data: repeating the process:
SSO_reanalysis <- cmems3dmat(lon=data$lon, lat=data$lat, date=data$date, productid=3, repo=repo) #each column name is the date of the observation: e.g. as.Date(18691, origin = "1970-01-01") = 2021-05-03
head(SSO_reanalysis)

# Save data:
depth_layer <- as.numeric(dimnames(SSO_reanalysis)[[1]])
all_columns_o2 <- list()
for (i in 1:ncol(SSO_reanalysis)) {
  all_columns_o2[[i]] <- SSO_reanalysis[, i]
}

col_names <- colnames(SSO_reanalysis)
names(all_columns_o2) <- as.Date(as.numeric(col_names), origin = "1970-01-01")
SSO_reanalysis_df <- data.frame(depth_layer, all_columns_o2)
head(SSO_reanalysis_df)

unique_deepest <- vector("numeric", length(SSO_reanalysis_df) - 1)
unique_surface <- vector("numeric", length(SSO_reanalysis_df) - 1)

for (i in 2:length(SSO_reanalysis_df)) {
  col <- SSO_reanalysis_df[[i]]
  last_non_na_row <- max(which(!is.na(col)))
  
  unique_deepest[i - 1] <- col[last_non_na_row]
  unique_surface[i - 1] <- col[1]
}

# Add the unique values to the "data" dataframe
data$seabottom_o2_reanalysis <- unique_deepest
data$seasurface_o2_reanalysis <- unique_surface

View(data)

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# (1.3.) Eliminate data for the analysis and reanalysis period that are not included in the netCDF:
##- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# For all dates you will have data from both analysis and reanalysis, although, because the data is 
# extracted from the closest date, lat, lon, and depth, although it is far away. 
# You have already filtered the depth, selecting the surface and the deepest points.
# Thus, you have to now filter such data as maintain for the analysis and reanalysis the correct ranges. 

# Define the date ranges
date_range_anfc_start <- as.POSIXct("2021-07-01", format = "%Y-%m-%d", tz="UTC")
date_range_anfc_end <- as.POSIXct("2022-07-01", format = "%Y-%m-%d", tz="UTC")

date_range_rean_start <- as.POSIXct("2020-12-01", format = "%Y-%m-%d", tz="UTC")
date_range_rean_end <- as.POSIXct("2021-06-30", format = "%Y-%m-%d", tz="UTC")

# Filter and replace values for seabottom_o2_reanalysis and seasurface_o2_reanalysis
data$seabottom_o2_reanalysis <- ifelse(data$date >= date_range_rean_start & data$date <= date_range_rean_end, data$seabottom_o2_reanalysis, NA)
data$seasurface_o2_reanalysis <- ifelse(data$date >= date_range_rean_start & data$date <= date_range_rean_end, data$seasurface_o2_reanalysis, NA)

# Filter and replace values for seasurface_o2 and seabottom_o2
data$seasurface_o2 <- ifelse(data$date >= date_range_anfc_start & data$date <= date_range_anfc_end, data$seasurface_o2, NA)
data$seabottom_o2 <- ifelse(data$date >= date_range_anfc_start & data$date <= date_range_anfc_end, data$seabottom_o2, NA)

View(data)

#Merge analysis and reanalysis data:
data$sso_merged <- ifelse(is.na(data$seasurface_o2_reanalysis), data$seasurface_o2, data$seasurface_o2_reanalysis)
data$sbo_merged <- ifelse(is.na(data$seabottom_o2), data$seabottom_o2_reanalysis, data$seabottom_o2)
View(data)

#Calculate the difference in o2 between sea surface and bottom:
data$diff_sso_sbo <- data$sso_merged - data$sbo_merged
View(data)

data <- data[, !names(data) %in% c("seasurface_o2_reanalysis", "seasurface_o2", "seabottom_o2", "seabottom_o2_reanalysis")]
names(data)

#------------------------------------------------------------------------------------------------------------------------------------------------

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# (2.1) Salinity (SAL) ANALYSIS 
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#Extract data:
SAL_analysis <- cmems3dmat(lon=data$lon, lat=data$lat, date=data$date, productid=6, repo=repo) #each column name is the date of the observation: e.g. as.Date(18691, origin = "1970-01-01") = 2021-05-03
head(SAL_analysis)
# The extracted data regards to the parameters lon, lat, depth and time that are closer to the one of the netCDF 
# this happens even if the parameters are very far away from the closer netCDF point. For example, if you .nc has data from 
# 2021-01-01 to 2023-07-01 and you want to extact data on 1990-01-01, it will extract for the first day of the .nc (2021-01-01)
# and the same happens for depth, lon and lat.

# In terms of depth, it extracts all the values at the closer lat, lon and date to the netCDF but at all depths.
# Thus, you have to later select which one is the value you are interested in (in my case the deepest and the shallower).

#First re-arrange the extracted data set by: 

# (1) Make list of each of the columns you will use to create the dataframe:
#Create a column for depth categories:
depth_layer <- as.numeric(dimnames(SAL_analysis)[[1]])

# Initialize a list to store the selected columns
all_columns_SAL <- list()
# Loop through each column and select all values
for (i in 1:ncol(SAL_analysis)) {
  all_columns_SAL[[i]] <- SAL_analysis[, i]
}

# (2) Change the name of the columns to date format to understand better which dates are outside the range of the particular
# netCDF you are obtaining the data from. In my case half of the data comes from Reanalysis netCDF and part from Analysis.

# Generate a sequence of names regarding to the dates
col_names <- colnames(SAL_analysis)
# Assign the names to the list
names(all_columns_SAL) <- as.Date(as.numeric(col_names), origin = "1970-01-01")

# (3) Create a dataframe with 1 column regarding to depth and the rest to each of the points (3078 points):

# Create a new DataFrame with the "depth_layer" and "o2_level" columns
SAL_analysis_df <- data.frame(depth_layer, all_columns_SAL)
head(SAL_analysis_df)
#View(SAL_analysis_df)

# (4) Select the first and last datum regarding to the shallowest (surface) and deepest possition within each location point (lon, lat, depth)

# Initialize empty vectors to store the unique deepest and surface values
unique_deepest <- vector("numeric", length(SAL_analysis_df) - 1)
unique_surface <- vector("numeric", length(SAL_analysis_df) - 1)

# Loop through the columns of SAL_analysis_df starting from the second column (index 2)
for (i in 2:length(SAL_analysis_df)) {
  col <- SAL_analysis_df[[i]]
  last_non_na_row <- max(which(!is.na(col)))
  
  # Store the unique deepest and surface values in the respective vectors
  unique_deepest[i - 1] <- col[last_non_na_row]
  unique_surface[i - 1] <- col[1]
}

# (5) Add the data to your dataframe:

# Add the unique values to the "data" dataframe
data$seabottom_SAL <- unique_deepest
data$seasurface_SAL <- unique_surface

View(data)

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# (2.2.) Salinity (SAL) REANALYSIS
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

repo <- paste0(output_data, "/cmems")

# Extract data: repeating the process:
SAL_reanalysis <- cmems3dmat(lon=data$lon, lat=data$lat, date=data$date, productid=5, repo=repo) #each column name is the date of the observation: e.g. as.Date(18691, origin = "1970-01-01") = 2021-05-03
head(SAL_reanalysis)

# Save data:
depth_layer <- as.numeric(dimnames(SAL_reanalysis)[[1]])
all_columns <- list()
for (i in 1:ncol(SAL_reanalysis)) {
  all_columns[[i]] <- SAL_reanalysis[, i]
}

col_names <- colnames(SAL_reanalysis)
names(all_columns) <- as.Date(as.numeric(col_names), origin = "1970-01-01")
SAL_reanalysis_df <- data.frame(depth_layer, all_columns)
head(SAL_reanalysis_df)

unique_deepest <- vector("numeric", length(SAL_reanalysis_df) - 1)
unique_surface <- vector("numeric", length(SAL_reanalysis_df) - 1)

for (i in 2:length(SAL_reanalysis_df)) {
  col <- SAL_reanalysis_df[[i]]
  last_non_na_row <- max(which(!is.na(col)))
  
  unique_deepest[i - 1] <- col[last_non_na_row]
  unique_surface[i - 1] <- col[1]
}

# Add the unique values to the "data" dataframe
data$seabottom_SAL_reanalysis <- unique_deepest
data$seasurface_SAL_reanalysis <- unique_surface

View(data)

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# (2.3.) Eliminate data for the analysis and reanalysis period that are not included in the netCDF:
##- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Define the date ranges
date_range_anfc_start <- as.POSIXct("2021-09-25", format = "%Y-%m-%d", tz="UTC")
date_range_anfc_end <- as.POSIXct("2022-07-01", format = "%Y-%m-%d", tz="UTC")

date_range_rean_start <- as.POSIXct("2020-12-01", format = "%Y-%m-%d", tz="UTC")
date_range_rean_end <- as.POSIXct("2021-06-30", format = "%Y-%m-%d", tz="UTC")

# Filter and replace values for seabottom_o2_reanalysis and seasurface_o2_reanalysis
data$seabottom_SAL_reanalysis <- ifelse(data$date >= date_range_rean_start & data$date <= date_range_rean_end, data$seabottom_SAL_reanalysis, NA)
data$seasurface_SAL_reanalysis <- ifelse(data$date >= date_range_rean_start & data$date <= date_range_rean_end, data$seasurface_SAL_reanalysis, NA)

# Filter and replace values for seasurface_o2 and seabottom_o2
data$seasurface_SAL <- ifelse(data$date >= date_range_anfc_start & data$date <= date_range_anfc_end, data$seasurface_SAL, NA)
data$seabottom_SAL <- ifelse(data$date >= date_range_anfc_start & data$date <= date_range_anfc_end, data$seabottom_SAL, NA)

View(data)

#Merge analysis and reanalysis data:
data$SSSAL_merged <- ifelse(is.na(data$seasurface_SAL_reanalysis), data$seasurface_SAL, data$seasurface_SAL_reanalysis)
data$SBSAL_merged <- ifelse(is.na(data$seabottom_SAL), data$seabottom_SAL_reanalysis, data$seabottom_SAL)
View(data)

#Calculate the difference in o2 between sea surface and bottom:
data$diff_SSSAL_SBSAL <- data$SSSAL_merged - data$SBSAL_merged
View(data)

data <- data[, !names(data) %in% c("seasurface_SAL_reanalysis", "seasurface_SAL", "seabottom_SAL_reanalysis", "seabottom_SAL")]
names(data)

#------------------------------------------------------------------------------------------------------------------------------------------------

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# (3.1) pH ANALYSIS 
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#Extract data:
ph_analysis <- cmems3dmat(lon=data$lon, lat=data$lat, date=data$date, productid=8, repo=repo) #each column name is the date of the observation: e.g. as.Date(18691, origin = "1970-01-01") = 2021-05-03
head(ph_analysis)

#First re-arrange the extracted data set by: 
# (1) Make list of each of the columns you will use to create the dataframe:
#Create a column for depth categories:
depth_layer <- as.numeric(dimnames(ph_analysis)[[1]])

# Initialize a list to store the selected columns
all_columns_ph <- list()
# Loop through each column and select all values
for (i in 1:ncol(ph_analysis)) {
  all_columns_ph[[i]] <- ph_analysis[, i]
}

# (2) Change the name of the columns to date format to understand better which dates are outside the range of the particular
# netCDF you are obtaining the data from. In my case half of the data comes from Reanalysis netCDF and part from Analysis.

# Generate a sequence of names regarding to the dates
col_names <- colnames(ph_analysis)
# Assign the names to the list
names(all_columns_ph) <- as.Date(as.numeric(col_names), origin = "1970-01-01")

# (3) Create a dataframe with 1 column regarding to depth and the rest to each of the points (3078 points):

# Create a new DataFrame with the "depth_layer" and "o2_level" columns
ph_analysis_df <- data.frame(depth_layer, all_columns_ph)
head(ph_analysis_df)
#View(ph_analysis_df)

# (4) Select the first and last datum regarding to the shallowest (surface) and deepest possition within each location point (lon, lat, depth)

# Initialize empty vectors to store the unique deepest and surface values
unique_deepest <- vector("numeric", length(ph_analysis_df) - 1)
unique_surface <- vector("numeric", length(ph_analysis_df) - 1)

# Loop through the columns of ph_analysis_df starting from the second column (index 2)
for (i in 2:length(ph_analysis_df)) {
  col <- ph_analysis_df[[i]]
  last_non_na_row <- max(which(!is.na(col)))
  
  # Store the unique deepest and surface values in the respective vectors
  unique_deepest[i - 1] <- col[last_non_na_row]
  unique_surface[i - 1] <- col[1]
}

# (5) Add the data to your dataframe:

# Add the unique values to the "data" dataframe
data$seabottom_ph <- unique_deepest
data$seasurface_ph <- unique_surface

View(data)

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# (3.2.) ph REANALYSIS
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

repo <- paste0(output_data, "/cmems")

# Extract data: repeating the process:
ph_reanalysis <- cmems3dmat(lon=data$lon, lat=data$lat, date=data$date, productid=7, repo=repo) #each column name is the date of the observation: e.g. as.Date(18691, origin = "1970-01-01") = 2021-05-03
head(ph_reanalysis)

# Save data:
depth_layer <- as.numeric(dimnames(ph_reanalysis)[[1]])
all_columns <- list()
for (i in 1:ncol(ph_reanalysis)) {
  all_columns[[i]] <- ph_reanalysis[, i]
}

col_names <- colnames(ph_reanalysis)
names(all_columns) <- as.Date(as.numeric(col_names), origin = "1970-01-01")
ph_reanalysis_df <- data.frame(depth_layer, all_columns)
head(ph_reanalysis_df)

unique_deepest <- vector("numeric", length(ph_reanalysis_df) - 1)
unique_surface <- vector("numeric", length(ph_reanalysis_df) - 1)

for (i in 2:length(ph_reanalysis_df)) {
  col <- ph_reanalysis_df[[i]]
  last_non_na_row <- max(which(!is.na(col)))
  
  unique_deepest[i - 1] <- col[last_non_na_row]
  unique_surface[i - 1] <- col[1]
}

# Add the unique values to the "data" dataframe
data$seabottom_ph_reanalysis <- unique_deepest
data$seasurface_ph_reanalysis <- unique_surface

View(data)

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# (3.3.) Eliminate data for the analysis and reanalysis period that are not included in the netCDF:
##- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Define the date ranges
date_range_anfc_start <- as.POSIXct("2021-06-02", format = "%Y-%m-%d", tz="UTC")
date_range_anfc_end <- as.POSIXct("2022-07-01", format = "%Y-%m-%d", tz="UTC")

date_range_rean_start <- as.POSIXct("2020-12-01", format = "%Y-%m-%d", tz="UTC")
date_range_rean_end <- as.POSIXct("2021-06-01", format = "%Y-%m-%d", tz="UTC")

# Filter and replace values for seabottom_o2_reanalysis and seasurface_o2_reanalysis
data$seabottom_ph_reanalysis <- ifelse(data$date >= date_range_rean_start & data$date <= date_range_rean_end, data$seabottom_ph_reanalysis, NA)
data$seasurface_ph_reanalysis <- ifelse(data$date >= date_range_rean_start & data$date <= date_range_rean_end, data$seasurface_ph_reanalysis, NA)

# Filter and replace values for seasurface_o2 and seabottom_o2
data$seasurface_ph <- ifelse(data$date >= date_range_anfc_start & data$date <= date_range_anfc_end, data$seasurface_ph, NA)
data$seabottom_ph <- ifelse(data$date >= date_range_anfc_start & data$date <= date_range_anfc_end, data$seabottom_ph, NA)

View(data)

#Merge analysis and reanalysis data:
data$SSph_merged <- ifelse(is.na(data$seasurface_ph_reanalysis), data$seasurface_ph, data$seasurface_ph_reanalysis)
data$SBph_merged <- ifelse(is.na(data$seabottom_ph), data$seabottom_ph_reanalysis, data$seabottom_ph)
View(data)

#Calculate the difference in o2 between sea surface and bottom:
data$diff_SSph_SBph <- data$SSph_merged - data$SBph_merged
View(data)

data <- data[, !names(data) %in% c("seasurface_ph_reanalysis", "seasurface_ph", "seabottom_ph_reanalysis", "seabottom_ph")]
names(data)

#------------------------------------------------------------------------------------------------------------------------------------------------

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# (4.1) Temperature ANALYSIS 
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#Extract data:
temp_analysis <- cmems3dmat(lon=data$lon, lat=data$lat, date=data$date, productid=2, repo=repo) #each column name is the date of the observation: e.g. as.Date(18691, origin = "1970-01-01") = 2021-05-03
head(temp_analysis)

#First re-arrange the extracted data set by: 
# (1) Make list of each of the columns you will use to create the dataframe:
#Create a column for depth categories:
depth_layer <- as.numeric(dimnames(temp_analysis)[[1]])

# Initialize a list to store the selected columns
all_columns_temp <- list()
# Loop through each column and select all values
for (i in 1:ncol(temp_analysis)) {
  all_columns_temp[[i]] <- temp_analysis[, i]
}

# (2) Change the name of the columns to date format to understand better which dates are outside the range of the particular
# netCDF you are obtaining the data from. In my case half of the data comes from Reanalysis netCDF and part from Analysis.

# Generate a sequence of names regarding to the dates
col_names <- colnames(temp_analysis)
# Assign the names to the list
names(all_columns_temp) <- as.Date(as.numeric(col_names), origin = "1970-01-01")

# (3) Create a dataframe with 1 column regarding to depth and the rest to each of the points (3078 points):

# Create a new DataFrame with the "depth_layer" and "o2_level" columns
temp_analysis_df <- data.frame(depth_layer, all_columns_temp)
head(temp_analysis_df)
View(temp_analysis_df)
column_values <- temp_analysis_df$X2021.06.22.1

# (4) Select the first and last datum regarding to the shallowest (surface) and deepest position within each location point (lon, lat, depth)

# Initialize empty vectors to store the unique deepest and surface values
unique_deepest <- vector("numeric", length(temp_analysis_df) - 1)
unique_surface <- vector("numeric", length(temp_analysis_df) - 1)

# Loop through the columns of temp_analysis_df starting from the second column (index 2)
for (i in 2:length(temp_analysis_df)) {
  col <- temp_analysis_df[[i]]
  last_non_na_row <- max(which(!is.na(col)))
  
  # Store the unique deepest and surface values in the respective vectors
  unique_deepest[i - 1] <- col[last_non_na_row]
  unique_surface[i - 1] <- col[1]
}

# (5) Add the data to your dataframe:

# Add the unique values to the "data" dataframe
data$seabottom_temp <- unique_deepest
data$seasurface_temp <- unique_surface

View(data)

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# (4.2.) Temperature REANALYSIS
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

repo <- paste0(output_data, "/cmems")

# Extract data: repeating the process:
temp_reanalysis <- cmems3dmat(lon=data$lon, lat=data$lat, date=data$date, productid=1, repo=repo) #each column name is the date of the observation: e.g. as.Date(18691, origin = "1970-01-01") = 2021-05-03
head(temp_reanalysis)

# Save data:
depth_layer <- as.numeric(dimnames(temp_reanalysis)[[1]])
all_columns <- list()
for (i in 1:ncol(temp_reanalysis)) {
  all_columns[[i]] <- temp_reanalysis[, i]
}

col_names <- colnames(temp_reanalysis)
names(all_columns) <- as.Date(as.numeric(col_names), origin = "1970-01-01")
temp_reanalysis_df <- data.frame(depth_layer, all_columns)
head(temp_reanalysis_df)

unique_deepest <- vector("numeric", length(temp_reanalysis_df) - 1)
unique_surface <- vector("numeric", length(temp_reanalysis_df) - 1)

for (i in 2:length(temp_reanalysis_df)) {
  col <- temp_reanalysis_df[[i]]
  last_non_na_row <- max(which(!is.na(col)))
  
  unique_deepest[i - 1] <- col[last_non_na_row]
  unique_surface[i - 1] <- col[1]
}

# Add the unique values to the "data" dataframe
data$seabottom_temp_reanalysis <- unique_deepest
data$seasurface_temp_reanalysis <- unique_surface

View(data)

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# (4.3.) Eliminate data for the analysis and reanalysis period that are not included in the netCDF:
##- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Define the date ranges
date_range_rean_start <- as.POSIXct("2021-07-01", format = "%Y-%m-%d", tz="UTC")
date_range_rean_end <- as.POSIXct("2022-07-01", format = "%Y-%m-%d", tz="UTC")

date_range_anfc_start <- as.POSIXct("2020-12-01", format = "%Y-%m-%d", tz="UTC")
date_range_anfc_end <- as.POSIXct("2021-06-30", format = "%Y-%m-%d", tz="UTC")

# Filter and replace values for seabottom_o2_reanalysis and seasurface_o2_reanalysis
data$seabottom_temp_reanalysis <- ifelse(data$date >= date_range_rean_start & data$date <= date_range_rean_end, data$seabottom_temp_reanalysis, NA)
data$seasurface_temp_reanalysis <- ifelse(data$date >= date_range_rean_start & data$date <= date_range_rean_end, data$seasurface_temp_reanalysis, NA)

# Filter and replace values for seasurface_o2 and seabottom_o2
data$seasurface_temp <- ifelse(data$date >= date_range_anfc_start & data$date <= date_range_anfc_end, data$seasurface_temp, NA)
data$seabottom_temp <- ifelse(data$date >= date_range_anfc_start & data$date <= date_range_anfc_end, data$seabottom_temp, NA)

View(data)

#Merge analysis and reanalysis data:
data$SST_merged <- ifelse(is.na(data$seasurface_temp_reanalysis), data$seasurface_temp, data$seasurface_temp_reanalysis)
data$SBT_merged <- ifelse(is.na(data$seabottom_temp), data$seabottom_temp_reanalysis, data$seabottom_temp)
View(data)

#Calculate the difference in o2 between sea surface and bottom:
data$diff_SST_SBT <- data$SST_merged - data$SBT_merged
data$diff_SST_at <- data$SST_merged - data$SBT_merged
View(data)

data <- data[, !names(data) %in% c("seasurface_temp_reanalysis", "seasurface_temp", "seabottom_temp_reanalysis", "seabottom_temp")]
names(data)


#------------------------------------------------------------------------------------------------------------------------------------------------

#Save dataset
output_file <- file.path(output_data, "data_env_all.csv")
write.csv2(data, file = output_file, row.names = FALSE)
