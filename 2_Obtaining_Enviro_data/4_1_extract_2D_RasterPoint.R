#------------------------------------------------------------------------------------
# 4_1_extract_2D_RasterPoint.R     Extract 2D data from raster to points   
#------------------------------------------------------------------------------------

#Load data
setwd(output_data)
data <- read.csv("dataset_rm_var.csv", sep = ";") #remember having date format in your .csv (actively change it)
names(data)
head(data)

# explore temporal and spatial range
# use same temporal resolution (day) and numeric for lon and lat
data$time <- as.Date(data$time) #if your time scale has not hours
data$time_hours <- as.POSIXct(data$time_hours, format = "%d-%m-%y %H:%M", tz = "UTC")
data$lon <- as.numeric(gsub(",", ".", data$lon))
data$lat <- as.numeric(gsub(",", ".", data$lat))
range(data$time)
range(data$time_hours)
range(data$lon)
range(data$lat)

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Load all the netCDF and rasters.
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# (1) at_reanalysis:
wd<-paste0(output_data, "/ERA5/AT_reanalysis")
setwd(wd)

#Check parameters:
nc<- nc_open("AT_2022.nc")
nclon <- nc$dim$lon$vals#ncvar_get(nc, varid="lon") # nc$dim$lon$vals => same or faster?
nclat <- nc$dim$lat$vals#ncvar_get(nc, varid="lat") 
nctime <- nc$dim$time$vals
ncday <-  as.numeric(as.POSIXct(nctime, origin = "1970-01-01 00:00:00", tz = "UTC"))
reference_time <- as.POSIXct("1900-01-01 00:00:00", tz = "UTC")
date_times <- reference_time + ncday * 3600  # Assuming values represent hours

#Open brick:
AT_2022<- brick("AT_2022.nc")
AT_2021<- brick("AT_2021.nc")
AT_2020<- brick("AT_2020.nc")

# (2) sbt_reanalysis
wd<-paste0(output_data, "/cmems/MEDSEA_MULTIYEAR_PHY_006_004/med-cmcc-tem-rean-d")
setwd(wd)

#Check parameters:
nc<- nc_open("SBT_Reanalysis_01-12-2020_30-06-2022.nc")
nclon <- nc$dim$lon$vals#ncvar_get(nc, varid="lon") # nc$dim$lon$vals => same or faster?
nclat <- nc$dim$lat$vals#ncvar_get(nc, varid="lat") 
nctime <- nc$dim$time$vals
ncday <-  as.numeric(as.POSIXct(nctime, origin = "1970-01-01 00:00:00", tz = "UTC"))
reference_time <- as.POSIXct("1900-01-01", tz = "UTC")
date_times <- reference_time + as.difftime(nctime, units = "mins")
#Open brick:
sbt_reanalysis<- brick("SBT_Reanalysis_01-12-2020_30-06-2022.nc")

# (3) sst_reanalysis
wd<-paste0(output_data, "/ERA5/SST_reanalysis")
setwd(wd)

#Check parameters:
nc<- nc_open("SST_2022.nc")
nclon <- nc$dim$lon$vals#ncvar_get(nc, varid="lon") # nc$dim$lon$vals => same or faster?
nclat <- nc$dim$lat$vals#ncvar_get(nc, varid="lat") 
nctime <- nc$dim$time$vals
ncday <-  as.numeric(as.POSIXct(nctime, origin = "1970-01-01 00:00:00", tz = "UTC"))
reference_time <- as.POSIXct("1900-01-01 00:00:00", tz = "UTC")
date_times <- reference_time + ncday * 3600  # Assuming values represent hours
max(date_times)

#Open brick:
SST_2022<- brick("SST_2022.nc")
SST_2021<- brick("SST_2021.nc")
SST_2020<- brick("SST_2020.nc")

# (4) Bathymetry (depth)
wd<-paste0(output_data, "/EMODnet/Bathy")
setwd(wd)
bathy<- raster("Bathy.tif")
bathy
plot(bathy)

# (5) Substrate
wd<-paste0(output_data, "/EMODnet/Subs")
setwd(wd)
subs<- raster("Sed_Raster.tif")
subs
plot(subs)

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Configure time format for each netCDF and raster
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#at_reanalysis
time <- getZ(AT_2020)
time_hours <- as.POSIXct(time, origin = "1900-01-01", tz = "UTC") 
AT_2020 <- setZ(AT_2020, z = time_hours)
AT_2020

time <- getZ(AT_2021)
time_hours <- as.POSIXct(time, origin = "1900-01-01", tz = "UTC") 
AT_2021 <- setZ(AT_2021, z = time_hours)
AT_2021

time <- getZ(AT_2022)
time_hours <- as.POSIXct(time, origin = "1900-01-01", tz = "UTC") 
AT_2022 <- setZ(AT_2022, z = time_hours)
AT_2022

#sbt_reanalysis
time <- getZ(sbt_reanalysis)
#time <- as.POSIXct(time*60, origin = "1900-01-01", tz = "UTC") 
days <- as.Date(time)
sbt_reanalysis <- setZ(sbt_reanalysis, z = days)
sbt_reanalysis

#sst_reanalysis
time <- getZ(SST_2020)
time_hours <- as.POSIXct(time, origin = "1900-01-01", tz = "UTC") 
SST_2020 <- setZ(SST_2020, z = time_hours)
SST_2020

time <- getZ(SST_2021)
time_hours <- as.POSIXct(time, origin = "1900-01-01", tz = "UTC") 
SST_2021 <- setZ(SST_2021, z = time_hours)
SST_2021

time <- getZ(SST_2022)
time_hours <- as.POSIXct(time, origin = "1900-01-01", tz = "UTC") 
SST_2022 <- setZ(SST_2022, z = time_hours)
SST_2022

# create new extract function
extractTSR <- function(x, y, t){
  
  # get time from raster
  xtime <- raster::getZ(x)
  
  # match point time with raster
  # returns index from multilayer
  idx <- match(t, xtime)
  
  # extract data for all points from all layers
  ex <- raster::extract(x, y)
  
  # for each data point, select the data for idx
  dat <- ex[cbind(1:length(t), idx)]
  return(dat)
}

#sbt_reanalysis
data$sbt_reanalysis <- extractTSR(x = sbt_reanalysis, y = cbind(data$lon, data$lat), t = data$time)
View(data)

#sst_reanalysis
data$sst_reanalysis2020 <- extractTSR(x = SST_2020, y = cbind(data$lon, data$lat), t = data$time_hours)
data$sst_reanalysis2021 <- extractTSR(x = SST_2021, y = cbind(data$lon, data$lat), t = data$time_hours)
data$sst_reanalysis2022 <- extractTSR(x = SST_2022, y = cbind(data$lon, data$lat), t = data$time_hours)
data <- data %>%
  mutate(sst_reanalysis = coalesce(sst_reanalysis2020, sst_reanalysis2021, sst_reanalysis2022))
data <- data[, !names(data) %in% c("sst_reanalysis2020", "sst_reanalysis2021", "sst_reanalysis2022")]
View(data)

#at_reanalysis
data$at_reanalysis2020 <- extractTSR(x = AT_2020, y = cbind(data$lon, data$lat), t = data$time_hours)
data$at_reanalysis2021 <- extractTSR(x = AT_2021, y = cbind(data$lon, data$lat), t = data$time_hours)
data$at_reanalysis2022 <- extractTSR(x = AT_2022, y = cbind(data$lon, data$lat), t = data$time_hours)
data <- data %>%
  mutate(at_reanalysis = coalesce(at_reanalysis2020, at_reanalysis2021, at_reanalysis2022))
data <- data[, !names(data) %in% c("at_reanalysis2020", "at_reanalysis2021", "at_reanalysis2022")]
View(data)

#Transform from Fahrenheit degrees to Celsius degrees:
data$at_celsius <- data$at_reanalysis - 273.15
data$sst_celsius <- data$sst_reanalysis - 273.15
data <- data[, !names(data) %in% c("at_reanalysis", "sst_reanalysis")]
View(data)

#Bathy
data$bathy <- raster::extract(bathy, cbind(data$lon, data$lat)) #, buffer = 15000, fun = mean)
data$bathy <- abs(data$bathy)
View(data)

#subs
data$subs <- raster::extract(subs, cbind(data$lon, data$lat)) #, buffer = 15000, fun = mean)
data$subs <- abs(data$subs)
View(data)

#Calculate differences:
data$diff_sst_sbt <- data$sst_celsius - data$sbt_reanalysis
data$diff_at_sbt <- data$at_celsius - data$sbt_reanalysis
View(data)

#Save dataset
output_file <- file.path(output_data, "data_env.csv")
write.csv2(data, file = output_file, row.names = FALSE)
