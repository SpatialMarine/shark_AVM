#------------------------------------------------------------------------------------
# 3_enviroERA5.R    Download ERA5 data      
#------------------------------------------------------------------------------------
#Select unique values from your dates
setwd(output_data)
data <- read.csv("dataset_rm_var.csv", sep = ";")
names(data)
dates<- paste0(data$time, sep = " ", data$hour)
date<-unique(dates)
Days <- as.data.frame(date)

# Convert "date" column to date format
Days$date <- as.POSIXct(Days$date, format = "%Y-%m-%d %H:%M")
Days

#Divide the date in different columns as requested by ERA5 code:
# Extract year, month, day and hour components each in a column:
Days$year <- as.numeric(format(Days$date, "%Y"))
Days$month <- as.numeric(format(Days$date, "%m"))
Days$day = as.numeric(format(Days$date, "%d"))  
Days$hour = format(Days$date, "%H:%M") 
View(Days)

# Define the name of the file and the destination
destination_folder <- paste0(output_data, "/ERA5/AT_Reanalysis")
wd<-paste0(main_dir)
setwd(wd)

#If you have several parameters to download from ERA5, you can use the catalog instead and
# generate nested loops  to proceed with the download of all of them at the same time.
#catalogERA <- read.csv("Catalog_ERA5.csv", sep=";")
#head(catalogERA)

wf_set_key(user = myUID, key = myAPI, service = "cds")

for(i in 1:nrow(Days)){ 
  
  print(paste("Processing product", i))
  
  # Create folders for different dates inside the ERA5 folder
  date_dir <- file.path(destination_folder, paste0("AT_renalysis_", format(Days$date[i], "%Y-%m-%d_%H_%M")))
  if (!file.exists(date_dir)) {
    dir.create(date_dir, recursive = TRUE)}
  # Define the file name using the current date
  file_name <- paste0("AT_renalysis_", format(Days$date[i], "%Y-%m-%d_%H_%M"), ".nc")
  
  # Loop through each row of the "Days" data frame
  request <- list(
    dataset_short_name = "reanalysis-era5-single-levels",
    product_type = "reanalysis",
    format = "netcdf",
    variable = "2m_temperature",
    year = Days$year[i],
    month = Days$month[i],
    day = Days$day[i], 
    time = Days$hour[i],  
    area = c(43, 4, -2, 36),
    target = file_name
  )
  
  file <- wf_request(user = myUID,
                     request = request,
                     transfer = TRUE,
                     path = date_dir,
                     verbose = TRUE)
  }

# Create an empty list to store the rasters
raster_list <- list()

# List all the folders containing the raster files
all_date_dirs <- list.dirs(destination_folder, full.names = TRUE, recursive = FALSE)

# Loop through each folder and read the raster file
for (date_dir in all_date_dirs) {
  # List all the .nc files in the folder
  nc_files <- list.files(date_dir, pattern = "\\.nc$", full.names = TRUE)
  
  # Read the .nc file into a raster stack and append it to the raster_list
  raster_list <- c(raster_list, brick(nc_files))
}

# Merge all the raster bricks into a single 3-dimensional brick
merged_brick <- brick(raster_list)
print(merged_brick)
plot(merged_brick)

# Now you have a single brick object with all the rasters merged
# Save the merged brick to a new file
output_file <- paste0(output_data, "/ERA5/AT_Reanalysis/brick_AT_Reanalysis_StudyDates.nc")
writeRaster(merged_brick, filename = output_file, format="CDF", overwrite=TRUE)

AT_Reanalysis_StudyDates<-nc_open(output_data, "/ERA5/AT_Reanalysis/brick_AT_Reanalysis_StudyDates.nc")
print(AT_Reanalysis_StudyDates)
