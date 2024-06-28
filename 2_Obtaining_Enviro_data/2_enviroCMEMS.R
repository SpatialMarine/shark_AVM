#------------------------------------------------------------------------------------
# 2_enviroCMEMS.R    Download CMEMS data      
#------------------------------------------------------------------------------------

# Define the name of the file and the destination
destination_folder <- paste0(output_data, "/cmems")

# Import data catalog
setwd(main_dir)
catalog <- read.csv("Catalog_CMEMS_2.csv", sep=";")
head(catalog)

# Create vector dates
Days <- data.frame(dateOnly_min = catalog$date_min, dateOnly_max = catalog$date_max) 
Days$date_min <- paste0(Days$dateOnly_min, sep=" ", "12:00:00")
Days$date_max <- paste0(Days$dateOnly_max, sep=" ", "12:00:00")
Days$dateOnly_min <- as.Date(Days$dateOnly_min)
Days$dateOnly_max <- as.Date(Days$dateOnly_max)
Days$date_min <- as.POSIXct(Days$date_min, format="%Y-%m-%d %H:%M:%S")
Days$date_max <- as.POSIXct(Days$date_max, format="%Y-%m-%d %H:%M:%S")
print(Days)
str(Days)

### Download data

for(i in 1:nrow(catalog)){ 
  
  print(paste("Processing product", i))
  
  # Create the folder for each product if it doesn't exist already 
  dir_path <- file.path(destination_folder, catalog$service[i], catalog$layer[i])
  if (!file.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)}
  
  ##If you need a folder per each date:
#  for(j in 1:nrow(days_table)){
#    print(paste("Processing date", j))
  
# Create folders for different dates inside the variable folders
#    date_dir <- file.path(dir_path, days_table$Date_only[j])
#    if (!file.exists(date_dir)) {
#    dir.create(date_dir, recursive = TRUE)}
  
# Define the file name using the current date
  file_name <- paste0(catalog$var_name[i], "_", format(as.Date(Days$dateOnly_min[i], origin = "01-01-1970"), "%d-%m-%Y"), "_", format(as.Date(Days$dateOnly_max[i], origin = "1970-01-01"), "%d-%m-%Y"), ".nc")
  
# Define the full file path including the file name
  file_path <- file.path(dir_path, file_name)
  
# start the download (See https://github.com/pepijn-devries/CopernicusMarine - potential updates may be needed, currently only Python works)
  cms_download_subset(username = username,
                           password = password,
                           destination = file_path,
                           product = catalog$service[i],
                           layer = catalog$layer[i],
                           variable = catalog$variable[i],
                           output = "netcdf",
                           region = c(catalog$xmin[i], catalog$ymin[i], catalog$xmax[i], catalog$ymax[i]),
                           timerange = c(Days$dateOnly_min[i], Days$dateOnly_max[i]),
                           verticalrange = c(0, 800),
                           sub_variables = catalog$subvar[i],
                           overwrite = TRUE)
}
