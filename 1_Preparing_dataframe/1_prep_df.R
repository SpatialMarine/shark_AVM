#------------------------------------------------------------------------------------
# 1_prep_df    Organise your data set previously to import environmental data     
#------------------------------------------------------------------------------------

#Load data
setwd(input_data)
data <- read.csv("270621_AllData.csv", sep = ";")
names(data)
head(data)

#Extract date from the code data:
# Extract year, month, and day components from the "Code" column for ECEME codes:
data$year <- substr(data$tripID, 1, 2)
data$month <- substr(data$tripID, 4, 5)
data$day <- substr(data$tripID, 7, 8)
data$year <- paste0("20", data$year)

## Combine year, month, and day components to create the new date column
data$date <- as.Date(paste(data$year, data$month, data$day, sep = "-"), format = "%Y-%m-%d")

## Drop the intermediate columns (year, month, day) if you don't need them anymore
data <- data[, !names(data) %in% c("year", "month", "day")]
names(data)

## Use coalesce to combine the two columns while keeping non-NA values
data$time <- coalesce(data$date)

## Print the resulting data frame
print(data)

data <- data[, !names(data) %in% c("date")]
names(data)

#Add data and hour:
data$time_hours <- paste(data$time, data$hour)

#export
output_file <- file.path(output_data, "dataset_dates.csv")
write.csv2(data, file = output_file, row.names = FALSE)

#Now drop variables that you won't need anymore for the analyses you will conduct:
setwd(output_data)
data <- read.csv("dataset_dates.csv", sep = ";")
names(data)
head(data)

data <- data[, !names(data) %in% c("N_withinHaul", "Port", "Vessel", "CommentsInSurvival")]

names(data)
output_file <- file.path(output_data, "dataset_rm_var.csv")
write.csv2(data, file = output_file, row.names = FALSE)
