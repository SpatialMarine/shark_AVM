#---------------------------------------------------------------------------------------------------
# 11_set_folds.R      Fit the Boosted Regression Tree model (BRT; frequently used as species distribution model)
#---------------------------------------------------------------------------------------------------
# As mentioned previously, I will use all data to fit the model and check the model using cross-validation,
# instead of a training and testing data sets.

sp_code <- "Sca" #Sca, Gme, Esp, Tma

#Load data
wd<- paste0(output_data, "/data_subsets")
setwd(wd)
data <- read.csv("Scanicula.csv", sep = ";") #Scanicula #Gmelastomus #Espinax #Tmarmorata
names(data)
head(data)

#Set categorical predictors as categories:
data <- data %>% 
  mutate(Haul_N = factor(data$Haul_N),
         Alive_Dead = factor(Alive_Dead, c(0, 1)),
         Sex = factor(Sex, c(0, 1)),
         subs = factor(subs, c(1:2)),
         Season = factor(Season, c(1:4)),
         Maturity = factor(Maturity, c(0:1)),
         Pregnancy = factor(Pregnancy, c(0:1)),
         Abortion = factor(Abortion, c(0:1)))

#Set categorical predictors as categories:
data <- data %>%
  mutate(TL = as.numeric(gsub(",", ".", TL)),  # Convert comma decimal to dot decimal
         DW = as.numeric(gsub(",", ".", DW)),
         lon = as.numeric(gsub(",", ".", lon)),
         lat = as.numeric(gsub(",", ".", lat)),
         Cloud.cover = as.numeric(gsub(",", ".", Cloud.cover)),
         MinsExposedtoAir = as.numeric(gsub(",", ".", MinsExposedtoAir)),
         Average_speed = as.numeric(gsub(",", ".", Average_speed)),
         TotalBiomassHaul = as.numeric(gsub(",", ".", TotalBiomassHaul)),
         Trawl_duration = as.numeric(gsub(",", ".", Trawl_duration)),
         Wind.strength = as.numeric(gsub(",", ".", Wind.strength)),
         Sea.state = as.numeric(gsub(",", ".", Sea.state)),
         Distance_covered_GPS = as.numeric(gsub(",", ".", Distance_covered_GPS)),
         Activity = as.numeric(gsub(",", ".", Activity)),
         Wunds = as.numeric(gsub(",", ".", Wunds)),
         Brusing = as.numeric(gsub(",", ".", Brusing)),
         Ectoparasites = as.numeric(gsub(",", ".", Ectoparasites)),
         at_celsius = as.numeric(gsub(",", ".", at_celsius)),
         sst_celsius = as.numeric(gsub(",", ".", sst_celsius)),
         sbt_reanalysis = as.numeric(gsub(",", ".", sbt_reanalysis)),
         diff_at_sbt = as.numeric(gsub(",", ".", diff_at_sbt)),
         diff_sst_sbt = as.numeric(gsub(",", ".", diff_sst_sbt)),
         depth = as.numeric(gsub(",", ".", depth)),
         #sso_merged = as.numeric(gsub(",", ".", sso_merged)),
         #sbo_merged = as.numeric(gsub(",", ".", sbo_merged)),
         #diff_sso_sbo = as.numeric(gsub(",", ".", diff_sso_sbo)),
         #SSSAL_merged = as.numeric(gsub(",", ".", SSSAL_merged)),
         #SBSAL_merged = as.numeric(gsub(",", ".", SBSAL_merged)),
         #diff_SSSAL_SBSAL = as.numeric(gsub(",", ".", diff_SSSAL_SBSAL)),
         #SSph_merged = as.numeric(gsub(",", ".", SSph_merged)),
         #SBph_merged = as.numeric(gsub(",", ".", SBph_merged)),
         #diff_SSph_SBph = as.numeric(gsub(",", ".", diff_SSph_SBph))
  )

# Convert the 'time' column to Date format if needed 
data$date <- as.Date(data$date) #, format = "%Y-%m-%d"
data$time_hours <- as.POSIXct(data$time_hours, format = "%d-%m-%y %H:%M", tz = "UTC")

summary(data)
str(data)

# Generate Random Number (from 1 to 100) and an ID column for each row (i.e. specimen)
# RN will serve as an indicator for variables that have influence greater or less than 
# random (Scales et al., 2017; Soykan, Eguchi, Kohin, & Dewar, 2014);
# only variables with influence greater than the random number were included in the final models.

data$RN <- sample.int(100, size=nrow(data), replace=T, prob=NULL)
data <- data %>%
  mutate(id = seq_along(RN))
head(data)

# Prepare folds
#* set number of folds
#* Create them based on the previously created N_Haul (group variable; similar to random factor) column.
#* This is to not split the data into groups for cross-validation (the model is fitted with 9 groups in this case
#* where there are 10 folds, and then it repeated every time leaving one of the groups out and calculating a deviance)
#* Thus, if you have a variable which groups the data, something like a random effect, you should respect it
#* when doing the groups. So in my case, it is the haul, I want to merge several hauls into one fold but making sure
#* that all the data from a haul is not splitted among folds.

# Set the number of folds
n.folds <- 5

# Set the seed for reproducibility
set.seed(123)

#create folds
f <- fold(data = data, id_col = "Haul_N", method = "n_dist", k = n.folds) 

data <- f %>%
  dplyr::rename(fold = .folds) %>%
  dplyr::mutate(fold = as.numeric(fold)) %>%
  as.data.frame()

#Check that each fold has a similar number of samples/specimens (rows)
table(data$fold)

data %>%
  group_by(id, fold) %>%
  dplyr::summarize(n = n())

head(data)

#Create folder to save them:
output_dir <- file.path(output_data, paste0("folds_dataset/"))
if (!file.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)}

#Save data set:
output_file <- paste0(output_dir, "/", sp_code, "_folds_dataset.csv")
write.csv2(data, file = output_file, row.names = FALSE)

