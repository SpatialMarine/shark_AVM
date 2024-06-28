#-----------------------------------------------------------------
#  6_check_missingData     Check missing data in predictors
#-----------------------------------------------------------------
sp_code <- "Sca" #replace by the file name: Sca, Gme, Esp, Tma

#Load data
wd<- paste0(output_data, "/data_subsets")
setwd(wd)
data <- read.csv("Scanicula.csv", sep = ";") #replace by the file name: Scanicula #Gmelastomus #Espinax #Tmarmorata
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

#Check that it all is correct:
summary(data)

#Check data particularities
#How many individuals and trips appear in this dataset?
#indiv <- unique(data$organismID) #1430
#trip <- unique(data$tripID) #53

#---------------------------------------------------------------------------------------------------
# Select predictors            
#---------------------------------------------------------------------------------------------------
#Create function for plot how many missing values are there in a particular column (taken from dmarch github: https://github.com/dmarch/agazella):
plot_Missing <- function(data_in, title = NULL){
  # https://www.kaggle.com/notaapple/detailed-exploratory-data-analysis-using-r
  temp_df <- as.data.frame(ifelse(is.na(data_in), 0, 1))
  temp_df <- temp_df[,order(colSums(temp_df))]
  data_temp <- expand.grid(list(x = 1:nrow(temp_df), y = colnames(temp_df)))
  data_temp$m <- as.vector(as.matrix(temp_df))
  data_temp <- data.frame(x = unlist(data_temp$x), y = unlist(data_temp$y), m = unlist(data_temp$m))
  ggplot(data_temp) + geom_tile(aes(x=x, y=y, fill=factor(m))) + scale_fill_manual(values=c("grey80", "grey10"), name="Missing\n(0=Yes, 1=No)") + theme_light() + ylab("") + xlab("") + ggtitle(title)
}

# set names of the environmental variables
vars <- c("TL", "Sex", "WeightLWR", "Maturity", "Sea.state", "Wind.strength", "Cloud.cover", "Distance_covered_GPS", "Average_speed",                  
          "Trawl_duration", "MinsExposedtoAir", "at_celsius", "sst_celsius", "sbt_reanalysis", "diff_sst_sbt", "diff_at_sbt", 
          "depth", "subs", "TotalBiomassHaul")

#"Species", "tripID", "lat", "lon", "organismID", "Haul_N", "DW","Activity", "Wunds", "Brusing", "Ectoparasites", "Pregnancy", "Abortion", 
#"Metier","Net_horizontal_opening","date", "time_hours", 
#"sso_merged", "sbo_merged", "diff_sso_sbo", "SSSAL_merged", "SBSAL_merged", "diff_SSSAL_SBSAL", 
#"SSph_merged", "SBph_merged", "diff_SSph_SBph",

# Select columns with environmental data
selEnv <- data %>% dplyr::select(all_of(vars))
# Plot missing data for environmental variables and save it:
#Create folder to save it:
output_dir <- file.path(output_data, paste0("prefitting_checks/", sp_code))
if (!file.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)}

#Plot and save:
setwd(output_dir)
jpeg(file = "MissingData.jpeg", 
     width = 23.8, height = 21.65, units = "cm", res = 300)
plot_Missing(selEnv) #there are no data in some points.
dev.off()

#Calculate percentage of NA in your predictors:
#TotalBiomass:
na_count <- sum(is.na(data$TotalBiomassHaul))
na_count
total_observations <- length(data$TotalBiomassHaul)
percentage <- (na_count / total_observations) * 100
percentage #16.15% NA for TotalBiomassHaul Sca; 23.01% for Gme; 13.18 for Esp; 15.93% for Tma

#SAL (diff_SSSAL_SBSAL; SBSAL_merged; SSSAL_merged):
#na_count <- sum(is.na(data$diff_SSSAL_SBSAL))
#na_count
#total_observations <- length(data$diff_SSSAL_SBSAL)
#percentage <- (na_count / total_observations) * 100
#percentage #10.49% NA for all SAL (diff_SSSAL_SBSAL; SBSAL_merged; SSSAL_merged) Sca; 11.80% for Gme ; 11.63% for Esp; 0.88% for Tma
