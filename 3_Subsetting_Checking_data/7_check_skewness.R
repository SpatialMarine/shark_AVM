#-----------------------------------------------------------------
# 7_check_skewness.R     Check skewness in predictors and log transform if needed
#-----------------------------------------------------------------
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

# Select specific columns from the data dataset in which you want to assess skewness
names(data)
varskew  <- c("TL", "Sea.state", "Wind.strength", "Cloud.cover", "Distance_covered_GPS", "Average_speed",                  
          "Trawl_duration", "MinsExposedtoAir", "at_celsius", "sst_celsius", "sbt_reanalysis", "diff_sst_sbt", "diff_at_sbt", 
          "depth", "subs", "TotalBiomassHaul") 
#"sso_merged", "sbo_merged", "diff_sso_sbo", "SSSAL_merged", "SBSAL_merged", "diff_SSSAL_SBSAL", "SSph_merged", "SBph_merged", "diff_SSph_SBph",

# Select columns with environmental data
class(varskew)
selvarskew <- data %>% dplyr::select(all_of(varskew))
head(selvarskew)

# Initialize an empty data frame for skewness table
skewness_table <- data.frame(variable = character(), skewness = numeric(), shapiro = numeric())

# Iterate over column names in selvarskew dataset
for (col_name in names(selvarskew)) {
  # Check if column is numeric
  if (is.numeric(selvarskew[[col_name]])) {
    # Original variable
    column <- selvarskew[[col_name]]
    column_noNA <- column[!is.na(column)]
    
    # Calculate skewness and perform Shapiro-Wilk test for the original variable
    skewness_value <- skewness(column_noNA)
    shapiro_test <- shapiro.test(column_noNA)
    p_value <- shapiro_test$p.value
    
    # Add variable name, skewness value, and p-value to the skewness table
    skewness_table <- rbind(
      skewness_table,
      data.frame(
        variable = col_name,
        skewness = skewness_value,
        shapiro = p_value
      )
    )
    
    # Transformed variable using log1p()
    column_transformed <- log1p(column_noNA)
    
    # Calculate skewness and perform Shapiro-Wilk test for the transformed variable
    LN_skewness_value <- skewness(column_transformed)
    LN_shapiro_test <- shapiro.test(column_transformed)
    LN_p_value <- LN_shapiro_test$p.value
    
    # Add variable name, skewness value, and p-value for the transformed variable to the skewness table
    skewness_table <- rbind(
      skewness_table,
      data.frame(
        variable = paste0("LN_", col_name),
        skewness = LN_skewness_value,
        shapiro = LN_p_value
      )
    )
  }
}

# Print the skewness table
print(skewness_table) 
#Improve: all execept: TL, WeightLWR, Cloud.cover, Net_horizontal_opening, Distance_covered_GPS, Average_speed, 

data <- data %>% #recomendacion solo si cambia mucho.
  mutate(LN_TotalBiomassHaul = log1p(TotalBiomassHaul),
         LN_MinsExposedtoAir = log1p(MinsExposedtoAir))
head(data)

output_file <- file.path(output_data, "data_subsets", paste0(sp_code,"_dataset_log_pred.csv")) 
write.csv2(data, file = output_file, row.names = FALSE)
