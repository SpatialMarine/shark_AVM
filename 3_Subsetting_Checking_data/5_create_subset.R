#-----------------------------------------------------------------
# 5_create_subset       Create data subsets
#-----------------------------------------------------------------
# We also included a random number between 1 and 100 to serve as an indicator for variables that
# have influence greater or less than random (Scales et al., 2017; Soykan, Eguchi, Kohin, & Dewar, 2014);
# only variables with influence greater than the random number were included in the final models.

#Load data
setwd(input_data)
data <- read.csv("data_env_all.csv", sep = ";") #AVM_PRM_all.csv for PRM
names(data)
head(data)

output_dir <- file.path(output_data, paste0("data_subsets"))
if (!file.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)}

Sp <- unique(data$Species)

#Create your selected database:
#------------------------------------------------------------------------------#
#                                Scyliorhinus canicula                          
#------------------------------------------------------------------------------#
Scanicula <- data %>% 
  filter(Species == "Scanicula") %>%
  filter(!is.na(Alive_Dead))
names(Scanicula)
head(Scanicula)

#Save dataset
output_file <- file.path(output_dir, "Scanicula.csv")
write.csv2(Scanicula, file = output_file, row.names = FALSE)

#------------------------------------------------------------------------------#
#                                Galeus melastomus                          
#------------------------------------------------------------------------------#
Gmelastomus <- data %>% 
  filter(Species == "Gmelastomus") %>%
  filter(!is.na(Alive_Dead))
names(Gmelastomus)
head(Gmelastomus)

# Filter the data frame to remove rows where data$depth is less than 250 (rare observations, not normal depth range)
Gmelastomus <- Gmelastomus %>% filter(depth >= 250)

#Save dataset
output_file <- file.path(output_dir, "Gmelastomus.csv")
write.csv2(Gmelastomus, file = output_file, row.names = FALSE)

#------------------------------------------------------------------------------#
#                                Etmopterus spinax                        
#------------------------------------------------------------------------------#
Espinax <- data %>% 
  filter(Species == "Espinax") %>%
  filter(!is.na(Alive_Dead))
names(Espinax)
head(Espinax)

#Save dataset
output_file <- file.path(output_dir, "Espinax.csv")
write.csv2(Espinax, file = output_file, row.names = FALSE)

#------------------------------------------------------------------------------#
#                                Torpedo marmorata                          
#------------------------------------------------------------------------------#
Tmarmorata <- data %>% 
  filter(Species == "Tmarmorata") %>%
  filter(!is.na(Alive_Dead))
names(Tmarmorata)
head(Tmarmorata)

#Save dataset
output_file <- file.path(output_dir, "Tmarmorata.csv")
write.csv2(Tmarmorata, file = output_file, row.names = FALSE)

