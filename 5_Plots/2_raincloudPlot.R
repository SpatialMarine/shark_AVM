#---------------------------------------------------------------------------------------------------
# 2_raincloudPlot.R    Plot data distribution with raincloud (violin plots and box plots) + calculate mean values
#---------------------------------------------------------------------------------------------------
#REFERENCE: https://www.smin95.com/dataviz/raincloud-and-forest-plots

#Load packages:
pacman::p_load(readxl, cowplot, gghalves, tidyverse, gridExtra, install = FALSE)
#devtools::install_github('smin95/smplot2', force = TRUE)
library(smplot2)

#Load data
setwd(input_data)
data <- read.csv("data_env_all.csv", sep = ";") 
names(data)
head(data)
#Set categorical predictors as categories:
data <- data %>% 
  mutate(Sex = factor(Sex, c(0, 1)),
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

# prepare data
df_Sca <-  filter(data, Species == 'Scanicula')
df_Gme <-  filter(data, Species == 'Gmelastomus')

# color palette
sm_palette(20)

#----------------------------------------------------------------------------------- 
# 1. Make raincloud plots for each predictor used in the BRT
#----------------------------------------------------------------------------------- 

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Depth
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
p1 <- ggplot(data = data, mapping = aes(x = Species, y = depth, fill = Species)) +
  sm_raincloud(data = df_Gme, position = position_nudge(x = 0.9),
               show.legend = FALSE,
               point.params = list(size = 2, shape = 21,
                                   color = 'transparent', 
                                   show.legend = TRUE,
                                   alpha = 0,
                                   position = sdamr::position_jitternudge(nudge.x = 1.04, seed = 10,
                                                                          jitter.width = 0.06))) +
  sm_raincloud(data = df_Sca, which_side = 'left',
               show.legend = FALSE,
               position = position_nudge(x = -0.15),
               point.params = list(size = 2, shape = 21,
                                   show.legend = TRUE,
                                   color = 'transparent', alpha = 0,
                                   position = sdamr::position_jitternudge(nudge.x = -0.06, seed = 10,
                                                                          jitter.width = 0.06))) +
  # color groups
  scale_fill_manual(values = sm_color('skyblue', 'orange')) +
  # y lab
  ylab('DEPTH') +
  # Positioning Y-axis values on the right side
  scale_y_continuous(position = "right") +
  # theme 
  theme(axis.title.x = element_blank(),
        axis.text.y = element_text(angle = 90),
        axis.line.y = element_blank(), 
        axis.ticks.y = element_blank())
        #legend.position = c(0.24,0.85),
        #legend.title = element_blank(),
        #legend.text = element_text(size = 12))

print(p1)

# export plot
outdir <- paste0(output_data, "/raincloud")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
p_png <- paste0(outdir, "/Depth.png")
ggsave(p_png, p1, width=17, height=10, units="cm", dpi=300)

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# DUR
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
p2 <- ggplot(data = data, mapping = aes(x = Species, y = Trawl_duration, fill = Species)) +
  sm_raincloud(data = df_Gme, position = position_nudge(x = 0.9),
               show.legend = FALSE,
               point.params = list(size = 2, shape = 21,
                                   color = 'transparent', 
                                   show.legend = TRUE,
                                   alpha = 0,
                                   position = sdamr::position_jitternudge(nudge.x = 1.04, seed = 10,
                                                                          jitter.width = 0.06))) +
  sm_raincloud(data = df_Sca, which_side = 'left',
               show.legend = FALSE,
               position = position_nudge(x = -0.15),
               point.params = list(size = 2, shape = 21,
                                   show.legend = TRUE,
                                   color = 'transparent', alpha = 0,
                                   position = sdamr::position_jitternudge(nudge.x = -0.06, seed = 10,
                                                                          jitter.width = 0.06))) +
  # color groups
  scale_fill_manual(values = sm_color('skyblue', 'orange')) +
  # y lab
  ylab('DUR') +
  # Positioning Y-axis values on the right side
  scale_y_continuous(position = "right") +
  # theme 
  theme(axis.title.x = element_blank(),
        axis.text.y = element_text(angle = 90),
        axis.line.y = element_blank(), 
        axis.ticks.y = element_blank())

print(p2)

# export plot
outdir <- paste0(output_data, "/raincloud")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
p_png <- paste0(outdir, "/DUR.png")
ggsave(p_png, p2, width=17, height=10, units="cm", dpi=300)

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# SPEED
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
p3 <- ggplot(data = data, mapping = aes(x = Species, y = Average_speed, fill = Species)) +
  sm_raincloud(data = df_Gme, position = position_nudge(x = 0.9),
               show.legend = FALSE,
               point.params = list(size = 2, shape = 21,
                                   color = 'transparent', 
                                   show.legend = TRUE,
                                   alpha = 0,
                                   position = sdamr::position_jitternudge(nudge.x = 1.04, seed = 10,
                                                                          jitter.width = 0.06))) +
  sm_raincloud(data = df_Sca, which_side = 'left',
               show.legend = FALSE,
               position = position_nudge(x = -0.15),
               point.params = list(size = 2, shape = 21,
                                   show.legend = TRUE,
                                   color = 'transparent', alpha = 0,
                                   position = sdamr::position_jitternudge(nudge.x = -0.06, seed = 10,
                                                                          jitter.width = 0.06))) +
  # color groups
  scale_fill_manual(values = sm_color('skyblue', 'orange')) +
  # y lab
  ylab('SPEED') +
  # Positioning Y-axis values on the right side
  scale_y_continuous(position = "right") +
  # theme 
  theme(axis.title.x = element_blank(),
        axis.text.y = element_text(angle = 90),
        axis.line.y = element_blank(), 
        axis.ticks.y = element_blank())

print(p3)

# export plot
outdir <- paste0(output_data, "/raincloud")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
p_png <- paste0(outdir, "/SPEED.png")
ggsave(p_png, p3, width=17, height=10, units="cm", dpi=300)

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# DTEMP
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
p5 <- ggplot(data = data, mapping = aes(x = Species, y = diff_at_sbt, fill = Species)) +
  sm_raincloud(data = df_Gme, position = position_nudge(x = 0.9),
               show.legend = FALSE,
               point.params = list(size = 2, shape = 21,
                                   color = 'transparent', 
                                   show.legend = TRUE,
                                   alpha = 0,
                                   position = sdamr::position_jitternudge(nudge.x = 1.04, seed = 10,
                                                                          jitter.width = 0.06))) +
  sm_raincloud(data = df_Sca, which_side = 'left',
               show.legend = FALSE,
               position = position_nudge(x = -0.15),
               point.params = list(size = 2, shape = 21,
                                   show.legend = TRUE,
                                   color = 'transparent', alpha = 0,
                                   position = sdamr::position_jitternudge(nudge.x = -0.06, seed = 10,
                                                                          jitter.width = 0.06))) +
  # color groups
  scale_fill_manual(values = sm_color('skyblue', 'orange')) +
  # y lab
  ylab('DTEMP') +
  # Positioning Y-axis values on the right side
  scale_y_continuous(position = "right") +
  # theme 
  theme(axis.title.x = element_blank(),
        axis.text.y = element_text(angle = 90),
        axis.line.y = element_blank(), 
        axis.ticks.y = element_blank())

print(p5)

# export plot
outdir <- paste0(output_data, "/raincloud")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
p_png <- paste0(outdir, "/DTEMP.png")
ggsave(p_png, p5, width=17, height=10, units="cm", dpi=300)

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# ATEMP
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
p50 <- ggplot(data = data, mapping = aes(x = Species, y = at_celsius, fill = Species)) +
  sm_raincloud(data = df_Gme, position = position_nudge(x = 0.9),
               show.legend = FALSE,
               point.params = list(size = 2, shape = 21,
                                   color = 'transparent', 
                                   show.legend = TRUE,
                                   alpha = 0,
                                   position = sdamr::position_jitternudge(nudge.x = 1.04, seed = 10,
                                                                          jitter.width = 0.06))) +
  sm_raincloud(data = df_Sca, which_side = 'left',
               show.legend = FALSE,
               position = position_nudge(x = -0.15),
               point.params = list(size = 2, shape = 21,
                                   show.legend = TRUE,
                                   color = 'transparent', alpha = 0,
                                   position = sdamr::position_jitternudge(nudge.x = -0.06, seed = 10,
                                                                          jitter.width = 0.06))) +
  # color groups
  scale_fill_manual(values = sm_color('skyblue', 'orange')) +
  # y lab
  ylab('ATEMP') +
  # Positioning Y-axis values on the right side
  scale_y_continuous(position = "right") +
  # theme 
  theme(axis.title.x = element_blank(),
        axis.text.y = element_text(angle = 90),
        axis.line.y = element_blank(), 
        axis.ticks.y = element_blank())

print(p50)

# export plot
outdir <- paste0(output_data, "/raincloud")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
p_png <- paste0(outdir, "/ATEMP.png")
ggsave(p_png, p50, width=17, height=10, units="cm", dpi=300)
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# TOWMASS
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
p4 <- ggplot(data = data, mapping = aes(x = Species, y = TotalBiomassHaul, fill = Species)) +
  sm_raincloud(data = df_Gme, position = position_nudge(x = 0.9),
               show.legend = FALSE,
               point.params = list(size = 2, shape = 21,
                                   color = 'transparent', 
                                   show.legend = TRUE,
                                   alpha = 0,
                                   position = sdamr::position_jitternudge(nudge.x = 1.04, seed = 10,
                                                                          jitter.width = 0.06))) +
  sm_raincloud(data = df_Sca, which_side = 'left',
               show.legend = FALSE,
               position = position_nudge(x = -0.15),
               point.params = list(size = 2, shape = 21,
                                   show.legend = TRUE,
                                   color = 'transparent', alpha = 0,
                                   position = sdamr::position_jitternudge(nudge.x = -0.06, seed = 10,
                                                                          jitter.width = 0.06))) +
  # color groups
  scale_fill_manual(values = sm_color('skyblue', 'orange')) +
  # y lab
  ylab('TOWMASS') +
  # Positioning Y-axis values on the right side
  scale_y_continuous(position = "right") +
  # theme 
  theme(axis.title.x = element_blank(),
        axis.text.y = element_text(angle = 90),
        axis.line.y = element_blank(), 
        axis.ticks.y = element_blank())

print(p4)

# export plot
outdir <- paste0(output_data, "/raincloud")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
p_png <- paste0(outdir, "/TOWMASS.png")
ggsave(p_png, p4, width=17, height=10, units="cm", dpi=300)

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# CLOUD
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
p6 <- ggplot(data = data, mapping = aes(x = Species, y = Cloud.cover, fill = Species)) +
  sm_raincloud(data = df_Gme, position = position_nudge(x = 0.9),
               show.legend = FALSE,
               point.params = list(size = 2, shape = 21,
                                   color = 'transparent', 
                                   show.legend = TRUE,
                                   alpha = 0,
                                   position = sdamr::position_jitternudge(nudge.x = 1.04, seed = 10,
                                                                          jitter.width = 0.06))) +
  sm_raincloud(data = df_Sca, which_side = 'left',
               show.legend = FALSE,
               position = position_nudge(x = -0.15),
               point.params = list(size = 2, shape = 21,
                                   show.legend = TRUE,
                                   color = 'transparent', alpha = 0,
                                   position = sdamr::position_jitternudge(nudge.x = -0.06, seed = 10,
                                                                          jitter.width = 0.06))) +
  # color groups
  scale_fill_manual(values = sm_color('skyblue', 'orange')) +
  # y lab
  ylab('CLOUD') +
  # Positioning Y-axis values on the right side
  scale_y_continuous(position = "right") +
  # theme 
  theme(axis.title.x = element_blank(),
        axis.text.y = element_text(angle = 90),
        axis.line.y = element_blank(), 
        axis.ticks.y = element_blank())

print(p6)

# export plot
outdir <- paste0(output_data, "/raincloud")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
p_png <- paste0(outdir, "/CLOUD.png")
ggsave(p_png, p6, width=17, height=10, units="cm", dpi=300)

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# SEASTATE
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
p7 <- ggplot(data = data, mapping = aes(x = Species, y = Sea.state, fill = Species)) +
  sm_raincloud(data = df_Gme, position = position_nudge(x = 0.9),
               show.legend = FALSE,
               point.params = list(size = 2, shape = 21,
                                   color = 'transparent', 
                                   show.legend = TRUE,
                                   alpha = 0,
                                   position = sdamr::position_jitternudge(nudge.x = 1.04, seed = 10,
                                                                          jitter.width = 0.06))) +
  sm_raincloud(data = df_Sca, which_side = 'left',
               show.legend = FALSE,
               position = position_nudge(x = -0.15),
               point.params = list(size = 2, shape = 21,
                                   show.legend = TRUE,
                                   color = 'transparent', alpha = 0,
                                   position = sdamr::position_jitternudge(nudge.x = -0.06, seed = 10,
                                                                          jitter.width = 0.06))) +
  # color groups
  scale_fill_manual(values = sm_color('skyblue', 'orange')) +
  # y lab
  ylab('SEASTATE') +
  # Positioning Y-axis values on the right side
  scale_y_continuous(position = "right") +
  # theme 
  theme(axis.title.x = element_blank(),
        axis.text.y = element_text(angle = 90),
        axis.line.y = element_blank(), 
        axis.ticks.y = element_blank())

print(p7)

# export plot
outdir <- paste0(output_data, "/raincloud")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
p_png <- paste0(outdir, "/SEASTATE.png")
ggsave(p_png, p7, width=17, height=10, units="cm", dpi=300)

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# WIND
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
p71 <- ggplot(data = data, mapping = aes(x = Species, y = Wind.strength, fill = Species)) +
  sm_raincloud(data = df_Gme, position = position_nudge(x = 0.9),
               show.legend = FALSE,
               point.params = list(size = 2, shape = 21,
                                   color = 'transparent', 
                                   show.legend = TRUE,
                                   alpha = 0,
                                   position = sdamr::position_jitternudge(nudge.x = 1.04, seed = 10,
                                                                          jitter.width = 0.06))) +
  sm_raincloud(data = df_Sca, which_side = 'left',
               show.legend = FALSE,
               position = position_nudge(x = -0.15),
               point.params = list(size = 2, shape = 21,
                                   show.legend = TRUE,
                                   color = 'transparent', alpha = 0,
                                   position = sdamr::position_jitternudge(nudge.x = -0.06, seed = 10,
                                                                          jitter.width = 0.06))) +
  # color groups
  scale_fill_manual(values = sm_color('skyblue', 'orange')) +
  # y lab
  ylab('WIND') +
  # Positioning Y-axis values on the right side
  scale_y_continuous(position = "right") +
  # theme 
  theme(axis.title.x = element_blank(),
        axis.text.y = element_text(angle = 90),
        axis.line.y = element_blank(), 
        axis.ticks.y = element_blank())

print(p71)

# export plot
outdir <- paste0(output_data, "/raincloud")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
p_png <- paste0(outdir, "/WIND.png")
ggsave(p_png, p71, width=17, height=10, units="cm", dpi=300)

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
#DECKTIME 
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
p8 <- ggplot(data = data, mapping = aes(x = Species, y = MinsExposedtoAir, fill = Species)) +
  sm_raincloud(data = df_Gme, position = position_nudge(x = 0.9),
               show.legend = FALSE,
               point.params = list(size = 2, shape = 21,
                                   color = 'transparent', 
                                   show.legend = TRUE,
                                   alpha = 0,
                                   position = sdamr::position_jitternudge(nudge.x = 1.04, seed = 10,
                                                                          jitter.width = 0.06))) +
  sm_raincloud(data = df_Sca, which_side = 'left',
               show.legend = FALSE,
               position = position_nudge(x = -0.15),
               point.params = list(size = 2, shape = 21,
                                   show.legend = TRUE,
                                   color = 'transparent', alpha = 0,
                                   position = sdamr::position_jitternudge(nudge.x = -0.06, seed = 10,
                                                                          jitter.width = 0.06))) +
  # color groups
  scale_fill_manual(values = sm_color('skyblue', 'orange')) +
  # y lab
  ylab('DECKTIME') +
  # Positioning Y-axis values on the right side
  scale_y_continuous(position = "right") +
  # theme 
  theme(axis.title.x = element_blank(),
        axis.text.y = element_text(angle = 90),
        axis.line.y = element_blank(), 
        axis.ticks.y = element_blank())

print(p8)

# export plot
outdir <- paste0(output_data, "/raincloud")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
p_png <- paste0(outdir, "/DECKTIME.png")
ggsave(p_png, p8, width=17, height=10, units="cm", dpi=300)

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
#TL
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# prepare data
df_Sca_male <-  filter(df_Sca, Sex == '0')
df_Sca_female <-  filter(df_Sca, Sex == '1')
df_Gme_male <-  filter(df_Gme, Sex == '0')
df_Gme_female <-  filter(df_Gme, Sex == '1')

#TL male
p9 <- ggplot(data = df_Sca_male, mapping = aes(x = Species, y = TL, fill = Species)) +
  sm_raincloud(data = df_Gme_male, position = position_nudge(x = 0.9),
               show.legend = FALSE,
               point.params = list(size = 2, shape = 21,
                                   color = 'transparent', 
                                   show.legend = TRUE,
                                   alpha = 0,
                                   position = sdamr::position_jitternudge(nudge.x = 1.04, seed = 10,
                                                                          jitter.width = 0.06))) +
  sm_raincloud(data = df_Sca_male, which_side = 'left',
               show.legend = FALSE,
               position = position_nudge(x = -0.15),
               point.params = list(size = 2, shape = 21,
                                   show.legend = TRUE,
                                   color = 'transparent', alpha = 0,
                                   position = sdamr::position_jitternudge(nudge.x = -0.06, seed = 10,
                                                                          jitter.width = 0.06))) +
  # color groups
  scale_fill_manual(values = sm_color('skyblue', 'orange')) +
  # y lab
  ylab('TL') +
  # Positioning Y-axis values on the right side
  scale_y_continuous(position = "right") +
  # theme 
  theme(axis.title.x = element_blank(),
        axis.text.y = element_text(angle = 90),
        axis.line.y = element_blank(), 
        axis.ticks.y = element_blank())

print(p9)

# export plot
outdir <- paste0(output_data, "/raincloud")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
p_png <- paste0(outdir, "/TL_males.png")
ggsave(p_png, p9, width=12, height=10, units="cm", dpi=300)

#TL female
p10 <- ggplot(data = df_Sca_female, mapping = aes(x = Species, y = TL, fill = Species)) +
  sm_raincloud(data = df_Gme_female, position = position_nudge(x = 0.9),
               show.legend = FALSE,
               point.params = list(size = 2, shape = 21,
                                   color = 'transparent', 
                                   show.legend = TRUE,
                                   alpha = 0,
                                   position = sdamr::position_jitternudge(nudge.x = 1.04, seed = 10,
                                                                          jitter.width = 0.06))) +
  sm_raincloud(data = df_Sca_female, which_side = 'left',
               show.legend = FALSE,
               position = position_nudge(x = -0.15),
               point.params = list(size = 2, shape = 21,
                                   show.legend = TRUE,
                                   color = 'transparent', alpha = 0,
                                   position = sdamr::position_jitternudge(nudge.x = -0.06, seed = 10,
                                                                          jitter.width = 0.06))) +
  # color groups
  scale_fill_manual(values = sm_color('skyblue', 'orange')) +
  # y lab
  ylab('TL') +
  # Positioning Y-axis values on the right side
  scale_y_continuous(position = "right") +
  # theme 
  theme(axis.title.x = element_blank(),
        axis.text.y = element_text(angle = 90),
        axis.line.y = element_blank(), 
        axis.ticks.y = element_blank())

print(p10)

# export plot
outdir <- paste0(output_data, "/raincloud")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
p_png <- paste0(outdir, "/TL_females.png")
ggsave(p_png, p10, width=12, height=10, units="cm", dpi=300)


#----------------------------------------------------------------------------------- 
# 2. What is the mean +- SD for gear values for each tow? and for min an max values?
#----------------------------------------------------------------------------------- 


# Group the data by tows and calculate mean and standard deviation for the specified columns, which are:
names(data)
# "Net_horizontal_opening", "Distance_covered_GPS", "Average_speed", "Trawl_duration", "MinsExposedtoAir", "depth"

# Merge all data from a single haul into a single row to calculate averages (add SD to check that there are no errors):
gear_df <- data %>%
  filter(!is.na(Alive_Dead)) %>%
  mutate(Net_horizontal_opening = as.numeric(gsub(",", ".", Net_horizontal_opening)),
         Distance_covered_GPS = as.numeric(gsub(",", ".", Distance_covered_GPS)),
         Average_speed = as.numeric(gsub(",", ".", Average_speed)),
         Trawl_duration = as.numeric(gsub(",", ".", Trawl_duration)),
         MinsExposedtoAir = as.numeric(gsub(",", ".", MinsExposedtoAir)),
         depth = as.numeric(gsub(",", ".", depth)),
         TotalBiomassHaul = as.numeric(gsub(",", ".", TotalBiomassHaul)),
         Sea.state = as.numeric(gsub(",", ".", Sea.state)),
         Wind.strength = as.numeric(gsub(",", ".", Wind.strength)),
         Cloud.cover = as.numeric(gsub(",", ".", Cloud.cover)),
         diff_at_sbt = as.numeric(gsub(",", ".", diff_at_sbt)),
         at_celsius = as.numeric(gsub(",", ".", at_celsius))) %>%
  group_by(Haul_N) %>%
  summarise(
    Mean_Net_horizontal_opening = mean(Net_horizontal_opening, na.rm = TRUE),
    SD_Net_horizontal_opening = sd(Net_horizontal_opening, na.rm = TRUE),
    Mean_Distance_covered_GPS = mean(Distance_covered_GPS, na.rm = TRUE),
    SD_Distance_covered_GPS = sd(Distance_covered_GPS, na.rm = TRUE),
    Mean_Average_speed = mean(Average_speed, na.rm = TRUE),
    SD_Average_speed = sd(Average_speed, na.rm = TRUE),
    Mean_Trawl_duration = mean(Trawl_duration, na.rm = TRUE),
    SD_Trawl_duration = sd(Trawl_duration, na.rm = TRUE),
    Mean_Trawl_duration = mean(Trawl_duration, na.rm = TRUE),
    SD_Trawl_duration = sd(Trawl_duration, na.rm = TRUE),
    Mean_depth = mean(depth, na.rm = TRUE),
    SD_Trawl_depth = sd(depth, na.rm = TRUE),
    Mean_MinsExposedtoAir = mean(MinsExposedtoAir, na.rm = TRUE),
    SD_MinsExposedtoAir = sd(MinsExposedtoAir, na.rm = TRUE),
    Mean_TotalBiomassHaul = mean(TotalBiomassHaul, na.rm = TRUE),
    SD_TotalBiomassHaul = sd(TotalBiomassHaul, na.rm = TRUE),
    Mean_Sea.state = mean(Sea.state, na.rm = TRUE),
    SD_Sea.state = sd(Sea.state, na.rm = TRUE),
    Mean_Wind.strength = mean(Wind.strength, na.rm = TRUE),
    SD_Trawl_Wind.strength = sd(Wind.strength, na.rm = TRUE),
    Mean_Cloud.cover = mean(Cloud.cover, na.rm = TRUE),
    SD_Cloud.cover = sd(Cloud.cover, na.rm = TRUE),
    Mean_diff_at_sbt = mean(diff_at_sbt, na.rm = TRUE),
    SD_diff_at_sbt = sd(diff_at_sbt, na.rm = TRUE),
    Mean_at_celsius = mean(at_celsius, na.rm = TRUE),
    SD_at_celsius = sd(at_celsius, na.rm = TRUE),)

# Print the result
print(gear_df)

#Put variables as numerical:
gear_df <- gear_df %>%
  filter(Haul_N != 67) %>%
  mutate(
    Mean_Net_horizontal_opening = as.numeric(gsub(",", ".", Mean_Net_horizontal_opening)),
    Mean_Distance_covered_GPS = as.numeric(gsub(",", ".", Mean_Distance_covered_GPS)),
    Mean_Average_speed = as.numeric(gsub(",", ".", Mean_Average_speed)),
    Mean_Trawl_duration = as.numeric(gsub(",", ".", Mean_Trawl_duration)),
    Mean_MinsExposedtoAir = as.numeric(gsub(",", ".", Mean_MinsExposedtoAir)),
    Mean_depth = as.numeric(gsub(",", ".", Mean_depth)),
    Mean_TotalBiomassHaul = as.numeric(gsub(",", ".", Mean_TotalBiomassHaul)),
    Mean_Sea.state = as.numeric(gsub(",", ".", Mean_Sea.state)),
    Mean_Wind.strength = as.numeric(gsub(",", ".", Mean_Wind.strength)),
    Mean_Cloud.cover = as.numeric(gsub(",", ".", Mean_Cloud.cover)),
    Mean_diff_at_sbt = as.numeric(gsub(",", ".", Mean_diff_at_sbt)),
    Mean_at_celsius = as.numeric(gsub(",", ".", Mean_at_celsius)))

# Calculate overall average +-SD values:
gear_df_summary <- gear_df %>%
  summarise(
    M_Mean_Net_horizontal_opening = mean(Mean_Net_horizontal_opening, na.rm = TRUE),
    sd_Net_horizontal_opening = sd(Mean_Net_horizontal_opening, na.rm = TRUE),
    M_Mean_Distance_covered_GPS = mean(Mean_Distance_covered_GPS, na.rm = TRUE),
    sd_Distance_covered_GPS = sd(Mean_Distance_covered_GPS, na.rm = TRUE),
    M_Mean_Average_speed = mean(Mean_Average_speed, na.rm = TRUE),
    sd_Average_speed = sd(Mean_Average_speed, na.rm = TRUE),
    M_Mean_Trawl_duration = mean(Mean_Trawl_duration, na.rm = TRUE),
    sd_Trawl_duration = sd(Mean_Trawl_duration, na.rm = TRUE),
    M_Mean_MinsExposedtoAir = mean(Mean_MinsExposedtoAir, na.rm = TRUE),
    sd_MinsExposedtoAir = sd(Mean_MinsExposedtoAir, na.rm = TRUE),
    M_Mean_depth = mean(Mean_depth, na.rm = TRUE),
    sd_depth = sd(Mean_depth, na.rm = TRUE),
    M_Mean_TotalBiomassHaul = mean(Mean_TotalBiomassHaul, na.rm = TRUE),
    sd_Mean_TotalBiomassHaul = sd(Mean_TotalBiomassHaul, na.rm = TRUE),
    M_Mean_Sea.state = mean(Mean_Sea.state, na.rm = TRUE),
    sd_Mean_Sea.state = sd(Mean_Sea.state, na.rm = TRUE),
    M_Mean_Wind.strength = mean(Mean_Wind.strength, na.rm = TRUE),
    sd_Mean_Wind.strength = sd(Mean_Wind.strength, na.rm = TRUE),
    M_Mean_Cloud.cover = mean(Mean_Cloud.cover, na.rm = TRUE),
    sd_Mean_Cloud.cover = sd(Mean_Cloud.cover, na.rm = TRUE),
    M_Mean_diff_at_sbt = mean(Mean_diff_at_sbt, na.rm = TRUE),
    sd_Mean_diff_at_sbt = sd(Mean_diff_at_sbt, na.rm = TRUE),
    M_Mean_at_celsius = mean(Mean_at_celsius, na.rm = TRUE),
    sd_Mean_at_celsius = sd(Mean_at_celsius, na.rm = TRUE),)

# Print the result
print(gear_df_summary)

#Min and Max values:
gear_df_summary_range <- gear_df %>%
  summarise(
    Min_Mean_Net_horizontal_opening = min(Mean_Net_horizontal_opening, na.rm = TRUE),
    Max_Mean_Net_horizontal_opening = max(Mean_Net_horizontal_opening, na.rm = TRUE),
    Min_Mean_Distance_covered_GPS = min(Mean_Distance_covered_GPS, na.rm = TRUE),
    Max_Mean_Distance_covered_GPS = max(Mean_Distance_covered_GPS, na.rm = TRUE),
    Min_Mean_Average_speed = min(Mean_Average_speed, na.rm = TRUE),
    Max_Mean_Average_speed = max(Mean_Average_speed, na.rm = TRUE),
    Min_Mean_Trawl_duration = min(Mean_Trawl_duration, na.rm = TRUE),
    Max_Mean_Trawl_duration = max(Mean_Trawl_duration, na.rm = TRUE),
    Min_Mean_MinsExposedtoAir = min(Mean_MinsExposedtoAir, na.rm = TRUE),
    Max_Mean_MinsExposedtoAir = max(Mean_MinsExposedtoAir, na.rm = TRUE),
    Min_Mean_depth = min(Mean_depth, na.rm = TRUE),
    Max_Mean_depth = max(Mean_depth, na.rm = TRUE),
    Min_Mean_TotalBiomassHaul = min(Mean_TotalBiomassHaul, na.rm = TRUE),
    Max_Mean_TotalBiomassHaul = max(Mean_TotalBiomassHaul, na.rm = TRUE),
    Min_Mean_Sea.state = min(Mean_Sea.state, na.rm = TRUE),
    Max_Mean_Sea.state = max(Mean_Sea.state, na.rm = TRUE),
    Min_Mean_Wind.strength = min(Mean_Wind.strength, na.rm = TRUE),
    Max_Mean_Wind.strength = max(Mean_Wind.strength, na.rm = TRUE),
    Min_Mean_Cloud.cover = min(Mean_Cloud.cover, na.rm = TRUE),
    Max_Mean_Cloud.cover = max(Mean_Cloud.cover, na.rm = TRUE),
    Min_Mean_diff_at_sbt = min(Mean_diff_at_sbt, na.rm = TRUE),
    Max_Mean_diff_at_sbt = max(Mean_diff_at_sbt, na.rm = TRUE),
    Min_Mean_at_celsius = min(Mean_at_celsius, na.rm = TRUE),
    Max_Mean_at_celsius = max(Mean_at_celsius, na.rm = TRUE))

# Print the result
print(gear_df_summary_range)

