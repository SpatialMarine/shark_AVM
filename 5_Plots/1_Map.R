#---------------------------------------------------------------------------------------------------
# 1_Map.R         Map plotting: Plot sampling stations
#---------------------------------------------------------------------------------------------------
pacman::p_load("magrittr", "ggshadow", "giscoR", "ggforce", "terra", "ggspatial", "raster", "rnaturalearth", "ggplot2","marmap", "terra", "tidyterra", "sf", "sfheaders", "ggforce", install=FALSE)

#---------------------------------------------------------------
# 1. Set data repository and load rasters
#---------------------------------------------------------------
#Bathymetry
wd<-paste0(output_data, "/EMODnet/Bathy")
setwd(wd)
bathy<- raster("Bathy.tif")
print(bathy)
# plot(bathy)
# Convert bathy raster to data frame
bathy_df <- as.data.frame(bathy, xy = TRUE)

#Mask
wd<-paste0(output_data, "/landmask/Europa")
setwd(wd)
mask<- st_read("Europe_coastline_poly.shp")
print(mask)
mask <- st_transform(mask, crs = 4326)

#Bath contour
wd<-paste0(output_data, "/EMODnet/Bathy/contour1")
setwd(wd)
Bathy_cont<- st_read("Contours_2022.shp")
Bathy_cont$Elevation <- as.numeric(Bathy_cont$Elevation)
print(Bathy_cont)

#Select the bathymetrical lines that you want to plot:
Bathy_cont1 <- Bathy_cont %>%
  filter(Elevation %in% c(50, 200, 1000, 2000, 3000, 4000))
# Set the CRS for the raster
st_crs(Bathy_cont1) <- st_crs(mask)
print(Bathy_cont1)

#Load data
setwd(input_data)
data <- read.csv("data_env_all.csv", sep = ";") #AVM_PRM_all.csv for PRM
names(data)
head(data)
data <- data %>%
  mutate(lon = as.numeric(gsub(",", ".", lon)),
         lat = as.numeric(gsub(",", ".", lat)))

#---------------------------------------------------------------
# 2. Make zoomed in map
#---------------------------------------------------------------
#Colour for bathymetry:
# Create a mask if you dont want to plot all the bathymetricla range
bathy_bb <-  bathy_df$Bathy <= 5 #if you want to put a below limit: bathy_df$Bathy >= -800 &
# Apply the mask
bathy_df <- bathy_df[bathy_bb, ]
print(bathy_df)

#Create colour ramp
#color_palette_bathy <- colorRampPalette(c("lightblue", "white")) 
color_palette_bathy <- colorRampPalette(rev(c('#ecf9ff','#BFEFFF','#97C8EB','#4682B4','#264e76','#162e46')))(100)
cuts_bathy <- cut(bathy_df$Bathy, breaks = 100)
color_indices_bathy <- as.numeric(cuts_bathy) * 100 / length(levels(cuts_bathy))
bathy_df$filling_color <- color_palette_bathy[color_indices_bathy]

# Create a ggplot object
p <- ggplot() +
  geom_tile(data = bathy_df, aes(x = x, y = y, fill = filling_color)) +
  geom_sf(data = Bathy_cont1,  lwd = 0.05) +
  # land mask
  geom_sf(data = mask) +
  # add tracks
  geom_point(data = data, aes(x = lon, y = lat, fill = "#FFE4B2"), shape = 21, size = 1.5, alpha = 0.6) + ##FFE4B2
  #Set spatial bounds
  coord_sf(xlim = c(-1.2978974, 3), ylim = c(37, 39.87846), expand = TRUE) +
  # Add scale bar
  #annotation_scale(location = "bl", width_hint = 0.2) +  
  # theme
  theme_bw() +
  # Directly map colors without scaling
  scale_fill_identity()+
  # Remove grids
  theme(panel.grid = element_blank())
  
print(p)

p + guides(fill = guide_legend(title = "Legend Title"))

# export plot
outdir <- paste0(output_data, "/Map")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
p_png <- paste0(outdir, "/StudyArea.png")
ggsave(p_png, p, width=17, height=17, units="cm", dpi=300)

#Plot legend:
# Define data for the legend
legend_data <- data.frame(values = seq(0, 260, length.out = 100))

# Create a ggplot object with a blank layer
p1 <- ggplot(legend_data, aes(x = 1, y = values, fill = values)) +
  geom_tile() +
  scale_fill_gradientn(colors = color_palette_bathy,
                       name = "Bathymetry", limits = c(0, 260), breaks = pretty(c(0, 260), n = 5))
# Remove axis labels and ticks
p1 <- p1 + theme(panel.grid = element_blank(),
               axis.text.x = element_blank(),
               axis.text.y = element_blank(),
               axis.ticks = element_blank(),
               axis.title.x = element_blank(),
               axis.title.y = element_blank(),
               legend.position = "right")

# Display the plot
print(p1)

# export plot
outdir <- paste0(output_data, "/Map")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
p_png <- paste0(outdir, "/Legend.png")
ggsave(p_png, p1, width=5, height=5, units="cm", dpi=300)

# export plot
outdir <- paste0(output_data, "/Map")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
p_png <- paste0(outdir, "/StudyArea.png")
ggsave(p_png, p, width=17, height=17, units="cm", dpi=300)

#---------------------------------------------------------------
# 3. Make zoomed out map
#---------------------------------------------------------------
#---------------------------------------------------------------
# 3.1. 3D World globe base map
#---------------------------------------------------------------
pacman::p_load(dplyr, data.table, rnaturalearth, rnaturalearthdata, 
               ggplot2, raster, terra, tidyr, stringr, gridExtra, 
               plotly, sf, ggshadow, ggforce, giscoR, install = FALSE)

# Custom global ortohraphic proyection from Western-Mediterranean
ortho_crs <-'+proj=ortho +lat_0=20 +lon_0=0.5 +x_0=0 +y_0=0 +R=6371000 +units=m +no_defs +type=crs'

# world coastlines
world_poly <- gisco_get_coastallines(year = "2016", epsg = "4326", resolution = "10")

# global graticule
grid <- st_graticule()

# ocean mask 
ocean <- st_point(x = c(0,0)) |>
  st_buffer(dist = 6371000) |> # Planet earth radius (m)
  st_sfc(crs = ortho_crs)
# plot(ocean)

# Select visible area and project
world <- world_poly |>
  st_intersection(st_transform(ocean, 4326)) |>
  st_transform(crs = ortho_crs) # 
# plot(world)

# delete grid trough continents to create a clean grid
grid_crp <- st_difference(grid, st_union(world_poly))

# select visible area
grid_crp <- st_intersection(grid_crp, st_transform(ocean, 4326)) |>
  st_transform(crs = ortho_crs)
# plot(grid_crp)

# cover globe limit into df - datframe
ocean_df <- st_cast(ocean, "LINESTRING") |> st_coordinates() |> as.data.frame()

# build shadow 
ggplot() + 
  geom_glowpath(data = ocean_df, 
                aes(X, Y, group = "L1"),
                shadowcolor='grey90',
                colour = "white",
                alpha = .01,
                shadowalpha=0.05,
                shadowsize = 1.5) +
  coord_sf() +
  theme_void()

# add more shadows
g <- ggplot() +
  geom_glowpath(data = ocean_df, 
                aes(X, Y, group = "L1"),
                shadowcolor='grey90',
                colour = "white",
                alpha = .01,
                shadowalpha=0.06,
                shadowsize = 1.5) +
  geom_glowpath(data = ocean_df, 
                aes(X, Y, group = "L1"),
                shadowcolor='grey90',
                colour = "white",
                alpha = .01,
                shadowalpha=0.02,
                shadowsize = 1) +
  geom_glowpath(data = ocean_df, 
                aes(X, Y, group = "L1"),
                shadowcolor='grey90',
                colour = "white",
                alpha = .01,
                shadowalpha=0.01,
                shadowsize = .5)

# adding other layers to the base shadow
g2 <- g +
  # add grid
  geom_sf(data = grid_crp, 
          colour = "grey85", 
          linewidth = .15) +
  # add sea-turtle SSM points
  #geom_sf(data = ssm_sf, size = 0.15, color = "deepskyblue3") +
  # add 3D globe land
  geom_sf(data = world, 
          #fill = "#FFE4B2",
          colour = "grey35",
          linewidth = .2) +
    # theme
  theme_void()

print(g2)

# export plot
outdir <- paste0(output_data, "/Map")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
p_png <- paste0(outdir, "/FullAreaGlobe.jpeg")
ggsave(p_png, g2, width=17, height=17, units="cm", dpi=300)

#---------------------------------------------------------------
# 3.2. 2D World globe base map (not used in my case)
#---------------------------------------------------------------

## Create a ggplot object
#p <- ggplot() +
#  # land mask
#  #geom_tile(data = bathy_df, aes(x = x, y = y, fill = filling_color)) +
#  #geom_sf(data = Bathy_cont,  lwd = 0.05) +
#  geom_sf(data = mask) +
#  # set spatial bounds
#  coord_sf(xlim = c(-5, 35), ylim = c(31, 45.2), expand = TRUE) +
#  # Add dtudy area bounding box
#  geom_rect(
#    aes(xmin = -1.2978974, xmax = 3, ymin = 37, ymax = 39.87846),
#    color = "black", fill = NA, lwd = 0.2) +
#  # theme
#  theme_bw() +
#  # Remove grids
#  theme(panel.grid = element_blank())  
#
#print(p)
#
## export plot
#outdir <- paste0(output_data, "/Map")
#if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
#p_png <- paste0(outdir, "/FullArea.png")
#ggsave(p_png, p, width=17, height=17, units="cm", dpi=300)
#