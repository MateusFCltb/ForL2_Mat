#install.packages("terra")
library(terra)
#install.packages("tmap")
library(tmap)
#install.packages("sf")
library(sf)

#setwd("W:/ACADEMIC_Git/AGR333/ForL2")

### Read in the inventory data
#filepath <- "W:/ACADEMIC_Git/AGR333/ForL2/files/"
filepath <- "C:/Users/mateuscaltabiano/GitProjects/ForL2_Mat/files/"
dem <- rast(paste0(filepath, "unit2.img"))
dem
df_dem <- as.data.frame(dem, xy = TRUE)


### 2a Slope Extraction
slope <- terrain(dem, v= "slope", unit = "degrees", neighbors = 8)

## Aspect Extraction 
aspect <- terrain(dem, v = "aspect", unit = "degrees")


##Vizualizing slope and aspect
ttm()
tm_shape(slope, alpha = 0.5)+
  tm_raster(style = "cont", alpha = 0.6, title = "slope")
######### -- Really cool interactive map! use more for other stuff!! 

tm_shape(aspect)+
  tm_raster(style = "cont")

### Aspect reclassification

## asp class matx
asp_class <- matrix(c(
  0, 45, 1,
  45, 90, 2,
  90, 175, 2,
  175, 180, 3,
  180, 225, 3,
  225, 270, 4,
  270, 315, 4,
  315, 360, 1
), ncol = 3, byrow = TRUE)

asp <- classify(aspect, asp_class)

tm_shape(asp) +
  tm_raster(style = "cat", palette = c("white", "blue", "green", "yellow", "red"),
            labels = c(NA, "North", "East", "South", "West"), alpha = 0.2)


#################################################

#Forest inv plots viz

sum_u2 <- read.csv(paste0(filepath, "sum_u2.csv"))
library(sf)
svy_pts <- st_read(paste0(filepath, "HEE_Overstory_Survey_Points_2017.shp"))
svy_pts <- st_transform(svy_pts, 32616) # Project to WGS 84 UTM 16 N
survey_pts <- subset(svy_pts, Unit == '2') # Subset for unit 2

## Merge summary table with plot locations

sum_u2 <- merge.data.frame(sum_u2, survey_pts, all.x = TRUE)
unique(sum_u2$Plot)
unique(survey_pts$Plot)

# Convertin to sf format
sum_u2 <- st_as_sf(sum_u2, coords = c("X", "Y"), crs = 32616)
sum_u2

# Creating buffers with radius of 17.83 since that is the radius of the cirular plot. 
#?# Is buffer of a point always a circle by default? That makes sense since there is only one var (distance "radius" ) supplied to the buffer and no other geometric data other than the position of the point since it is dimensionless
sf_plot <- st_buffer(sum_u2, dist = 17.83)
plot(sf_plot)


# CRS cheks
crs(sf_plot , proj=T)
crs(asp , proj=T)

## transforming crs of plots to match images
asp_crs <- crs(asp, proj = TRUE)
sf_plot_crs <- st_transform(sf_plot, crs = asp_crs)


## vis dominant species by aspect
tm_shape(asp, alpha = 0.5) +
  tm_raster(style = "cat", palette = c("white", "blue", "green", "yellow", "red"),
            showNA = FALSE, alpha = 0.2, labels = c(NA, "North", "East", "South", "West")) +
  tm_shape(sf_plot) +
  tm_polygons('Common.name') +
  tm_layout(legend.outside = TRUE, legend.outside.size = 0.2) +
  tm_text("Plot", ymod = -0.9)



#Q4 - dominant tree by slp
ttm()
tm_shape(slope, alpha = 0.5) +
  tm_raster(style = "cont", alpha = 0.6, title = "Slope (deg)") +
  tm_shape(sf_plot_crs) +  # use the CRS-matched object
  tm_polygons('Common.name', title = "Dominant Species", alpha = 0.6) +
  tm_layout(title = "Dominant Species by Slope",
            legend.outside = TRUE, legend.outside.size = 0.2) +
  tm_text("Plot", ymod = -0.9, size = 1.2)


#q5 BA clustering

ttm()
tm_shape(asp, alpha = 0.5) +
  tm_raster(style = "cat",
            palette = c("white", "blue", "green", "yellow", "red"),
            labels = c(NA, "North", "East", "South", "West"),
            showNA = FALSE,
            alpha = 0.2) +
  tm_shape(sf_plot_crs) +
  tm_polygons("BA", palette = "spectral", title = "Basal Area (sq_ft/acre)", alpha = 0.7) +
  tm_layout(title = "Basal Area by Aspect",
            legend.outside = TRUE,
            legend.outside.size = 0.2) +
  tm_text("Plot", ymod = -1.5, size = 1.2) +
  tm_scale_bar()

# q6 - TPA

ttm()
tm_shape(asp, alpha = 0.5) +
  tm_raster(style = "cat",
            palette = c("white", "blue", "green", "yellow", "red"),
            labels = c(NA, "North", "East", "South", "West"),
            showNA = FALSE,
            alpha = 0.2) +
  tm_shape(sf_plot_crs) +
  tm_polygons("TPA", palette = "RdYlBu", title = "Trees Per Acre", alpha = 0.85) +
  tm_layout(title = "Trees Per Acre (TPA) by Aspect",
            legend.outside = TRUE,
            legend.outside.size = 0.25) +
  tm_text("Plot", ymod = -1.5, size = 1.2) +
  tm_scale_bar()

# q7 - biomass

ttm()
tm_shape(asp, alpha = 0.2) +
  tm_raster(style = "cat",
            palette = c("white", "blue", "green", "yellow", "red"),
            labels = c(NA, "North", "East", "South", "West"),
            showNA = FALSE,
            alpha = 0.2) +
  tm_shape(sf_plot_crs) +
  tm_polygons("bm_tonpa",
              title = "Biomass (tons/ac)",
              palette = "spectral",
              alpha = 1) +
  tm_layout(title = "Biomass by Aspect",
            legend.outside = TRUE,
            legend.outside.size = 0.25) +
  tm_text("Plot", ymod = -1.5, size = 1.2) +
  tm_scale_bar()



