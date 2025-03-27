install.packages("terra")
library(terra)
install.packages("tmap")
library(tmap)
install.packages("sf")
library(sf)

setwd("W:/ACADEMIC_Git/AGR333/ForL2")

### Read in the inventory data
filepath <- "W:/ACADEMIC_Git/AGR333/ForL2/files/"
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




