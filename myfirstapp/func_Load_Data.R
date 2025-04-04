load_forest_data <- function(filepath, buffer_radius = 17.83) {
  library(terra)
  library(sf)
  
  #Load DEM
  dem <- rast(paste0(filepath, "unit2.img"))
  
  #Extract Slope and Aspect
  slope <- terrain(dem, v = "slope", unit = "degrees", neighbors = 8)
  aspect <- terrain(dem, v = "aspect", unit = "degrees")
  
  #Reclassify Aspect
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
  
  #Load Summary CSV and Shapefile
  sum_u2 <- read.csv(paste0(filepath, "sum_u2.csv"))
  svy_pts <- st_read(paste0(filepath, "HEE_Overstory_Survey_Points_2017.shp"))
  svy_pts <- st_transform(svy_pts, 32616)  # Project to UTM 16N
  survey_pts <- subset(svy_pts, Unit == '2')
  
  #Merge summary table with points
  sum_u2 <- merge.data.frame(sum_u2, survey_pts, all.x = TRUE)
  
  # Convert to sf and buffer
  sum_u2 <- st_as_sf(sum_u2, coords = c("X", "Y"), crs = 32616)
  sf_plot <- st_buffer(sum_u2, dist = buffer_radius)
  
  # Match CRS with raster
  asp_crs <- crs(asp, proj = TRUE)
  sf_plot <- st_transform(sf_plot, crs = asp_crs)
  
  # Return all relevant data
  return(list(
    dem = dem,
    slope = slope,
    aspect = aspect,
    asp = asp,
    sf_plot = sf_plot
  ))
}
