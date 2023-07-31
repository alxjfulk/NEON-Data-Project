#for creating NEON shapefile of ALL tick sites (including those that are decomissioned)

library(geoNEON)
library(neonUtilities)
library(tidyverse)
library(sp)
library(rgdal)
library(rgeos)
library(raster)
#download NEON tick data for DSNY
# ticks <- loadByProduct(
#   dpID='DP1.10093.001',
#   check.size=F,
#   site = "DSNY",
#   package='basic') # remove this line if you don't have a token, though everyone is encouraged to get one

# Turn all tables in the list to dataframe (DF) in the global environment, where name of table = name of DF. This is helpful because most functions require a DF as input. The list itself is still preserved when this command is run, for those few functions that actually need lists.
# list2env(ticks , envir=.GlobalEnv)

#rename or create a dataframe with a column called namedLocation that contains the plots you want centroid information for

#if you directly pulled the tck data from the portal using loadbyProduct then the tck_fielddata df contains the namedLocation field
# locationsDF <- data.frame(unique(tck_fielddata$namedLocation))
# names(locationsDF) <- "namedLocation"
locationsDF <- data.frame(unique(combineddata13$namedLocation))
names(locationsDF) <- "namedLocation"
locationsDF$namedLocation <- paste0(locationsDF$namedLocation, ".tck")
#of you can manually create a dataframe if you know what namedLocations to search for. Note that this method provides an activity start and end data if the plot is decommissioned
# locationsDF <- data.frame(namedLocation = c("DSNY_002.tickPlot.tck", "DSNY_014.tickPlot.tck"))
# locationsDF1 <- data.frame(locationsDF[1,])
# names(locationsDF1) <- "namedLocation"

#use geoNEOn to pull the spatial data
spatialData <- geoNEON::def.extr.geo.os(data = locationsDF,
                                        locCol = "namedLocation",
                                        locOnly = F)
spatialData$easting <- as.numeric(spatialData$easting)
spatialData$northing <- as.numeric(spatialData$northing)
# set the radius for the plots
radius <- 20 # radius in meters

# define the plot edges based upon the plot radius. 

yPlus <- spatialData$northing+radius
xPlus <- spatialData$easting+radius
yMinus <- spatialData$northing-radius
xMinus <- spatialData$easting-radius

# calculate polygon coordinates for each plot centroid. 
square=cbind(xMinus,yPlus,  # NW corner
             xPlus, yPlus,  # NE corner
             xPlus,yMinus,  # SE corner
             xMinus,yMinus, # SW corner
             xMinus,yPlus)  # NW corner again - close ploygon

# Extract the plot ID information
ID=spatialData$namedLocation
check.plots <- split(seq_along(tickshape@data[["plotID"]]), factor(tickshape@data[["plotID"]], spatialData$plotID))
# create spatial polygons from coordinates
polys <- SpatialPolygons(mapply(function(poly, id) {
  xy <- matrix(poly, ncol=2, byrow=TRUE)
  Polygons(list(Polygon(xy)), ID=id)
}, 
split(square, row(square)), ID),
proj4string=CRS(as.character("+proj=longlat +datum=NAD83 +no_defs")))

plot(polys)

# Create SpatialPolygonDataFrame -- this step is required to output multiple polygons.
rownames(spatialData) <- spatialData[,1]
polys.df <- SpatialPolygonsDataFrame(polys, spatialData)
#polys.df <- SpatialPolygonsDataFrame(polys, data.frame(id=ID, row.names=ID))
# write the shapefiles 
rgdal::writeOGR(obj = polys.df,
                dsn = "NEONtickplots",
                layer = "NEONtickplots",
                driver = "ESRI Shapefile",
                overwrite_layer = TRUE)

#all tick plots (except GUAN) are 40x40
# plot.size <- rep(40, each=304)
# create map of plots

# symbols(spatialData$easting,
        # spatialData$northing,
        # squares=plot.size, inches=F,
        # xlab="Easting", ylab="Northing")

#we need to convert the coordinates of the centroids into polygons. This can be done in several ways, here's one:

# lon_lat_NEON <- data.frame(spatialData$decimalLongitude,spatialData$decimalLatitude)
# names(lon_lat_NEON) <- c("lon", "lat")
# lon_lat_NEON$lon <- as.numeric(lon_lat_NEON$lon)
# lon_lat_NEON$lat <- as.numeric(lon_lat_NEON$lat)
# #THIS FUNCTION DOES NOT WORK CORRECTLY!!!
# #NEON_tick_polygons <- pts2poly_centroids(lon_lat_NEON, 20, crs = "+proj=longlat +datum=NAD83 +no_defs")
# 
# #create an actual list of polygons so we can convert to spatial polygons
# list_poly <- list()
# for (i in 1:length(NEON_tick_polygons)){
#   NEON_tick_polygons1 <- Polygon(NEON_tick_polygons[[i]][[1]])
#   list_poly[[length(list_poly) + 1]] <- NEON_tick_polygons1
# }
# 
# list_polys <- lapply(seq_along(list_poly), function(i) Polygons(list(list_poly[[i]]), ID = spatialData[i,1]))
# 
# #convert to spatial polygons
# spatial_polys <- SpatialPolygons(list_polys, proj4string=CRS("+proj=longlat +datum=NAD83 +no_defs"))
# 
# #spatial_polys_df <- SpatialPolygonsDataFrame(spatial_polys, data.frame(x = rep(NA, length(spatial_polys)), row.names = spatialData[,1]))
# rownames(spatialData) <- spatialData[,1]
# spatial_polys_df <- SpatialPolygonsDataFrame(spatial_polys, spatialData)
# 
# #write shapefile to the working directory
# rgdal::writeOGR(obj = spatial_polys_df,
#                 dsn = "NEONtickplots",
#                 layer = "NEONtickplots",
#                 driver = "ESRI Shapefile",
#                 overwrite_layer = TRUE)

#USING THE SHAPEFILE FROM NEON BECAUSE NOTHING ELSE HAS WORKED
# tickshape <- readOGR("C:/Users/bigfo/OneDrive/Desktop/ENM Tutorial/ENM A Americanum NEON/TimeSpecific/tick_plot_3000buffer/tickPlot.shp")
tickshape <- readOGR("C:/Users/bigfo/OneDrive/Desktop/ENM Tutorial/ENM A Americanum NEON/TimeSpecific/NEON site shapefile w R script/corrected_NEON_tickplots/NEONtickplots/PRISMNEONtickplots/NEONtickplots__FeatureEnvelo.shp")

#biggerpolys <- readOGR("C:/Users/bigfo/OneDrive/Desktop/ENM Tutorial/ENM A Americanum NEON/TimeSpecific/arcgis_tickplots/smallerpolys/biggerpolys.shp")
firerast <- raster("C:/Users/bigfo/OneDrive/Desktop/ENM Tutorial/ENM A Americanum NEON/TimeSpecific/fire_rasters/fire_2021.tif")
par(bg = 'blue')
plot(tickshape)
RStmean_proj <- projectRaster(RStmean, crs = CRS("+proj=longlat +datum=WGS84 +no_defs"))
temp_mask <- raster::mask(RStmean_proj,test1)
RStmean_proj_res <- disaggregate(temp_mask[[1]], fact = 45)
tick_temp_rast <- raster::mask(RStmean_proj_res,tickshape)
plot(temp_mask[[1]])
library(terra)
#tickshape <- vect("C:/Users/bigfo/OneDrive/Desktop/ENM Tutorial/ENM A Americanum NEON/TimeSpecific/NEON site shapefile w R script/corrected_NEON_tickplots/NEONtickplots/PRISMNEONtickplots/NEONtickplots__FeatureEnvelo.shp")
firerast <- rast("C:/Users/bigfo/OneDrive/Desktop/ENM Tutorial/ENM A Americanum NEON/TimeSpecific/fire_rasters/fire_2021.tif")
#add this to preserve names names = prism_archive_ls()
RStmean <- rast(RStmean)

tickshape <- project(tickshape, RStmean)
plot(temp_mask[[1]])
plot(tickshape, add = TRUE)
temp_mask <- terra::mask(RStmean, tickshape)
temp_mask <- terra::crop(RStmean_proj, firerast)

#writeRaster(temp_mask[[1]],filename = "C:/Users/bigfo/OneDrive/Desktop/ENM Tutorial/ENM A Americanum NEON/TimeSpecific/raster_test/rast1.tif")
#checking to see which plots are missing from the shapefile
check.plots <- split(seq_along(tickshape@data[["plotID"]]), factor(tickshape@data[["plotID"]], spatialData$plotID))
#remove those that are missing
remove.list <- paste(c("ABBY_077", "BLAN_002", "DELA_007", "DELA_016", "DSNY_002", "DSNY_014", "GRSM_005",
                       "GUAN_004", "HARV_020", "HARV_025", "JERC_005", "JERC_034", "JERC_044", "ONAQ_003",
                       "ONAQ_008", "ONAQ_010", "ORNL_006", "OSBS_003", "OSBS_005", "OSBS_022", "SCBI_012",
                       "SCBI_039", "SOAP_007", "STER_025", "UNDE_003", "YELL_005", "BARR_021", "JERC_030"), collapse = '|')

spatialData1 <- spatialData[ grep(remove.list, spatialData$plotID, invert = TRUE) , ]
#check that everything matches
check.plots <- split(seq_along(tickshape@data[["plotID"]]), factor(tickshape@data[["plotID"]], spatialData1$plotID))
#transforming the projection to that used in the PRISM data doesnt work
#tickshape1 <- sp::spTransform(tickshape,CRS("+proj=longlat +datum=NAD83 +no_defs"))

#let's try raising the resolution
target_rast <- raster(extent(temp_mask), res = (res(temp_mask)/100) , crs = st_crs(temp_mask)$proj4string) 

RStmean_proj_res <- resample(temp_mask, target_rast, method = 'bilinear')
tickshape$id <- "1"
nwd <- gUnaryUnion(tickshape, id = NULL, checkValidity = NULL)
cells_nona <- cellFromPolygon(RStmean_proj_res[[1]],nwd)
cellsids <- 1: ncell(RStmean_proj_res)
cell_na <- cellsids[-cells_nona[[1]]]
RStmean_proj_res[cell_na] <- NA

plot(RStmean_proj[[1]])
lines(nwd)
plot(temp_mask[[1]])
tick_mask <- rasterize(nwd,RStmean_proj[[1]], mask = T)
colnames(tickshape@data)
#rasterize, last attempt
rast_tick <- rasterize(tickshape,temp_mask[[1]], field = "latitude")
plot(rast_tick)
#add vectors of zeros for each year, then we can replace those instances with fire that have them
#See sdf_trimmed for fires. There are 68 instances total.
tickshape@data$fire_pres_2013 <- rep(0,235)
tickshape@data$fire_pres_2014 <- rep(0,235)
tickshape@data$fire_pres_2015 <- rep(0,235)
tickshape@data$fire_pres_2016 <- rep(0,235)
tickshape@data$fire_pres_2017 <- rep(0,235)
tickshape@data$fire_pres_2018 <- rep(0,235)
tickshape@data$fire_pres_2019 <- rep(0,235)
tickshape@data$fire_pres_2020 <- rep(0,235)
tickshape@data$fire_pres_2021 <- rep(0,235)
tickshape@data$fire_pres_2022 <- rep(0,235)
tickshape@data$fire_pres_2023 <- rep(0,235)
                               
#for 2013, there were three instances of fire at konz 1, 4, and 9
tickshape@data$plotID
tickshape@data$fire_pres_2013[[175]] <- 1
tickshape@data$fire_pres_2013[[177]] <- 1
tickshape@data$fire_pres_2013[[179]] <- 1

rast_tick <- rasterize(tickshape,temp_mask[[1]], field = "fire_pres_2013", update = TRUE)
plot(rast_tick)
writeRaster(rast_tick,filename = "C:/Users/bigfo/OneDrive/Desktop/ENM Tutorial/ENM A Americanum NEON/TimeSpecific/raster_test/rast1.tif")
