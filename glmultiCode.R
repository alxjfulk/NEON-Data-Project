library(glmulti)
library(dplyr)
library(effects)
library(prism)
library(sp)
library(reshape2)
library(raster)
library(rJava)
library(rgdal)
library(tictoc)
library(sf)
library(rgeos)
library(geoNEON)
#install_github('NEONScience/NEON-geolocation/geoNEON', dependencies=TRUE)
combinedraster <- rasterFromXYZ(test1)
test1 <- data.frame(cbind(as.numeric(combineddata13$decimalLatitude),as.numeric(combineddata13$decimalLongitude),combineddata13$fireSeverity))
combinedraster <- raster(test1)
combinedraster <- raster(combineddata13)
tickshape <- st_read(file.path("C:/Users/bigfo/Downloads/tickPlots/tickPlots/tickPlot.shp"))


WGS84 <- sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
tickshape1 <- sp::spTransform(tickshape,"+proj=longlat +datum=NAD83 +no_defs")
prism_tickshape <- mask(RStmean,tickshape)
prism_tickshape <- crop(RStmean,tickshape)
writeRaster(prism_tickshape, filename=names(prism_tickshape), bylayer = TRUE)
plot(prism_tickshape)

tickshape <- readOGR("C:/Users/bigfo/Downloads/tickPlots/tickPlots/tickPlot.shp")
tickshape1 <- sp::spTransform(tickshape,CRS("+proj=longlat +datum=NAD83 +no_defs"))
tickshape$id <- "1"
nwd <- gUnaryUnion(tickshape, id = NULL, checkValidity = NULL)
cells_nona <- cellFromPolygon(RStmean,nwd)
cellsids <- 1: ncell(RStmean)
cell_na <- cellsids[-cells_nona[[1]]]
RStmean[cell_na] <- NA
#"nlcdClass","elevation","totalSampledArea","fireSeverity","ppt","tmean","vpdmin","daysSinceLastBurn",
#"mean_slopeAspect","mean_slopeGradient","NDVI","countMammalSite","countBirdSite","soilMoisture","soilTemp","soilInCaClpH","organicCPercent"
####GLMULTI####
combineddata13 <- read.csv("C:/Users/bigfo/OneDrive/Desktop/ENM Tutorial/ENM A Americanum NEON/TimeSpecific/NEONdata.csv")
#First, remove NAs in variables of interest and subset data to include only one observation per time point per location
#removedNAcombineddata <- combineddata13[!is.na(combineddata13$NDVI) & !is.na(combineddata13$ppt) & !is.na(combineddata13$tmean) & !is.na(combineddata13$vpdmin) & !is.na(combineddata13$soilMoisture),]
removedNAcombineddata <- combineddata13[!is.na(combineddata13$tdmean) & !is.na(combineddata13$ppt) & !is.na(combineddata13$tmean) & !is.na(combineddata13$vpdmin) & !is.na(combineddata13$tmax) & !is.na(combineddata13$tmin) & !is.na(combineddata13$vpdmax),]

pres_obs <- removedNAcombineddata[removedNAcombineddata$individualCount > 0,]

pres_obs_rm <- distinct(pres_obs, collectDate, plotID, .keep_all = TRUE)

oneObsPerTimePerLoc <- distinct(removedNAcombineddata, collectDate, plotID, .keep_all = TRUE)
#convert to presence/absence data
oneObsPerTimePerLoc$pres_abs <-  ifelse(oneObsPerTimePerLoc$individualCount > 0, 1, 0)
# Randomly select n observations from the dataset
trainingsubset <- oneObsPerTimePerLoc %>% sample_n(nrow(oneObsPerTimePerLoc)/2)
#take those observations not included in teh training subset for testing
testingsubset <- anti_join(oneObsPerTimePerLoc,trainingsubset)

glmModel <- glmulti(y = "pres_abs", xr = c("ppt","tmean","vpdmin","tmax","tdmean","vpdmax","tmin"), data = trainingsubset, crit = "aicc", method = "g",family = binomial(link = "logit"))
 #family = binomial(link = "logit")
consensusobj <- consensus(list(glmModel,glmModel1,glmModel2,glmModel3,glmModel4), confsetsize = 100)
# print(glmModel)
# plot(glmModel, type = "p")
# plot(glmModel, type = "w")
# plot(glmModel, type = "s")

plot(consensusobj, type = "p")
plot(consensusobj, type = "w")
plot(consensusobj, type = "s")

# tableofModels <- weightable(glmModel)
# bestmodel <- glmModel@formulas[1]
# fittedbestmodel <- glmModel@objects[1]
# modelsummary <- summary(glmModel)
# 
# bestmodelpredict <- predict(glmModel, select = 1)

tableofModels <- weightable(consensusobj)
bestmodel <- consensusobj@formulas[1]
fittedbestmodel <- consensusobj@objects[1]
modelsummary <- summary(consensusobj)

#bestmodelpredict <- predict(consensusobj, select = 1)

# plot(effects::allEffects(glmModel@objects[1]),
#      lines = list(multiline = T),
#      confint = list(style = "auto"))

bestmodel1 <- glm(individualCount ~ 1 + ppt + tmean + vpdmin + vpdmax + tmin +
                    vpdmin:ppt + tmax:vpdmin + tdmean:ppt + tdmean:tmax + vpdmax:tmean +
                    tmin:ppt + tmin:tmean + tmin:vpdmin + tmin:tmax + tmin:tdmean, data = trainingsubset, family = binomial(link = "logit"))
summary(bestmodel1)

prism_set_dl_dir("C:/Users/bigfo/OneDrive/Desktop/ENM Tutorial/ENM A Americanum NEON/TimeSpecific/PRISM Data/monthly/MeanTemperature")
#load raster data for a single year
RStmean <- pd_stack(prism_archive_subset("tmean", temp_period = "monthly", years = c(2018), mon = c(1,2,3,4,5,6,7,8,9,10,11,12)))

prism_set_dl_dir("C:/Users/a426f262/OneDrive - University of Kansas/Desktop/School/Research/ENM Tutorial/A Americanum 2023/MinTemperature")
RStmin <- pd_stack(prism_archive_subset("tmin", temp_period = "daily", minDate = "2018-11-01", maxDate = "2019-02-01"))

prism_set_dl_dir("C:/Users/a426f262/OneDrive - University of Kansas/Desktop/School/Research/ENM Tutorial/A Americanum 2023/MaxTemperature")
RStmax <- pd_stack(prism_archive_subset("tmax", temp_period = "daily", minDate = "2018-11-01", maxDate = "2019-02-01"))

prism_set_dl_dir("C:/Users/a426f262/OneDrive - University of Kansas/Desktop/School/Research/ENM Tutorial/A Americanum 2023/MinVPD")
RSvpdmin <- pd_stack(prism_archive_subset("vpdmin", temp_period = "daily", minDate = "2018-11-01", maxDate = "2019-02-01"))

prism_set_dl_dir("C:/Users/a426f262/OneDrive - University of Kansas/Desktop/School/Research/ENM Tutorial/A Americanum 2023/MaxVPD")
RSvpdmax <- pd_stack(prism_archive_subset("vpdmax", temp_period = "daily", minDate = "2018-11-01", maxDate = "2019-02-01"))

prism_set_dl_dir("C:/Users/a426f262/OneDrive - University of Kansas/Desktop/School/Research/ENM Tutorial/A Americanum 2023/Precipitation")
RSppt <- pd_stack(prism_archive_subset("ppt", temp_period = "daily", minDate = "2018-11-01", maxDate = "2019-02-01"))

prism_set_dl_dir("C:/Users/a426f262/OneDrive - University of Kansas/Desktop/School/Research/ENM Tutorial/A Americanum 2023/MeanDewpointTemperature")
RStdmean <- pd_stack(prism_archive_subset("tdmean", temp_period = "daily", minDate = "2018-11-01", maxDate = "2019-02-01"))
#plot(RStmean1)
RStmean1 <- RStmean[[85]]
plot(RStdmean1)
names(RStmean1) <- "tmean"

RStmin1 <- RStmin[[85]]
names(RStmin1) <- "tmin"

RStmax1 <- RStmax[[85]]
names(RStmax1) <- "tmax"

RSvpdmin1 <- RSvpdmin[[85]]
names(RSvpdmin1) <- "vpdmin"

RSvpdmax1 <- RSvpdmax[[85]]
names(RSvpdmax1) <- "vpdmax"

RSppt1 <- RSppt[[85]]
names(RSppt1) <- "ppt"

RStdmean1 <- RStdmean[[85]]
names(RStdmean1) <- "tdmean"
#stack raster data into one object with 4 layers
#PRISMDataStack <- stack(meanRStmin2018,meanRStmean2018,meanRStmax2018,meanRSvpdmin2018,meanRSvpdmax2018,meanRSppt2018,meanRStdmean2018)
PRISMDataStack <- stack(RStmin1,RStmean1,RStmax1,RSvpdmin1,RSvpdmax1,RSppt1,RStdmean1)
#names(PRISMDataStack)
#PRISMDataStackDF <- data.frame(rasterToPoints(PRISMDataStack))
modelprediction <- predict(PRISMDataStack, model = bestmodel1, progress = "window")
#dev.off()
plot(modelprediction, maxpixels = 1000000, col = rainbow(15), main = "December 28th, 2018", zlim = c(-5,15))

# #set download directory so that R can find the files we have downloaded
# prism_set_dl_dir("C:/Users/bigfo/OneDrive/Desktop/ENM Tutorial/ENM A Americanum NEON/TimeSpecific/PRISM Data/MeanTemperature")
# #load PRISM data as rasters
# prism_archive_ls()
# RStmean <- pd_stack(prism_archive_subset("tmean", temp_period = "daily", minDate = "2015-01-01", maxDate = "2017-01-01"))
# proj4string(RStmean)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

#convert to data frame for easier data extraction, NOT NEEDED FOR NOW:
# tmeanDF <- data.frame(rasterToPoints(RStmean))
# tmeanDF <- melt(tmeanDF, c("x", "y"))
# 
# names(tmeanDF)[1:2] <- c("lon", "lat") #rename columns
# head(tmeanDF)
