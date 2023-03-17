# install.packages('neonUtilities')
library(neonUtilities)
library(dplyr)
library(tidyr)

####CLEANING DATA####
#*TICK DATA -----
#for combining the downloaded data into tables:
stackByTable(filepath="C:/Users/bigfo/OneDrive/Desktop/school/neon data/tick site data/NEON_count-ticks.zip")
#load the stacked data:
df1 <- read.csv("C:/Users/bigfo/OneDrive/Desktop/school/neon data/tick site data/NEON_count-ticks/stackedFiles/tck_taxonomyProcessed.csv")
df2 <- read.csv("C:/Users/bigfo/OneDrive/Desktop/school/neon data/tick site data/NEON_count-ticks/stackedFiles/tck_fielddata.csv")

#load the site tick data (https://data.neonscience.org/data-products/DP1.10093.001)
# tickdata <- loadByProduct(dpID = 'DP1.10093.001', site = 'KONZ')
# tickdata <- loadByProduct(dpID = 'DP1.10093.001', site = 'OAES')
# tickdata <- loadByProduct(dpID = 'DP1.10093.001', site = 'SOAP')
tickdata <- loadByProduct(dpID = 'DP1.10093.001')
#see what is contained in the data
View(tickdata$variables_10093)

#seems like we may need nlcdClass data from the tck_fielddata, but nothing else 
#since the other data is contained within tck_taxonomyProcessed
View(tickdata$tck_taxonomyProcessed)
View(tickdata$tck_fielddata)

#let's try a different approach taken from this stackoverflow link: https://stackoverflow.com/questions/64992027/adding-column-to-a-dataframe-in-r-based-on-matching-conditions-in-another-datafr
df1 <- tickdata$tck_taxonomyProcessed
df2 <- tickdata$tck_fielddata
#TO CHECK IF DIRECTLY DOWNLOADED DATA IS SAME AS DATA ThROUGh R, USE THIS FUNCTION:
#isTRUE( all.equal(df2,df1) )
#here we specify those rows that should match in each dataframe and the column that we want to copy from.
#this step preserves data from the field data that may be needed for analysis in teh taxonomic data
#for example, the type of enviornment that the ticks are sampled from
df1$nlcdClass <- df2[match(paste(df1$plotID),paste(df2$plotID)),"nlcdClass"]
df1$elevation <- df2[match(paste(df1$plotID),paste(df2$plotID)),"elevation"]
df1$decimalLatitude <- df2[match(paste(df1$plotID),paste(df2$plotID)),"decimalLatitude"]
df1$decimalLongitude <- df2[match(paste(df1$plotID),paste(df2$plotID)),"decimalLongitude"]
df1$totalSampledArea <- df2[match(paste(df1$plotID),paste(df2$plotID)),"totalSampledArea"]

#summing by males and females by year. First replace male and female as adult
df1$sexOrAge <-replace(df1$sexOrAge,df1$sexOrAge=="Male","Adult")
df1$sexOrAge <-replace(df1$sexOrAge,df1$sexOrAge=="Female","Adult")
#reformat collectDate column to make the aggregtion easier 
df1$collectDate <- format(as.POSIXct(df1$collectDate,format='%Y-%m-%d %H:%M:%S'),format='%Y-%m-%d %H:%M:%S')


# View(df1)

#now we need to remove the entries from the field data that had ticks collected so that we avoid duplicate data when merging
df2zeros <- df2[(df2$adultCount == 0 | is.na(df2$adultCount)) & (df2$nymphCount == 0 | is.na(df2$nymphCount)) & (df2$larvaCount == 0 | is.na(df2$larvaCount)) & !is.na(df2$totalSampledArea),]
# View(df2zeros)
#now we need to remove unneeded columns before merging the two dataframes (https://stackoverflow.com/questions/5234117/how-to-drop-columns-by-name-in-a-data-frame)
df1_trimmed <- subset(df1, select=-c(identifiedDate,sampleID,subsampleID,acceptedTaxonID,taxonRank,family,subfamily,tribe,
                      subtribe,genus,subgenus,specificEpithet,infraspecificEpithet,identificationQualifier,identificationReferences,
                      identificationProtocolVersion,scientificNameAuthorship,sampleCondition,archiveMedium,
                      archiveFacilityID,deprecatedVialID,identifiedBy,laboratoryName,remarks,publicationDate,release))
# View(df1_trimmed)
#let's see what an aggregation will give us
View(aggregate(individualCount~sexOrAge + collectDate + scientificName + namedLocation + plotID + nlcdClass + elevation + decimalLatitude + decimalLongitude + totalSampledArea,data=subset(df1_trimmed,sexOrAge == c('Adult')),sum))
#looks right! now we need to remove the rows that have adult in df1_trimmed and then add the aggregated data back to our trimmed dataframe
adultcount <- aggregate(individualCount~sexOrAge + collectDate + scientificName + namedLocation + plotID + nlcdClass + elevation + decimalLatitude + decimalLongitude + totalSampledArea,data=subset(df1_trimmed,sexOrAge == c('Adult')),sum)
df1_trimmed <- df1_trimmed[df1_trimmed$sexOrAge != 'Adult',]
df1_trimmed <- dplyr::bind_rows(df1_trimmed,adultcount)
#it's the same! Now we just need to remove the adult entries from the trimmed dataset and append the aggregated data
df2zeros_trimmed <- subset(df2zeros, select=-c(domainID,siteID,plotType,geodeticDatum,coordinateUncertainty,
                                               elevationUncertainty,samplingImpractical,biophysicalCriteria,eventID,
                                               sampleID,sampleCode,samplingMethod,targetTaxaPresent,adultCount,nymphCount,
                                               larvaCount,sampleCondition,samplingProtocolVersion,measuredBy,remarks,
                                               publicationDate,release))
# View(df2zeros_trimmed)

#we have removed unneeded columns, now we need to combine the dataframes (https://stackoverflow.com/questions/53004125/concatenate-multiple-data-frames-with-different-columns-in-r)
df1_trimmed$collectDate <- as.Date(df1_trimmed$collectDate)
mergetest <- dplyr::bind_rows(df1_trimmed,df2zeros_trimmed)
#need to remove ".tck" from the namedLocation so that things line up when combining with the fire data
mergetest$namedLocation <- gsub(".tck","",as.character(mergetest$namedLocation))
View(mergetest)

#save to csv
# write.csv(mergetest,"D:/NEONexample/NEONtickdatacleaned.csv",row.names = FALSE)

#*FIRE DATA ----
#for combining the downloaded data into tables:
stackByTable(filepath="C:/Users/bigfo/OneDrive/Desktop/school/neon data/site management/NEON_site-mgt-and-event-report.zip")
#load the stacked data:
sdf <- read.csv("C:/Users/bigfo/OneDrive/Desktop/school/neon data/site management/NEON_site-mgt-and-event-report/stackedFiles/sim_eventData.csv")

#load the site event reporting data (https://data.neonscience.org/data-products/DP1.10111.001)
# sitedata <- loadByProduct(dpID = 'DP1.10111.001', site = 'KONZ')
# sitedata <- loadByProduct(dpID = 'DP1.10111.001', site = 'OAES')
# sitedata <- loadByProduct(dpID = 'DP1.10111.001', site = 'SOAP')
sitedata <- loadByProduct(dpID = 'DP1.10111.001')

#to see where the useful data is contained, we view the variables:
View(sitedata$variables_10111)

#all of the site data that is relevent to us is contained within sim_eventData
#notice that the locationID and methodTypeChoice what we will be using to identify 
#those tick plots that are burned in a controlled manner. However, let's keep wildfires in for now
# View(sitedata$sim_eventData)

sdf <- sitedata$sim_eventData
#TO CHECK IF DIRECTLY DOWNLOADED DATA IS SAME AS DATA ThROUGh R, USE THIS FUNCTION:
#isTRUE( all.equal(df2,df1) )

#subset fire data
# sdf_fire <- sdf[sdf$eventType == 'fire',]
#to include ONLY controlled burns
sdf_fire <- sdf[sdf$methodTypeChoice == 'fire-controlledBurn',]
View(sdf_fire)

#separate the locationID column into multiple rows so that we can extract the tickPlots that are burned
sdf_fire_separate <- tidyr::separate_rows(sdf_fire,locationID, sep = ",")
View(sdf_fire_separate)
# 
#subset to include only tickPlots (https://stackoverflow.com/questions/13043928/selecting-data-frame-rows-based-on-partial-string-match-in-a-column)
sdf_fire_separate_tick <- sdf_fire_separate[grep("tickPlot", sdf_fire_separate$locationID),]
View(sdf_fire_separate_tick)

#remove unneeded columns
sdf_trimmed <- subset(sdf_fire_separate_tick, select=-c(domainID,siteID,namedLocation,ongoingEvent,
                                                        estimatedOrActualDate,dateRemarks,eventID,samplingProtocolVersion,
                                                        eventType,name,scientificName,otherScientificName,biomassRemoval,
                                                        minQuantity,maxQuantity,quantityUnit,reporterType,remarks,recordedBy,
                                                        dataQF,publicationDate,release))
View(sdf_trimmed)

#change locationID to namedLocation and startDate to so that they match with the other dataframes
colnames(sdf_trimmed)[2] <- "namedLocation"
colnames(sdf_trimmed)[3] <- "collectDate"

#remove spaces in new namedLocation so we can order later
sdf_trimmed$namedLocation <- gsub(" ", "", sdf_trimmed$namedLocation, fixed = TRUE)

#save to csv
# write.csv(sdf_trimmed,"D:/NEONexample/NEONfiredatacleaned.csv",row.names = FALSE)

mergetest1 <- dplyr::bind_rows(mergetest,sdf_trimmed)
View(mergetest1)
#now that we have fused these data, we can remove the '.tickPlot' to make merging easier later on
mergetest1$namedLocation <- gsub(".tickPlot","",as.character(mergetest1$namedLocation))


#*MAMMAL DATA ----
#for combining the downloaded data into tables:
stackByTable(filepath="C:/Users/bigfo/OneDrive/Desktop/school/neon data/mammal site data/NEON_count-small-mammals.zip")
#load the stacked data:
mammaldata_pertrap <- read.csv("C:/Users/bigfo/OneDrive/Desktop/school/neon data/mammal site data/NEON_count-small-mammals/stackedFiles/mam_pertrapnight.csv")

mammaldata <- loadByProduct(dpID = 'DP1.10072.001', site = 'KONZ')
mammaldata <- loadByProduct(dpID = 'DP1.10072.001')
View(mammaldata$variables_10072)

mammaldata_pertrap <- mammaldata$mam_pertrapnight
#TO CHECK IF DIRECTLY DOWNLOADED DATA IS SAME AS DATA ThROUGh R, USE THIS FUNCTION:
#isTRUE( all.equal(df2,df1) )
View(mammaldata_pertrap)
#we need to remove all of the events that had no successful trapping
mammaldata_pertrap1 <- mammaldata_pertrap[!grepl("0",mammaldata_pertrap$trapStatus),]
mammaldata_pertrap1 <- mammaldata_pertrap1[!grepl("1",mammaldata_pertrap1$trapStatus),]
mammaldata_pertrap1 <- mammaldata_pertrap1[!grepl("2",mammaldata_pertrap1$trapStatus),]
mammaldata_pertrap1 <- mammaldata_pertrap1[!grepl("3",mammaldata_pertrap1$trapStatus),]
mammaldata_pertrap1 <- mammaldata_pertrap1[!grepl("6",mammaldata_pertrap1$trapStatus),]
View(mammaldata_pertrap1)
#Now we want to aggregate based on the siteID since it will remain the same if we are looking in the same site while keeping track of the scientific name, plotID, and date
View(aggregate(siteID~ collectDate + plotID + scientificName + nlcdClass, data = mammaldata_pertrap1, length))
#if you want to aggregate including the environment type that the mammal is caught in, use the following line instead of the uncommented one
#finalmammaldata <- aggregate(siteID~ collectDate + plotID + scientificName + nlcdClass, data = mammaldata_pertrap1, length)
finalmammaldata1 <- aggregate(siteID~ collectDate + plotID + scientificName, data = mammaldata_pertrap1, length)
colnames(finalmammaldata1)[4] <- "individualCount"
colnames(finalmammaldata1)[2] <- "namedLocation"
finalmammaldata2 <- finalmammaldata1[finalmammaldata1$namedLocation == mergetest$namedLocation,]
View(finalmammaldata)
#now this data is ready to be combined with the other datasets
mergetest2 <- dplyr::bind_rows(mergetest2,finalmammaldata)

#*TEMP DATA ----
#for combining the downloaded data into tables:
stackByTable(filepath="C:/Users/bigfo/OneDrive/Desktop/school/neon data/trip asp air temp data/NEON_site-mgt-and-event-report.zip")
#load the stacked data:
tempdataset <- read.csv("C:/Users/bigfo/OneDrive/Desktop/school/neon data/trip asp air temp data/NEON_site-mgt-and-event-report/stackedFiles/wss_daily_temp.csv")

# tempdata <- loadByProduct(dpID = 'DP4.00001.001', site = 'KONZ')
tempdata <- loadByProduct(dpID = 'DP4.00001.001')
View(tempdata$variables_00001)

tempdataset <- tempdata$wss_daily_temp
#TO CHECK IF DIRECTLY DOWNLOADED DATA IS SAME AS DATA ThROUGh R, USE THIS FUNCTION:
#isTRUE( all.equal(df2,df1) )
tempdataset$date <- format(tempdataset$date, '%Y-%m')
#View(aggregate(wssTempTripleMean~ date + siteID + domainID,data=tempdataset,mean))

tempdataset1 <- aggregate(wssTempTripleMean~ date + siteID,data=tempdataset,mean)
#change col names for merging
View(tempdataset1)
colnames(tempdataset1)[2] <- "namedLocation"
colnames(tempdataset1)[1] <- "collectDate"
#tempdataset2 <- tempdataset1[tempdataset1$namedLocation == mergetest$namedLocation,]
#THIS MIGHT WORK, BUT I HAVEN'T TESTED:
library(stringr)
library(dplyr)

tempdataset1 %>%
  filter(str_detect(row.names(tempdataset1$namedLocation != mergetest$namedlocation), paste(remove_if, collapse = "|"), negate = TRUE))
#
mergetest2$collectDate <- as.Date(mergetest2$collectDate, format = '%Y-%m-%d')
tempdataset1$collectDate <- as.Date(paste0(tempdataset1$collectDate, "-01"), format = '%Y-%m-%d')
mergetest3 <- dplyr::bind_rows(mergetest2,tempdataset1)
View(mergetest3)

#order by site, then date
finalmerge <- mergetest3[order(mergetest3$namedLocation,mergetest3$collectDate),]
View(finalmerge)
# ####TEMP DATASET THAT I FOUND###
# tempdata <- loadByProduct(dpID = 'DP1.00098.001', site = 'KONZ')
# View(tempdata$RH_30min)

write.csv(finalmerge,"D:/NEONexample/full_allcounts.csv",row.names = FALSE)




# *PRECIPITATION DATA ----
#for combining the downloaded data into tables:
stackByTable(filepath="/Users/a426f262/Downloads/Directly Downloaded Data/NEON_precipitation.zip")
#load the stacked data:
tempdataset <- read.csv("C:/Users/bigfo/OneDrive/Desktop/school/neon data/trip asp air temp data/NEON_site-mgt-and-event-report/stackedFiles/wss_daily_temp.csv")










#order by site, then date
finalmerge <- mergetest3[order(mergetest3$namedLocation,mergetest3$collectDate),]
View(finalmerge)
# ####TEMP DATASET THAT I FOUND###
# tempdata <- loadByProduct(dpID = 'DP1.00098.001', site = 'KONZ')
# View(tempdata$RH_30min)

write.csv(finalmerge,"D:/NEONexample/full_allcounts.csv",row.names = FALSE)

####CLEANING DATA ONLY KNOWN SPECIES####
#*TICK DATA ----
#for combining the downloaded data into tables:
stackByTable(filepath="C:/Users/bigfo/OneDrive/Desktop/school/neon data/tick site data/NEON_count-ticks.zip")
#load the stacked data:
df1 <- read.csv("C:/Users/bigfo/OneDrive/Desktop/school/neon data/tick site data/NEON_count-ticks/stackedFiles/tck_taxonomyProcessed.csv")
df2 <- read.csv("C:/Users/bigfo/OneDrive/Desktop/school/neon data/tick site data/NEON_count-ticks/stackedFiles/tck_fielddata.csv")

#load the site tick data (https://data.neonscience.org/data-products/DP1.10093.001)
# tickdata <- loadByProduct(dpID = 'DP1.10093.001', site = 'KONZ')
# tickdata <- loadByProduct(dpID = 'DP1.10093.001', site = 'OAES')
# tickdata <- loadByProduct(dpID = 'DP1.10093.001', site = 'SOAP')
tickdata <- loadByProduct(dpID = 'DP1.10093.001')

#see what is contained in the data
View(tickdata$variables_10093)

#seems like we may need nlcdClass data from the tck_fielddata, but nothing else 
#since the other data is contained within tck_taxonomyProcessed
View(tickdata$tck_taxonomyProcessed)
View(tickdata$tck_fielddata)

#let's try a different approach taken from this stackoverflow link: https://stackoverflow.com/questions/64992027/adding-column-to-a-dataframe-in-r-based-on-matching-conditions-in-another-datafr
df1 <- tickdata$tck_taxonomyProcessed
df2 <- tickdata$tck_fielddata
#TO CHECK IF DIRECTLY DOWNLOADED DATA IS SAME AS DATA ThROUGh R, USE THIS FUNCTION:
#isTRUE( all.equal(df2,df1) )

#here we specify those rows that should match in each dataframe and the column that we want to copy from.
#this step preserves data from the field data that may be needed for analysis in teh taxonomic data
#for example, the type of enviornment that the ticks are sampled from
df1$nlcdClass <- df2[match(paste(df1$plotID),paste(df2$plotID)),"nlcdClass"]
df1$elevation <- df2[match(paste(df1$plotID),paste(df2$plotID)),"elevation"]
df1$decimalLatitude <- df2[match(paste(df1$plotID),paste(df2$plotID)),"decimalLatitude"]
df1$decimalLongitude <- df2[match(paste(df1$plotID),paste(df2$plotID)),"decimalLongitude"]
df1$totalSampledArea <- df2[match(paste(df1$plotID),paste(df2$plotID)),"totalSampledArea"]

#summing by males and females by year. First replace male and female as adult
df1$sexOrAge <-replace(df1$sexOrAge,df1$sexOrAge=="Male","Adult")
df1$sexOrAge <-replace(df1$sexOrAge,df1$sexOrAge=="Female","Adult")
#reformat collectDate column to make the aggregtion easier 
#IDEALLY we would include the hour, minute, and second of collection however we cannot 
#df1$collectDate <- format(as.POSIXct(df1$collectDate,format='%Y-%m-%d %H:%M:%S'),format='%Y-%m-%d %H:%M:%S')
df1$collectDate <- format(as.POSIXct(df1$collectDate,format='%Y-%m-%d %H:%M:%S'),format='%Y-%m-%d %H:%M:%S')
df2$collectDate <- format(as.POSIXct(df2$collectDate,format='%Y-%m-%d %H:%M:%S'),format='%Y-%m-%d %H:%M:%S')


#REMOVE TICK COUNTS WHERE SPECIES COULD NOT BE DETERMINED
df1 <- df1[df1$taxonRank == 'species',]

# View(df1)

#now we need to remove the entries from the field data that had ticks collected so that we avoid duplicate data when merging
df2zeros <- df2[(df2$adultCount == 0 | is.na(df2$adultCount)) & (df2$nymphCount == 0 | is.na(df2$nymphCount)) & (df2$larvaCount == 0 | is.na(df2$larvaCount)) & !is.na(df2$totalSampledArea),]
# View(df2zeros)
#now we need to remove unneeded columns before merging the two dataframes (https://stackoverflow.com/questions/5234117/how-to-drop-columns-by-name-in-a-data-frame)
df1_trimmed <- subset(df1, select=-c(identifiedDate,sampleID,subsampleID,acceptedTaxonID,taxonRank,family,subfamily,tribe,
                                     subtribe,genus,subgenus,specificEpithet,infraspecificEpithet,identificationQualifier,identificationReferences,
                                     identificationProtocolVersion,scientificNameAuthorship,sampleCondition,archiveMedium,
                                     archiveFacilityID,deprecatedVialID,identifiedBy,laboratoryName,remarks,publicationDate,release))
# View(df1_trimmed)
#let's see what an aggregation will give us
View(aggregate(individualCount~sexOrAge + collectDate + scientificName + namedLocation + plotID + nlcdClass + elevation + decimalLatitude + decimalLongitude + totalSampledArea,data=subset(df1_trimmed,sexOrAge == c('Adult')),sum))
#looks right! now we need to remove the rows that have adult in df1_trimmed and then add the aggregated data back to our trimmed dataframe
adultcount <- aggregate(individualCount~sexOrAge + collectDate + scientificName + namedLocation + plotID + nlcdClass + elevation + decimalLatitude + decimalLongitude + totalSampledArea,data=subset(df1_trimmed,sexOrAge == c('Adult')),sum)
df1_trimmed <- df1_trimmed[df1_trimmed$sexOrAge != 'Adult',]
df1_trimmed <- dplyr::bind_rows(df1_trimmed,adultcount)
#Now we just need to remove the adult entries from the trimmed dataset and append the aggregated data
df2zeros_trimmed <- subset(df2zeros, select=-c(domainID,siteID,plotType,geodeticDatum,coordinateUncertainty,
                                               elevationUncertainty,samplingImpractical,biophysicalCriteria,eventID,
                                               sampleID,sampleCode,samplingMethod,targetTaxaPresent,adultCount,nymphCount,
                                               larvaCount,sampleCondition,samplingProtocolVersion,measuredBy,remarks,
                                               publicationDate,release))
# View(df2zeros_trimmed)

#we have removed unneeded columns, now we need to combine the dataframes (https://stackoverflow.com/questions/53004125/concatenate-multiple-data-frames-with-different-columns-in-r)
#df1_trimmed$collectDate <- as.Date(df1_trimmed$collectDate)
mergetest <- dplyr::bind_rows(df1_trimmed,df2zeros_trimmed)
#need to remove ".tck" from the namedLocation so that things line up when combining with the fire data
mergetest$namedLocation <- gsub(".tck","",as.character(mergetest$namedLocation))
View(mergetest)

#save to csv
# write.csv(mergetest,"D:/NEONexample/NEONtickdatacleaned.csv",row.names = FALSE)

#*FIRE DATA ----
#for combining the downloaded data into tables:
stackByTable(filepath="C:/Users/bigfo/OneDrive/Desktop/school/neon data/site management/NEON_site-mgt-and-event-report.zip")
#load the stacked data:
sdf <- read.csv("C:/Users/bigfo/OneDrive/Desktop/school/neon data/site management/NEON_site-mgt-and-event-report/stackedFiles/sim_eventData.csv")

#load the site event reporting data (https://data.neonscience.org/data-products/DP1.10111.001)
# sitedata <- loadByProduct(dpID = 'DP1.10111.001', site = 'KONZ')
# sitedata <- loadByProduct(dpID = 'DP1.10111.001', site = 'OAES')
# sitedata <- loadByProduct(dpID = 'DP1.10111.001', site = 'SOAP')
sitedata <- loadByProduct(dpID = 'DP1.10111.001')

#to see where the useful data is contained, we view the variables:
View(sitedata$variables_10111)

#all of the site data that is relevent to us is contained within sim_eventData
#notice that the locationID and methodTypeChoice what we will be using to identify 
#those tick plots that are burned in a controlled manner. However, let's keep wildfires in for now
# View(sitedata$sim_eventData)

sdf <- sitedata$sim_eventData
#TO CHECK IF DIRECTLY DOWNLOADED DATA IS SAME AS DATA ThROUGh R, USE THIS FUNCTION:
#isTRUE( all.equal(df2,df1) )
View(sdf)

#subset fire data
# sdf_fire <- sdf[sdf$eventType == 'fire',]
#to include ONLY controlled burns
sdf_fire <- sdf[sdf$methodTypeChoice == 'fire-controlledBurn',]
View(sdf_fire)

#separate the locationID column into multiple rows so that we can extract the tickPlots that are burned
sdf_fire_separate <- tidyr::separate_rows(sdf_fire,locationID, sep = ",")
View(sdf_fire_separate)
# 
#subset to include only tickPlots (https://stackoverflow.com/questions/13043928/selecting-data-frame-rows-based-on-partial-string-match-in-a-column)
sdf_fire_separate_tick <- sdf_fire_separate[grep("tickPlot", sdf_fire_separate$locationID),]
View(sdf_fire_separate_tick)

#remove unneeded columns
sdf_trimmed <- subset(sdf_fire_separate_tick, select=-c(domainID,siteID,namedLocation,ongoingEvent,
                                                        estimatedOrActualDate,dateRemarks,eventID,samplingProtocolVersion,
                                                        eventType,name,scientificName,otherScientificName,biomassRemoval,
                                                        minQuantity,maxQuantity,quantityUnit,reporterType,remarks,recordedBy,
                                                        dataQF,publicationDate,release))
View(sdf_trimmed)

#change locationID to namedLocation and startDate to so that they match with the other dataframes
colnames(sdf_trimmed)[2] <- "namedLocation"
colnames(sdf_trimmed)[3] <- "collectDate"

#remove spaces in new namedLocation so we can order later
sdf_trimmed$namedLocation <- gsub(" ", "", sdf_trimmed$namedLocation, fixed = TRUE)

#save to csv
# write.csv(sdf_trimmed,"D:/NEONexample/NEONfiredatacleaned.csv",row.names = FALSE)

mergetest1 <- dplyr::bind_rows(mergetest,sdf_trimmed)
View(mergetest1)
#now that we have fused these data, we can remove the '.tickPlot' to make merging easier later on
mergetest1$namedLocation <- gsub(".tickPlot","",as.character(mergetest1$namedLocation))


#*MAMMAL DATA ----
#for combining the downloaded data into tables:
stackByTable(filepath="C:/Users/bigfo/OneDrive/Desktop/school/neon data/mammal site data/NEON_count-small-mammals.zip")
#load the stacked data:
mammaldata_pertrap <- read.csv("C:/Users/bigfo/OneDrive/Desktop/school/neon data/mammal site data/NEON_count-small-mammals/stackedFiles/mam_pertrapnight.csv")

# mammaldata <- loadByProduct(dpID = 'DP1.10072.001', site = 'KONZ')
mammaldata <- loadByProduct(dpID = 'DP1.10072.001')

View(mammaldata$variables_10072)

mammaldata_pertrap <- mammaldata$mam_pertrapnight
#TO CHECK IF DIRECTLY DOWNLOADED DATA IS SAME AS DATA ThROUGh R, USE THIS FUNCTION:
#isTRUE( all.equal(df2,df1) )
View(mammaldata_pertrap)
#we need to remove all of the events that had no successful trapping
mammaldata_pertrap1 <- mammaldata_pertrap[!grepl("0",mammaldata_pertrap$trapStatus),]
mammaldata_pertrap1 <- mammaldata_pertrap1[!grepl("1",mammaldata_pertrap1$trapStatus),]
mammaldata_pertrap1 <- mammaldata_pertrap1[!grepl("2",mammaldata_pertrap1$trapStatus),]
mammaldata_pertrap1 <- mammaldata_pertrap1[!grepl("3",mammaldata_pertrap1$trapStatus),]
mammaldata_pertrap1 <- mammaldata_pertrap1[!grepl("6",mammaldata_pertrap1$trapStatus),]
View(mammaldata_pertrap1)
#Now we want to aggregate based on the siteID since it will remain the same if we are looking in the same site while keeping track of the scientific name, plotID, and date
View(aggregate(siteID~ collectDate + plotID + scientificName + nlcdClass, data = mammaldata_pertrap1, length))
#if you want to aggregate including the environment type that the mammal is caught in, use the following line instead of the uncommented one
#finalmammaldata <- aggregate(siteID~ collectDate + plotID + scientificName + nlcdClass, data = mammaldata_pertrap1, length)
finalmammaldata1 <- aggregate(siteID~ collectDate + plotID + scientificName, data = mammaldata_pertrap1, length)
colnames(finalmammaldata1)[4] <- "individualCount"
colnames(finalmammaldata1)[2] <- "namedLocation"
finalmammaldata2 <- finalmammaldata1[finalmammaldata1$namedLocation == mergetest$namedLocation,]
#now this data is ready to be combined with the other datasets
mergetest2 <- dplyr::bind_rows(mergetest1,finalmammaldata2)

#*TEMP DATA ----
#for combining the downloaded data into tables:
stackByTable(filepath="C:/Users/bigfo/OneDrive/Desktop/school/neon data/trip asp air temp data/NEON_site-mgt-and-event-report.zip")
#load the stacked data:
tempdataset <- read.csv("C:/Users/bigfo/OneDrive/Desktop/school/neon data/trip asp air temp data/NEON_site-mgt-and-event-report/stackedFiles/wss_daily_temp.csv")
# tempdata <- loadByProduct(dpID = 'DP4.00001.001', site = 'KONZ')
tempdata <- loadByProduct(dpID = 'DP1.00003.001')
View(tempdata$variables_00001)

tempdataset <- tempdata$wss_daily_temp
#TO CHECK IF DIRECTLY DOWNLOADED DATA IS SAME AS DATA ThROUGh R, USE THIS FUNCTION:
#isTRUE( all.equal(df2,df1) )
tempdataset$date <- format(tempdataset$date, '%Y-%m')
#View(aggregate(wssTempTripleMean~ date + siteID + domainID,data=tempdataset,mean))

tempdataset1 <- aggregate(wssTempTripleMean~ date + siteID,data=tempdataset,mean)
#change col names for merging
colnames(tempdataset1)[2] <- "namedLocation"
colnames(tempdataset1)[1] <- "collectDate"
#THIS MIGHT WORK, BUT I HAVEN'T TESTED:
library(stringr)
library(dplyr)

tempdataset1 %>%
  filter(str_detect(row.names(tempdataset1$namedLocation != mergetest$namedlocation), paste(remove_if, collapse = "|"), negate = TRUE))
#
mergetest2$collectDate <- as.Date(mergetest2$collectDate, format = '%Y-%m-%d')
tempdataset1$collectDate <- as.Date(paste0(tempdataset1$collectDate, "-01"), format = '%Y-%m-%d')
mergetest3 <- dplyr::bind_rows(mergetest2,tempdataset1)
View(mergetest3)

#order by site, then date
finalmerge <- mergetest3[order(mergetest3$namedLocation,mergetest3$collectDate),]
View(finalmerge)
# ####TEMP DATASET THAT I FOUND###
# tempdata <- loadByProduct(dpID = 'DP1.00098.001', site = 'KONZ')
# View(tempdata$RH_30min)
write.csv(finalmerge,"D:/NEONexample/full_only species.csv",row.names = FALSE)

# *PRECIPITATION DATA ----
#for combining the downloaded data into tables:
stackByTable(filepath="/Users/a426f262/Downloads/Directly Downloaded Data/NEON_precipitation.zip")
#load the stacked data:
tempdataset <- read.csv("C:/Users/bigfo/OneDrive/Desktop/school/neon data/trip asp air temp data/NEON_site-mgt-and-event-report/stackedFiles/wss_daily_temp.csv")










#order by site, then date
finalmerge <- mergetest3[order(mergetest3$namedLocation,mergetest3$collectDate),]
View(finalmerge)
# ####TEMP DATASET THAT I FOUND###
# tempdata <- loadByProduct(dpID = 'DP1.00098.001', site = 'KONZ')
# View(tempdata$RH_30min)

write.csv(finalmerge,"D:/NEONexample/full_allcounts.csv",row.names = FALSE)
