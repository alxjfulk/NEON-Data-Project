# install.packages('neonUtilities')
library(neonUtilities)
library(dplyr)
library(tidyr)
library(lubridate)

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
View(aggregate(siteID~ collectDate + plotID + scientificName, data = mammaldata_pertrap1, length))
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
#View(tickdata$variables_10093)

#seems like we may need nlcdClass data from the tck_fielddata, but nothing else 
#since the other data is contained within tck_taxonomyProcessed
#View(tickdata$tck_taxonomyProcessed)
#View(tickdata$tck_fielddata)

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
#The above is correct, however, we also need to convert those sampling instances that collected only other species to absence data for a americanum, thus we must edit df1
#start by splitting into a americanum collections and not (i.e. all the other species)
df1americanum <- df1[df1$scientificName == 'Amblyomma americanum',]
df1not <- df1[df1$scientificName != 'Amblyomma americanum',]
#Now we need to compare the dates and plots of the two dataframes and remove those that match from the "not" dataframe. 
#This allows us to include only those sampling events where they sampled and collected only ticks OTHER than a americanum. (https://stackoverflow.com/questions/72546014/remove-rows-that-do-not-match-common-dates-from-a-separate-data-frame)
#This is a bit tricky. Technically, it is possible that by using this filter, we remove sampling instances that occurred on
#the same day and at the same time, but at different plots, but I think this is unlikely. The only way that we could ensure
#that we don't remove relevant records is to create a second dataframe checking for those instances
df1not1 <- df1not %>% filter((!df1not$collectDate %in% df1americanum$collectDate))
df1check1 <- df1not %>% filter((df1not$collectDate %in% df1americanum$collectDate) & (!df1not$plotID %in% df1americanum$plotID))
df1not <- dplyr::bind_rows(df1not1, df1check1)

#now we want to convert all of the indiivudal counts in the "not" dataframe to zeros to indicate absence of a americanum
df1not$individualCount <- 0

#Finally, we will recombine both dataframes
df1 <- dplyr::bind_rows(df1americanum, df1not)
# View(df1americanum)
# View(df1not)
#now we need to remove unneeded columns before merging the two dataframes (https://stackoverflow.com/questions/5234117/how-to-drop-columns-by-name-in-a-data-frame)
df1_trimmed <- subset(df1, select=-c(identifiedDate,sampleID,subsampleID,acceptedTaxonID,taxonRank,family,subfamily,tribe,
                                     subtribe,genus,subgenus,specificEpithet,infraspecificEpithet,identificationQualifier,identificationReferences,
                                     identificationProtocolVersion,scientificNameAuthorship,sampleCondition,archiveMedium,
                                     archiveFacilityID,deprecatedVialID,identifiedBy,laboratoryName,remarks,publicationDate,release))
# View(df1_trimmed)
#let's see what an aggregation will give us
#View(aggregate(individualCount~sexOrAge + collectDate + scientificName + namedLocation + plotID + nlcdClass + elevation + decimalLatitude + decimalLongitude + totalSampledArea,data=subset(df1_trimmed,sexOrAge == c('Adult')),sum))
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
#unique(df1$scientificName)
#we have removed unneeded columns, now we need to combine the dataframes (https://stackoverflow.com/questions/53004125/concatenate-multiple-data-frames-with-different-columns-in-r)
#df1_trimmed$collectDate <- as.Date(df1_trimmed$collectDate)
mergetest <- dplyr::bind_rows(df1_trimmed,df2zeros_trimmed)
#remove spaces in new namedLocation so we can order later
mergetest$namedLocation <- gsub(" ", "", mergetest$namedLocation, fixed = TRUE)
#need to remove ".tck" from the namedLocation so that things line up when combining with the fire data
mergetest$namedLocation <- gsub(".tck","",as.character(mergetest$namedLocation))
#View(mergetest)

#somehow some rows of only NAs has been included. Let's remove that:
mergetest <- mergetest[rowSums(is.na(mergetest)) != ncol(mergetest), ]
#save to csv
#write.csv(mergetest,"C:/Users/bigfo/OneDrive/Desktop/Research/NEON Data/Tick NEON Data Cleaned/NEON_AAmericanum_tickdatacleaned.csv",row.names = FALSE)

#*FIRE DATA ----
#load the site event reporting data (https://data.neonscience.org/data-products/DP1.10111.001)
sitedata <- loadByProduct(dpID = 'DP1.10111.001')

#to see where the useful data is contained, we view the variables:
View(sitedata$variables_10111)

#all of the site data that is relevent to us is contained within sim_eventData
#notice that the locationID and methodTypeChoice what we will be using to identify 
#those tick plots that are burned in a controlled manner
# View(sitedata$sim_eventData)

sdf <- sitedata$sim_eventData
#View(sdf)

#to include ONLY controlled burns
sdf_fire <- sdf[sdf$methodTypeChoice == 'fire-controlledBurn',]
#View(sdf_fire)

#separate the locationID column into multiple rows so that we can extract the tickPlots that are burned
sdf_fire_separate <- tidyr::separate_rows(sdf_fire,locationID, sep = ",")
#View(sdf_fire_separate)
# 
#subset to include only tickPlots (https://stackoverflow.com/questions/13043928/selecting-data-frame-rows-based-on-partial-string-match-in-a-column)
sdf_fire_separate_tick <- sdf_fire_separate[grep("tickPlot", sdf_fire_separate$locationID),]
#View(sdf_fire_separate_tick)

#remove unneeded columns
sdf_trimmed <- subset(sdf_fire_separate_tick, select=-c(domainID,siteID,namedLocation,ongoingEvent,
                                                        estimatedOrActualDate,startDate,dateRemarks,eventID,samplingProtocolVersion,
                                                        eventType,name,scientificName,otherScientificName,biomassRemoval,
                                                        minQuantity,maxQuantity,quantityUnit,reporterType,remarks,recordedBy,
                                                        dataQF,publicationDate,release))

#View(sdf_trimmed)

#change locationID to namedLocation so that it matches with the other dataframes
colnames(sdf_trimmed)[1] <- "uidFire"
colnames(sdf_trimmed)[2] <- "locationIDFire"
colnames(sdf_trimmed)[3] <- "endDateFire"
colnames(sdf_trimmed)[4] <- "methodTypeChoiceFire"

#convert to dataframe and remove spaces in new namedLocation so we can order later
sdf_trimmed <- data.frame(sdf_trimmed)
sdf_trimmed$locationIDFire <- gsub(" ", "", sdf_trimmed$locationIDFire, fixed = TRUE)
write.csv(sdf_trimmed,"C:/Users/bigfo/OneDrive/Desktop/Research/NEON Data/Fire Data Cleaned/fire_cleaned.csv",row.names = FALSE)

#*MAMMAL DATA ----
mammaldata <- loadByProduct(dpID = 'DP1.10072.001')

#View(mammaldata$variables_10072)

mammaldata_pertrap <- mammaldata$mam_pertrapnight

#we need to remove all of the events that had no successful trapping
exclude_num <- c("0","1","2","3","6")
mammaldata_pertrap1 <- mammaldata_pertrap[!grepl(paste(exclude_num, collapse = "|"),mammaldata_pertrap$trapStatus),]

#View(mammaldata_pertrap1)

#NEED TO REMOVE ANY CAPTURES WHERE SPECIES IS NOT DETERMINED?

#Now we want to aggregate based on the siteID since it will remain the same if we are looking in the same site while keeping track of the scientific name, plotID, and date
#View(aggregate(siteID~ collectDate + plotID + scientificName + nlcdClass, data = mammaldata_pertrap1, length))
finalmammaldata1 <- aggregate(siteID~ collectDate + plotID + scientificName + nlcdClass, data = mammaldata_pertrap1, length)
colnames(finalmammaldata1)[5] <- "count"
colnames(finalmammaldata1)[2] <- "namedLocation"

#*TEMP, PRECIP, VPD, DEW TEMP DATA ----
#for combining the downloaded data:
file_names <- dir('C:/Users/bigfo/OneDrive/Desktop/Research/PRISM Data/final_adjusted/final',full.names = TRUE) #where you have your files

temp_frame <- do.call(rbind,lapply(file_names,read.csv)) #read them all and combine into a single dataframe

temp_frame$Date <- mdy(temp_frame$Date) #convert dates to something R can understand

##THIS IS UNNEEDED, BUT WORKS:
tempdataset <- read.csv("C:/Users/bigfo/OneDrive/Desktop/Research/PRISM Data/final_adjusted/final/PRISM_ppt_tmin_tmean_tmax_tdmean_vpdmin_vpdmax_provisional_4km_20220101_20221231.csv")
tempdataset1 <- read.csv("C:/Users/bigfo/OneDrive/Desktop/Research/PRISM Data/final_adjusted/final/PRISM_ppt_tmin_tmean_tmax_tdmean_vpdmin_vpdmax_provisional_4km_20230101_20230331.csv")
tempdataset2 <- read.csv("C:/Users/bigfo/OneDrive/Desktop/Research/PRISM Data/final_adjusted/final/PRISM_ppt_tmin_tmean_tmax_tdmean_vpdmin_vpdmax_stable_4km_20130101_20131231.csv")
tempdataset3 <- read.csv("C:/Users/bigfo/OneDrive/Desktop/Research/PRISM Data/final_adjusted/final/PRISM_ppt_tmin_tmean_tmax_tdmean_vpdmin_vpdmax_stable_4km_20140101_20141231.csv")
tempdataset4 <- read.csv("C:/Users/bigfo/OneDrive/Desktop/Research/PRISM Data/final_adjusted/final/PRISM_ppt_tmin_tmean_tmax_tdmean_vpdmin_vpdmax_stable_4km_20150101_20151231.csv")
tempdataset5 <- read.csv("C:/Users/bigfo/OneDrive/Desktop/Research/PRISM Data/final_adjusted/final/PRISM_ppt_tmin_tmean_tmax_tdmean_vpdmin_vpdmax_stable_4km_20160101_20161231.csv")
tempdataset6 <- read.csv("C:/Users/bigfo/OneDrive/Desktop/Research/PRISM Data/final_adjusted/final/PRISM_ppt_tmin_tmean_tmax_tdmean_vpdmin_vpdmax_stable_4km_20170101_20171231.csv")
tempdataset7 <- read.csv("C:/Users/bigfo/OneDrive/Desktop/Research/PRISM Data/final_adjusted/final/PRISM_ppt_tmin_tmean_tmax_tdmean_vpdmin_vpdmax_stable_4km_20180101_20181231.csv")
tempdataset8 <- read.csv("C:/Users/bigfo/OneDrive/Desktop/Research/PRISM Data/final_adjusted/final/PRISM_ppt_tmin_tmean_tmax_tdmean_vpdmin_vpdmax_stable_4km_20190101_20191231.csv")
tempdataset9 <- read.csv("C:/Users/bigfo/OneDrive/Desktop/Research/PRISM Data/final_adjusted/final/PRISM_ppt_tmin_tmean_tmax_tdmean_vpdmin_vpdmax_stable_4km_20200101_20201231.csv")
tempdataset10 <- read.csv("C:/Users/bigfo/OneDrive/Desktop/Research/PRISM Data/final_adjusted/final/PRISM_ppt_tmin_tmean_tmax_tdmean_vpdmin_vpdmax_stable_4km_20210101_20211231.csv")

tempdataset$Date <- mdy(tempdataset$Date)
tempdataset1$Date <- mdy(tempdataset1$Date)
tempdataset2$Date <- mdy(tempdataset2$Date)
tempdataset3$Date <- mdy(tempdataset3$Date)
tempdataset4$Date <- mdy(tempdataset4$Date)
tempdataset5$Date <- mdy(tempdataset5$Date)
tempdataset6$Date <- mdy(tempdataset6$Date)
tempdataset7$Date <- mdy(tempdataset7$Date)
tempdataset8$Date <- mdy(tempdataset8$Date)
tempdataset9$Date <- mdy(tempdataset9$Date)
tempdataset10$Date <- mdy(tempdataset10$Date)

fusetemp <- dplyr::bind_rows(tempdataset,tempdataset1,tempdataset2,tempdataset3,tempdataset4,tempdataset5,tempdataset6,tempdataset7,tempdataset8,tempdataset9,tempdataset10)
##
#save if needed, we will combine with the other dataset later
write.csv(temp_frame,"C:/Users/bigfo/OneDrive/Desktop/Research/NEON Data/Temperature Data Cleaned/prismdata.csv",row.names = FALSE)
#*COMBINE DATA ----
#load data and combine with temp data
combineddata <- read.csv("C:/Users/bigfo/OneDrive/Desktop/Research/NEON Data/Tick NEON Data Cleaned/AAmericanum_cleaned_withFire.csv")
originalDate <- combineddata$collectDate
combineddata$collectDate <- mdy_hm(combineddata$collectDate)
combineddata$collectDate <- as.Date(combineddata$collectDate)

#Just remove the non-matching part and create a new column to be removed later
#combineddata$namedLocation1 = substr(combineddata$namedLocation,1,nchar(combineddata$namedLocation)-4)

combineddata1 <- merge(combineddata,fusetemp, by.x = c("plotID","collectDate"), by.y = c("Name","Date"), all.x = TRUE)

#fill blanks with NA, need to write it to csv and load to convert columns to character before running
#load combined tick and fire data:
#combineddata2 <- read.csv("C:/Users/bigfo/OneDrive/Desktop/combined.csv")

combineddata2 <- combineddata1 %>%
  mutate(across(everything(), ~ifelse(.=="", NA, as.character(.))))
combineddata2 <- combineddata1
#convert to dates
combineddata2$collectDate <- ymd(combineddata2$collectDate)
combineddata2$endDateFire <- mdy(combineddata2$endDateFire)
#Add the days since last burn column:
combineddata2$daysSinceLastBurn <- difftime(combineddata2$endDateFire,combineddata2$collectDate, units = "days")
#this works, but gives negative # of days so let's make it positive
combineddata2$daysSinceLastBurn <- combineddata2$daysSinceLastBurn * -1
which(combineddata2$daysSinceLastBurn<0,arr.ind = TRUE)
#save to csv
write.csv(combineddata2,"C:/Users/bigfo/OneDrive/Desktop/combined.csv",row.names = FALSE)

#load combiend data
loadedcombineddata <- read.csv("C:/Users/bigfo/OneDrive/Desktop/Research/NEON Data/combined.csv")
combineddata2 <- loadedcombineddata

#repalce NAs with 0
combineddata2$individualCount[is.na(combineddata2$individualCount)] <- 0

library(ggplot2)
#PLOTS FOR PRECIP
#convert to numeric
combineddata2$ppt <- as.numeric(combineddata2$ppt)
#basic plot
plot(combineddata2$ppt,combineddata2$individualCount)
combineddata2 %>% count(ppt)
# Calculate the average individualCount per ppt
avgIndividualCountPPT = combineddata2 %>%
  group_by(ppt) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountPPT, aes(x = ppt, y = avgCount)) +
  geom_point() +
  xlab("Precipitation") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per Precipitation")
#add column for days since last precipitation?

#PLOTS FOR TSA
plot(combineddata2$totalSampledArea,combineddata2$individualCount)
combineddata2 %>% count(totalSampledArea)
#convert totalsampledarea to numeric
combineddata2$totalSampledArea <- as.numeric(combineddata2$totalSampledArea)
# Calculate the average individualCount per totalSampledArea
avgIndividualCountTSA = combineddata2 %>%
  group_by(totalSampledArea) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountTSA, aes(x = totalSampledArea, y = avgCount)) +
  geom_point() +
  xlab("Total Sampled Area") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per Total Sampled Area")
#create bar graph
ggplot(avgIndividualCountTSA, aes(x = totalSampledArea, y = avgCount)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  xlab("Total Sampled Area") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per Total Sampled Area")

#PLOTS FOR TEMP
#convert to numeric
combineddata2$tmean <- as.numeric(combineddata2$tmean)
#basic plot and count per temp measurement
plot(combineddata2$tmean,combineddata2$individualCount)
combineddata2 %>% count(tmean)
# Calculate the average individualCount per avg temp
avgIndividualCountTmean = combineddata2 %>%
  group_by(tmean) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountTmean, aes(x = tmean, y = avgCount)) +
  geom_point() +
  xlab("Average Temperature") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per Average Temp")

#PLOTS FOR DAYS SINCE LAST BURN
plot(combineddata2$daysSinceLastBurn,combineddata2$individualCount)
combineddata2 %>% count(daysSinceLastBurn)
# Calculate the average individualCount per #ofdaysSinceLastBurn
avgIndividualCountDSLB = combineddata2 %>%
  group_by(daysSinceLastBurn) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountDSLB, aes(x = daysSinceLastBurn, y = avgCount)) +
  geom_point() +
  xlab("Days Since Last Burn") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per # of Days Since Last Burn")


#plot those columns that contain characters

#PLOTS FOR ENVIRO CLASS
#basic plot + count
ggplot(data=combineddata2, mapping = aes(x = nlcdClass, y = individualCount)) + geom_count(stat = "identity") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
combineddata2 %>% count(nlcdClass)
# Calculate the average individualCount per Envrio class
avgIndividualCountNLCD = combineddata2 %>%
  group_by(nlcdClass) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountNLCD, aes(x = nlcdClass, y = avgCount)) +
  geom_point() +
  xlab("Environment") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per Enivromental Class") +
 theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#PLOTS FOR FIRE SEVERITY
#basic plot
ggplot(data=combineddata2, mapping = aes(x = fireSeverity, y = individualCount)) + geom_count(stat = "identity") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
combineddata2 %>% count(fireSeverity)
# Calculate the average individualCount per severity
avgIndividualCountSev = combineddata2 %>%
  group_by(fireSeverity) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountSev, aes(x = fireSeverity, y = avgCount)) +
  geom_point() +
  xlab("Fire Severity") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per Fire Severity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))





#*Explorations####
#*SOIL DATA
soildata <- loadByProduct(dpID = 'DP1.00096.001')
View(soildata)
#*EDDY COVARIANCE DATA
eddydata <- loadByProduct(dpID = 'DP4.00200.001')
#wait on this for now, likely won't need
View(eddydata)
#*BIRD DATA
birddata <- loadByProduct(dpID = 'DP1.10003.001')
View(birddata)
#*COURSE DEBRIS DATA
coursedata <- loadByProduct(dpID = 'DP1.10014.001')
View(coursedata)
#*PLOT VEGETATION DATA
plotvegdata <- loadByProduct(dpID = 'DP1.10017.001')
View(plotvegdata)
#*LITTERFALL AND FINE WOODY DEBRIS DATA
finedata <- loadByProduct(dpID = 'DP1.10033.001')
View(finedata)