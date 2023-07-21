#PACKAGES
library(neonUtilities)
library(dplyr)
library(lubridate)
library(ggplot2)
####CLEANING DATA####
#UNUSED FOR NOW
# #*TICK DATA -----
# #for combining the downloaded data into tables:
# stackByTable(filepath="C:/Users/bigfo/OneDrive/Desktop/school/neon data/tick site data/NEON_count-ticks.zip")
# #load the stacked data:
# df1 <- read.csv("C:/Users/bigfo/OneDrive/Desktop/school/neon data/tick site data/NEON_count-ticks/stackedFiles/tck_taxonomyProcessed.csv")
# df2 <- read.csv("C:/Users/bigfo/OneDrive/Desktop/school/neon data/tick site data/NEON_count-ticks/stackedFiles/tck_fielddata.csv")
# 
# #load the site tick data (https://data.neonscience.org/data-products/DP1.10093.001)
# # tickdata <- loadByProduct(dpID = 'DP1.10093.001', site = 'KONZ')
# # tickdata <- loadByProduct(dpID = 'DP1.10093.001', site = 'OAES')
# # tickdata <- loadByProduct(dpID = 'DP1.10093.001', site = 'SOAP')
# tickdata <- loadByProduct(dpID = 'DP1.10093.001')
# #see what is contained in the data
# View(tickdata$variables_10093)
# 
# #seems like we may need nlcdClass data from the tck_fielddata, but nothing else 
# #since the other data is contained within tck_taxonomyProcessed
# View(tickdata$tck_taxonomyProcessed)
# View(tickdata$tck_fielddata)
# 
# #let's try a different approach taken from this stackoverflow link: https://stackoverflow.com/questions/64992027/adding-column-to-a-dataframe-in-r-based-on-matching-conditions-in-another-datafr
# df1 <- tickdata$tck_taxonomyProcessed
# df2 <- tickdata$tck_fielddata
# #TO CHECK IF DIRECTLY DOWNLOADED DATA IS SAME AS DATA ThROUGh R, USE THIS FUNCTION:
# #isTRUE( all.equal(df2,df1) )
# #here we specify those rows that should match in each dataframe and the column that we want to copy from.
# #this step preserves data from the field data that may be needed for analysis in teh taxonomic data
# #for example, the type of enviornment that the ticks are sampled from
# df1$nlcdClass <- df2[match(paste(df1$plotID),paste(df2$plotID)),"nlcdClass"]
# df1$elevation <- df2[match(paste(df1$plotID),paste(df2$plotID)),"elevation"]
# df1$decimalLatitude <- df2[match(paste(df1$plotID),paste(df2$plotID)),"decimalLatitude"]
# df1$decimalLongitude <- df2[match(paste(df1$plotID),paste(df2$plotID)),"decimalLongitude"]
# df1$totalSampledArea <- df2[match(paste(df1$plotID),paste(df2$plotID)),"totalSampledArea"]
# 
# #summing by males and females by year. First replace male and female as adult
# df1$sexOrAge <-replace(df1$sexOrAge,df1$sexOrAge=="Male","Adult")
# df1$sexOrAge <-replace(df1$sexOrAge,df1$sexOrAge=="Female","Adult")
# #reformat collectDate column to make the aggregtion easier 
# df1$collectDate <- format(as.POSIXct(df1$collectDate,format='%Y-%m-%d %H:%M:%S'),format='%Y-%m-%d %H:%M:%S')
# 
# 
# # View(df1)
# 
# #now we need to remove the entries from the field data that had ticks collected so that we avoid duplicate data when merging
# df2zeros <- df2[(df2$adultCount == 0 | is.na(df2$adultCount)) & (df2$nymphCount == 0 | is.na(df2$nymphCount)) & (df2$larvaCount == 0 | is.na(df2$larvaCount)) & !is.na(df2$totalSampledArea),]
# # View(df2zeros)
# #now we need to remove unneeded columns before merging the two dataframes (https://stackoverflow.com/questions/5234117/how-to-drop-columns-by-name-in-a-data-frame)
# df1_trimmed <- subset(df1, select=-c(identifiedDate,sampleID,subsampleID,acceptedTaxonID,taxonRank,family,subfamily,tribe,
#                       subtribe,genus,subgenus,specificEpithet,infraspecificEpithet,identificationQualifier,identificationReferences,
#                       identificationProtocolVersion,scientificNameAuthorship,sampleCondition,archiveMedium,
#                       archiveFacilityID,deprecatedVialID,identifiedBy,laboratoryName,remarks,publicationDate,release))
# # View(df1_trimmed)
# #let's see what an aggregation will give us
# View(aggregate(individualCount~sexOrAge + collectDate + scientificName + namedLocation + plotID + nlcdClass + elevation + decimalLatitude + decimalLongitude + totalSampledArea,data=subset(df1_trimmed,sexOrAge == c('Adult')),sum))
# #looks right! now we need to remove the rows that have adult in df1_trimmed and then add the aggregated data back to our trimmed dataframe
# adultcount <- aggregate(individualCount~sexOrAge + collectDate + scientificName + namedLocation + plotID + nlcdClass + elevation + decimalLatitude + decimalLongitude + totalSampledArea,data=subset(df1_trimmed,sexOrAge == c('Adult')),sum)
# df1_trimmed <- df1_trimmed[df1_trimmed$sexOrAge != 'Adult',]
# df1_trimmed <- dplyr::bind_rows(df1_trimmed,adultcount)
# #it's the same! Now we just need to remove the adult entries from the trimmed dataset and append the aggregated data
# df2zeros_trimmed <- subset(df2zeros, select=-c(domainID,siteID,plotType,geodeticDatum,coordinateUncertainty,
#                                                elevationUncertainty,samplingImpractical,biophysicalCriteria,eventID,
#                                                sampleID,sampleCode,samplingMethod,targetTaxaPresent,adultCount,nymphCount,
#                                                larvaCount,sampleCondition,samplingProtocolVersion,measuredBy,remarks,
#                                                publicationDate,release))
# # View(df2zeros_trimmed)
# 
# #we have removed unneeded columns, now we need to combine the dataframes (https://stackoverflow.com/questions/53004125/concatenate-multiple-data-frames-with-different-columns-in-r)
# df1_trimmed$collectDate <- as.Date(df1_trimmed$collectDate)
# mergetest <- dplyr::bind_rows(df1_trimmed,df2zeros_trimmed)
# #need to remove ".tck" from the namedLocation so that things line up when combining with the fire data
# mergetest$namedLocation <- gsub(".tck","",as.character(mergetest$namedLocation))
# View(mergetest)
# 
# #save to csv
# # write.csv(mergetest,"D:/NEONexample/NEONtickdatacleaned.csv",row.names = FALSE)
# 
# #*FIRE DATA ----
# #for combining the downloaded data into tables:
# stackByTable(filepath="C:/Users/bigfo/OneDrive/Desktop/school/neon data/site management/NEON_site-mgt-and-event-report.zip")
# #load the stacked data:
# sdf <- read.csv("C:/Users/bigfo/OneDrive/Desktop/school/neon data/site management/NEON_site-mgt-and-event-report/stackedFiles/sim_eventData.csv")
# 
# #load the site event reporting data (https://data.neonscience.org/data-products/DP1.10111.001)
# # sitedata <- loadByProduct(dpID = 'DP1.10111.001', site = 'KONZ')
# # sitedata <- loadByProduct(dpID = 'DP1.10111.001', site = 'OAES')
# # sitedata <- loadByProduct(dpID = 'DP1.10111.001', site = 'SOAP')
# sitedata <- loadByProduct(dpID = 'DP1.10111.001')
# 
# #to see where the useful data is contained, we view the variables:
# View(sitedata$variables_10111)
# 
# #all of the site data that is relevent to us is contained within sim_eventData
# #notice that the locationID and methodTypeChoice what we will be using to identify 
# #those tick plots that are burned in a controlled manner. However, let's keep wildfires in for now
# # View(sitedata$sim_eventData)
# 
# sdf <- sitedata$sim_eventData
# #TO CHECK IF DIRECTLY DOWNLOADED DATA IS SAME AS DATA ThROUGh R, USE THIS FUNCTION:
# #isTRUE( all.equal(df2,df1) )
# 
# #subset fire data
# # sdf_fire <- sdf[sdf$eventType == 'fire',]
# #to include ONLY controlled burns
# sdf_fire <- sdf[sdf$methodTypeChoice == 'fire-controlledBurn',]
# View(sdf_fire)
# 
# #separate the locationID column into multiple rows so that we can extract the tickPlots that are burned
# sdf_fire_separate <- tidyr::separate_rows(sdf_fire,locationID, sep = ",")
# View(sdf_fire_separate)
# # 
# #subset to include only tickPlots (https://stackoverflow.com/questions/13043928/selecting-data-frame-rows-based-on-partial-string-match-in-a-column)
# sdf_fire_separate_tick <- sdf_fire_separate[grep("tickPlot", sdf_fire_separate$locationID),]
# View(sdf_fire_separate_tick)
# 
# #remove unneeded columns
# sdf_trimmed <- subset(sdf_fire_separate_tick, select=-c(domainID,siteID,namedLocation,ongoingEvent,
#                                                         estimatedOrActualDate,dateRemarks,eventID,samplingProtocolVersion,
#                                                         eventType,name,scientificName,otherScientificName,biomassRemoval,
#                                                         minQuantity,maxQuantity,quantityUnit,reporterType,remarks,recordedBy,
#                                                         dataQF,publicationDate,release))
# View(sdf_trimmed)
# 
# #change locationID to namedLocation and startDate to so that they match with the other dataframes
# colnames(sdf_trimmed)[2] <- "namedLocation"
# colnames(sdf_trimmed)[3] <- "collectDate"
# 
# #remove spaces in new namedLocation so we can order later
# sdf_trimmed$namedLocation <- gsub(" ", "", sdf_trimmed$namedLocation, fixed = TRUE)
# 
# #save to csv
# # write.csv(sdf_trimmed,"D:/NEONexample/NEONfiredatacleaned.csv",row.names = FALSE)
# 
# mergetest1 <- dplyr::bind_rows(mergetest,sdf_trimmed)
# View(mergetest1)
# #now that we have fused these data, we can remove the '.tickPlot' to make merging easier later on
# mergetest1$namedLocation <- gsub(".tickPlot","",as.character(mergetest1$namedLocation))
# 
# 
# #*MAMMAL DATA ----
# #for combining the downloaded data into tables:
# stackByTable(filepath="C:/Users/bigfo/OneDrive/Desktop/school/neon data/mammal site data/NEON_count-small-mammals.zip")
# #load the stacked data:
# mammaldata_pertrap <- read.csv("C:/Users/bigfo/OneDrive/Desktop/school/neon data/mammal site data/NEON_count-small-mammals/stackedFiles/mam_pertrapnight.csv")
# 
# mammaldata <- loadByProduct(dpID = 'DP1.10072.001', site = 'KONZ')
# mammaldata <- loadByProduct(dpID = 'DP1.10072.001')
# View(mammaldata$variables_10072)
# 
# mammaldata_pertrap <- mammaldata$mam_pertrapnight
# #TO CHECK IF DIRECTLY DOWNLOADED DATA IS SAME AS DATA ThROUGh R, USE THIS FUNCTION:
# #isTRUE( all.equal(df2,df1) )
# View(mammaldata_pertrap)
# #we need to remove all of the events that had no successful trapping
# mammaldata_pertrap1 <- mammaldata_pertrap[!grepl("0",mammaldata_pertrap$trapStatus),]
# mammaldata_pertrap1 <- mammaldata_pertrap1[!grepl("1",mammaldata_pertrap1$trapStatus),]
# mammaldata_pertrap1 <- mammaldata_pertrap1[!grepl("2",mammaldata_pertrap1$trapStatus),]
# mammaldata_pertrap1 <- mammaldata_pertrap1[!grepl("3",mammaldata_pertrap1$trapStatus),]
# mammaldata_pertrap1 <- mammaldata_pertrap1[!grepl("6",mammaldata_pertrap1$trapStatus),]
# View(mammaldata_pertrap1)
# #Now we want to aggregate based on the siteID since it will remain the same if we are looking in the same site while keeping track of the scientific name, plotID, and date
# View(aggregate(siteID~ collectDate + plotID + scientificName, data = mammaldata_pertrap1, length))
# #if you want to aggregate including the environment type that the mammal is caught in, use the following line instead of the uncommented one
# #finalmammaldata <- aggregate(siteID~ collectDate + plotID + scientificName + nlcdClass, data = mammaldata_pertrap1, length)
# finalmammaldata1 <- aggregate(siteID~ collectDate + plotID + scientificName, data = mammaldata_pertrap1, length)
# colnames(finalmammaldata1)[4] <- "individualCount"
# colnames(finalmammaldata1)[2] <- "namedLocation"
# finalmammaldata2 <- finalmammaldata1[finalmammaldata1$namedLocation == mergetest$namedLocation,]
# View(finalmammaldata)
# #now this data is ready to be combined with the other datasets
# mergetest2 <- dplyr::bind_rows(mergetest2,finalmammaldata)
# 
# #*TEMP DATA ----
# #for combining the downloaded data into tables:
# stackByTable(filepath="C:/Users/bigfo/OneDrive/Desktop/school/neon data/trip asp air temp data/NEON_site-mgt-and-event-report.zip")
# #load the stacked data:
# tempdataset <- read.csv("C:/Users/bigfo/OneDrive/Desktop/school/neon data/trip asp air temp data/NEON_site-mgt-and-event-report/stackedFiles/wss_daily_temp.csv")
# 
# # tempdata <- loadByProduct(dpID = 'DP4.00001.001', site = 'KONZ')
# tempdata <- loadByProduct(dpID = 'DP4.00001.001')
# View(tempdata$variables_00001)
# 
# tempdataset <- tempdata$wss_daily_temp
# #TO CHECK IF DIRECTLY DOWNLOADED DATA IS SAME AS DATA ThROUGh R, USE THIS FUNCTION:
# #isTRUE( all.equal(df2,df1) )
# tempdataset$date <- format(tempdataset$date, '%Y-%m')
# #View(aggregate(wssTempTripleMean~ date + siteID + domainID,data=tempdataset,mean))
# 
# tempdataset1 <- aggregate(wssTempTripleMean~ date + siteID,data=tempdataset,mean)
# #change col names for merging
# View(tempdataset1)
# colnames(tempdataset1)[2] <- "namedLocation"
# colnames(tempdataset1)[1] <- "collectDate"
# #tempdataset2 <- tempdataset1[tempdataset1$namedLocation == mergetest$namedLocation,]
# #THIS MIGHT WORK, BUT I HAVEN'T TESTED:
# library(stringr)
# library(dplyr)
# 
# tempdataset1 %>%
#   filter(str_detect(row.names(tempdataset1$namedLocation != mergetest$namedlocation), paste(remove_if, collapse = "|"), negate = TRUE))
# #
# mergetest2$collectDate <- as.Date(mergetest2$collectDate, format = '%Y-%m-%d')
# tempdataset1$collectDate <- as.Date(paste0(tempdataset1$collectDate, "-01"), format = '%Y-%m-%d')
# mergetest3 <- dplyr::bind_rows(mergetest2,tempdataset1)
# View(mergetest3)
# 
# #order by site, then date
# finalmerge <- mergetest3[order(mergetest3$namedLocation,mergetest3$collectDate),]
# View(finalmerge)
# # ####TEMP DATASET THAT I FOUND###
# # tempdata <- loadByProduct(dpID = 'DP1.00098.001', site = 'KONZ')
# # View(tempdata$RH_30min)
# 
# write.csv(finalmerge,"D:/NEONexample/full_allcounts.csv",row.names = FALSE)
# 
# 
# 
# 
# # *PRECIPITATION DATA ----
# #for combining the downloaded data into tables:
# stackByTable(filepath="/Users/a426f262/Downloads/Directly Downloaded Data/NEON_precipitation.zip")
# #load the stacked data:
# tempdataset <- read.csv("C:/Users/bigfo/OneDrive/Desktop/school/neon data/trip asp air temp data/NEON_site-mgt-and-event-report/stackedFiles/wss_daily_temp.csv")
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# #order by site, then date
# finalmerge <- mergetest3[order(mergetest3$namedLocation,mergetest3$collectDate),]
# View(finalmerge)
# # ####TEMP DATASET THAT I FOUND###
# # tempdata <- loadByProduct(dpID = 'DP1.00098.001', site = 'KONZ')
# # View(tempdata$RH_30min)
# 
# write.csv(finalmerge,"D:/NEONexample/full_allcounts.csv",row.names = FALSE)

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
#somehow some rows of only NAs has been included. Let's remove that:
df1 <- df1[rowSums(is.na(df1)) != ncol(df1),]

#now we need to remove the entries from the field data that had ticks collected so that we avoid duplicate data when merging
df2zeros <- df2[(df2$adultCount == 0 | is.na(df2$adultCount)) & (df2$nymphCount == 0 | is.na(df2$nymphCount)) & (df2$larvaCount == 0 | is.na(df2$larvaCount)) & !is.na(df2$totalSampledArea),]
#The above is correct, however, we also need to convert those sampling instances that collected only other species to absence data for a americanum, thus we must edit df1
#start by splitting into a americanum collections and not (i.e. all the other species). Uisng a simple != didn't work for some reason, so
#we split each species up and combine depending on the species of interest
df1americanum <- df1[df1$scientificName == 'Amblyomma americanum',]
df1maculatum <- df1[df1$scientificName == 'Amblyomma maculatum',]
df1andersoni <- df1[df1$scientificName == 'Dermacentor andersoni',]
df1occidentalis <- df1[df1$scientificName == 'Dermacentor occidentalis',]
df1parumapertus <- df1[df1$scientificName == 'Dermacentor parumapertus',]
df1variabilis <- df1[df1$scientificName == 'Dermacentor variabilis',]
df1leporispalustris <- df1[df1$scientificName == 'Haemaphysalis leporispalustris',]
df1longicornis <- df1[df1$scientificName == 'Haemaphysalis longicornis',]
df1affinis <- df1[df1$scientificName == 'Ixodes affinis',]
df1angustus <- df1[df1$scientificName == 'Ixodes angustus',]
df1dentatus <- df1[df1$scientificName == 'Ixodes dentatus',]
df1marxi <- df1[df1$scientificName == 'Ixodes marxi',]
df1muris <- df1[df1$scientificName == 'Ixodes muris',]
df1pacificus <- df1[df1$scientificName == 'Ixodes pacificus',]
df1scapularis <- df1[df1$scientificName == 'Ixodes scapularis',]

#now combine all of the df of the other species
df1not <- dplyr::bind_rows(df1maculatum,df1andersoni,df1occidentalis,df1parumapertus,df1variabilis,
                           df1leporispalustris,df1longicornis,df1affinis,df1angustus,df1dentatus,
                           df1marxi,df1muris,df1pacificus,df1scapularis)

#FROM PREVIOUS METHOD (GAVE DIFFERENT COUNTS FOR !=)
# df1notI <- df1[df1$scientificName != 'Ixodes scapularis',]
# df1not1I <- df1notI %>% filter((!df1notI$collectDate %in% df1scapularis$collectDate))
# df1check1I <- df1notI %>% filter((df1notI$collectDate %in% df1scapularis$collectDate) & (!df1notI$plotID %in% df1scapularis$plotID))
# df1notI <- dplyr::bind_rows(df1not1I, df1check1I)
# df1I <- dplyr::bind_rows(df1scapularis, df1notI)

#Now we need to compare the dates and plots of the two dataframes and remove those that match from the "not" dataframe. 
#This allows us to include only those sampling events where they sampled and collected only ticks OTHER than a americanum. (https://stackoverflow.com/questions/72546014/remove-rows-that-do-not-match-common-dates-from-a-separate-data-frame)
#This is a bit tricky. Technically, it is possible that by using this filter, we remove sampling instances that occurred on
#the same day and at the same time, but at different plots, but I think this is unlikely. The only way that we could ensure
#that we don't remove relevant records is to create a second dataframe checking for those instances
# df1not1 <- df1not %>% filter((!df1not$collectDate %in% df1americanum$collectDate))
# df1check1 <- df1not %>% filter((df1not$collectDate %in% df1americanum$collectDate) & (!df1not$plotID %in% df1americanum$plotID))
# df1not <- dplyr::bind_rows(df1not1, df1check1)

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
#NEW 07-20-23
#order by site, then date
combineddata13_ordered <- combineddata13[order(combineddata13$plotID,combineddata13$collectDate),]

#add new fire data column for rpesence of fire in same year
combineddata13_ordered$fire_within_same_year <- 0

#move collectDate to front to make it easier to figure out which sites need values adjusted
library(dplyr)
combineddata13 <- combineddata13 %>% relocate(collectDate)
#adjust 0 to 1 manually e.g.
#combineddata13[c(4266,4267,4268), 111] <- 1

#*TEMP, PRECIP, VPD, DEW TEMP DATA ----
#for combining the downloaded data:
file_names_temp <- dir('C:/Users/bigfo/OneDrive/Desktop/Research/PRISM Data/final_adjusted/final',full.names = TRUE) #where you have your files

temp_frame <- do.call(rbind,lapply(file_names_temp,read.csv)) #read them all and combine into a single dataframe

temp_frame$Date <- mdy(temp_frame$Date) #convert dates to something R can understand

# ##THIS IS UNNEEDED, BUT WORKS:
# tempdataset <- read.csv("C:/Users/bigfo/OneDrive/Desktop/Research/PRISM Data/final_adjusted/final/PRISM_ppt_tmin_tmean_tmax_tdmean_vpdmin_vpdmax_provisional_4km_20220101_20221231.csv")
# tempdataset1 <- read.csv("C:/Users/bigfo/OneDrive/Desktop/Research/PRISM Data/final_adjusted/final/PRISM_ppt_tmin_tmean_tmax_tdmean_vpdmin_vpdmax_provisional_4km_20230101_20230331.csv")
# tempdataset2 <- read.csv("C:/Users/bigfo/OneDrive/Desktop/Research/PRISM Data/final_adjusted/final/PRISM_ppt_tmin_tmean_tmax_tdmean_vpdmin_vpdmax_stable_4km_20130101_20131231.csv")
# tempdataset3 <- read.csv("C:/Users/bigfo/OneDrive/Desktop/Research/PRISM Data/final_adjusted/final/PRISM_ppt_tmin_tmean_tmax_tdmean_vpdmin_vpdmax_stable_4km_20140101_20141231.csv")
# tempdataset4 <- read.csv("C:/Users/bigfo/OneDrive/Desktop/Research/PRISM Data/final_adjusted/final/PRISM_ppt_tmin_tmean_tmax_tdmean_vpdmin_vpdmax_stable_4km_20150101_20151231.csv")
# tempdataset5 <- read.csv("C:/Users/bigfo/OneDrive/Desktop/Research/PRISM Data/final_adjusted/final/PRISM_ppt_tmin_tmean_tmax_tdmean_vpdmin_vpdmax_stable_4km_20160101_20161231.csv")
# tempdataset6 <- read.csv("C:/Users/bigfo/OneDrive/Desktop/Research/PRISM Data/final_adjusted/final/PRISM_ppt_tmin_tmean_tmax_tdmean_vpdmin_vpdmax_stable_4km_20170101_20171231.csv")
# tempdataset7 <- read.csv("C:/Users/bigfo/OneDrive/Desktop/Research/PRISM Data/final_adjusted/final/PRISM_ppt_tmin_tmean_tmax_tdmean_vpdmin_vpdmax_stable_4km_20180101_20181231.csv")
# tempdataset8 <- read.csv("C:/Users/bigfo/OneDrive/Desktop/Research/PRISM Data/final_adjusted/final/PRISM_ppt_tmin_tmean_tmax_tdmean_vpdmin_vpdmax_stable_4km_20190101_20191231.csv")
# tempdataset9 <- read.csv("C:/Users/bigfo/OneDrive/Desktop/Research/PRISM Data/final_adjusted/final/PRISM_ppt_tmin_tmean_tmax_tdmean_vpdmin_vpdmax_stable_4km_20200101_20201231.csv")
# tempdataset10 <- read.csv("C:/Users/bigfo/OneDrive/Desktop/Research/PRISM Data/final_adjusted/final/PRISM_ppt_tmin_tmean_tmax_tdmean_vpdmin_vpdmax_stable_4km_20210101_20211231.csv")
# 
# tempdataset$Date <- mdy(tempdataset$Date)
# tempdataset1$Date <- mdy(tempdataset1$Date)
# tempdataset2$Date <- mdy(tempdataset2$Date)
# tempdataset3$Date <- mdy(tempdataset3$Date)
# tempdataset4$Date <- mdy(tempdataset4$Date)
# tempdataset5$Date <- mdy(tempdataset5$Date)
# tempdataset6$Date <- mdy(tempdataset6$Date)
# tempdataset7$Date <- mdy(tempdataset7$Date)
# tempdataset8$Date <- mdy(tempdataset8$Date)
# tempdataset9$Date <- mdy(tempdataset9$Date)
# tempdataset10$Date <- mdy(tempdataset10$Date)
# 
# fusetemp <- dplyr::bind_rows(tempdataset,tempdataset1,tempdataset2,tempdataset3,tempdataset4,tempdataset5,tempdataset6,tempdataset7,tempdataset8,tempdataset9,tempdataset10)
# ##
#save if needed, we will combine with the other dataset later
write.csv(temp_frame,"C:/Users/bigfo/OneDrive/Desktop/Research/NEON Data/Temperature Data Cleaned/prismdata.csv",row.names = FALSE)


#*COMBINE TEMP DATA 
#load data and combine with temp data
combineddata <- read.csv("C:/Users/bigfo/OneDrive/Desktop/Research/NEON Data/Tick NEON Data Cleaned/AAmericanum_cleaned_withFire.csv")
originalDate <- combineddata$collectDate
combineddata$collectDate <- mdy_hm(combineddata$collectDate)
combineddata$collectDate <- as.Date(combineddata$collectDate)

#Just remove the non-matching part and create a new column to be removed later
#combineddata$namedLocation1 = substr(combineddata$namedLocation,1,nchar(combineddata$namedLocation)-4)

combineddata1 <- merge(combineddata,temp_frame, by.x = c("plotID","collectDate"), by.y = c("Name","Date"), all.x = TRUE)

#fill blanks with NA, need to write it to csv and load to convert columns to character before running
combineddata2 <- replace(combineddata1, combineddata1=='', NA)

#had to correct the controlled burn for dsny
combineddata2 <- read.csv("C:/Users/bigfo/OneDrive/Desktop/combined.csv")
combineddata2$collectDate <- mdy(combineddata2$collectDate)
#

#convert to dates
combineddata2$collectDate <- ymd(combineddata2$collectDate)
combineddata2$endDateFire <- mdy(combineddata2$endDateFire)
#Add the days since last burn column:
combineddata2$daysSinceLastBurn <- difftime(combineddata2$endDateFire,combineddata2$collectDate, units = "days")
#this works, but gives negative # of days so let's make it positive
combineddata2$daysSinceLastBurn <- combineddata2$daysSinceLastBurn * -1
#check to make sure 
which(combineddata2$daysSinceLastBurn<0,arr.ind = TRUE)
#save to csv
write.csv(combineddata2,"C:/Users/bigfo/OneDrive/Desktop/combined.csv",row.names = FALSE)
#load combiend data
loadedcombineddata <- read.csv("C:/Users/bigfo/OneDrive/Desktop/Research/NEON Data/combined.csv")
combineddata2 <- loadedcombineddata

#repalce NAs with 0 to indicate absence
combineddata2$individualCount[is.na(combineddata2$individualCount)] <- 0



#*SOIL DATA ####
periodicsoildata <- loadByProduct(dpID = 'DP1.10086.001')
soildata <- loadByProduct(dpID = 'DP1.00096.001')
#View(soildata)
#separate tables of interest:
soilChemistry <- periodicsoildata$sls_soilChemistry
soilCore <- periodicsoildata$sls_soilCoreCollection
soilMoisture <- periodicsoildata$sls_soilMoisture
soilpH <- periodicsoildata$sls_soilpH
perbiogeo <- soildata$mgp_perbiogeosample
perbulk <- soildata$mgp_perbulksample
perhorizon <- soildata$mgp_perhorizon
megapit <- soildata$mgp_permegapit

#preprocessing:
combineddata3 <- combineddata2
combineddata3$plotID1 = substr(combineddata2$plotID,1,nchar(combineddata2$plotID)-4)

#remove/subset unneeded/needed columns
soilChemistry <- subset(soilChemistry, select=c(siteID,collectDate,d15N,organicd13C,nitrogenPercent,organicCPercent,CNratio))

soilCore <- subset(soilCore, select=c(siteID,collectDate,soilTemp,litterDepth))
  
soilMoisture <- subset(soilMoisture, select=c(siteID,collectDate,soilMoisture))

soilpH <- subset(soilpH, select=c(siteID,collectDate,soilInWaterpH,soilInCaClpH))

perbiogeo <- subset(perbiogeo, select=-c(uid,domainID,pitNamedLocation,pitID,horizonID,biogeoID,horizonName,biogeoHorizonProportion,
                                         biogeoSampleType,setDate,laboratoryName,labProjID,biogeoTopDepth,biogeoCenterDepth,biogeoBottomDepth,remarks,
                                         publicationDate,release))

perbulk <- subset(perbulk, select=-c(uid,domainID,pitNamedLocation,pitID,horizonID,bulkDensID,horizonName,setDate,laboratoryName,
                                     labProjID,bulkDensTopDepth,bulkDensCenterDepth,bulkDensBottomDepth,bulkDensHorizonProportion,bulkDensSampleType,
                                     remarks,publicationDate,release))

perhorizon <- subset(perhorizon, select=-c(uid,domainID,pitNamedLocation,pitID,horizonID,horizonName,setDate,nrcsDescriptionID,remarks,
                                           publicationDate,release))

megapit <- subset(megapit, select=-c(uid,domainID,pitNamedLocation,pitID,decimalLatitude,decimalLongitude,geodeticDatum,coordinateUncertainty,
                                     elevation,elevationUncertainty,nlcdClass,setDate,samplingProtocolVersion,recordedByA,recordedByB,recordedByC,
                                     recordedByD,recordedByE,soilProfileDescriberA,soilProfileDescriberB,soilProfileDescriberC,soilProfileDescriberD,
                                     soilProfileDescriberE,soilProfileDescriberF,soilProfileDescriberInst,pitDepth,nrcsDescriptionID,
                                     publicationDate,release))
#convert to dataframes (currently lists)
perbiogeo <- data.frame(perbiogeo)
perbulk <- data.frame(perbulk)
perhorizon <- data.frame(perhorizon)
megapit <- data.frame(megapit)

#format dates (remove HMS)
soilChemistry$collectDate <- ymd_hms(soilChemistry$collectDate)
soilCore$collectDate <- ymd_hms(soilCore$collectDate)
soilMoisture$collectDate <- ymd_hms(soilMoisture$collectDate)
soilpH$collectDate <- ymd_hms(soilpH$collectDate)

soilChemistry <- soilChemistry %>% mutate(collectDate = as_date(collectDate))
soilCore <- soilCore %>% mutate(collectDate = as_date(collectDate))
soilMoisture <- soilMoisture %>% mutate(collectDate = as_date(collectDate))
soilpH <- soilpH %>% mutate(collectDate = as_date(collectDate))

soilChemistry$collectDate <- ymd(soilChemistry$collectDate)
soilMoisture$collectDate <- ymd(soilMoisture$collectDate)
soilCore$collectDate <- ymd(soilCore$collectDate)
#merge based on nearest date:
combineddata10 <- lapply(intersect(combineddata9$plotID1,soilMoisture$siteID),function(id) {
  d1 <- subset(combineddata9,plotID1==id)
  d2 <- subset(soilMoisture,siteID==id)
  
  d1$indices <- sapply(d1$collectDate,function(d) which.min(abs(d2$collectDate - d)))
  d2$indices <- 1:nrow(d2)
  
  merge(d1,d2,by.x=c('plotID1','indices'),by.y=c('siteID','indices'),all.x = T)
})

combineddata10 <- do.call(rbind,combineddata10)
combineddata10$indices <- NULL

combineddata10$collectDate <- ymd(combineddata10$collectDate.x)
combineddata10$collectDate.x <- NULL

colnames(combineddata10)[95] <- "nearestDateMoisture"
combineddata10 <- data.frame(combineddata10)

combineddata11 <- lapply(intersect(combineddata10$plotID1,soilCore$siteID),function(id) {
  d1 <- subset(combineddata10,plotID1==id)
  d2 <- subset(soilCore,siteID==id)
  
  d1$indices <- sapply(d1$collectDate,function(d) which.min(abs(d2$collectDate - d)))
  d2$indices <- 1:nrow(d2)
  
  merge(d1,d2,by.x=c('plotID1','indices'),by.y=c('siteID','indices'),all.x = T)
})

combineddata11 <- do.call(rbind,combineddata11)
combineddata11$indices <- NULL

combineddata11$collectDate <- ymd(combineddata11$collectDate.x)
combineddata11$collectDate.x <- NULL

colnames(combineddata11)[98] <- "nearestDateSTandLD"
combineddata11 <- data.frame(combineddata11)

combineddata12 <- lapply(intersect(combineddata11$plotID1,soilpH$siteID),function(id) {
  d1 <- subset(combineddata11,plotID1==id)
  d2 <- subset(soilpH,siteID==id)
  
  d1$indices <- sapply(d1$collectDate,function(d) which.min(abs(d2$collectDate - d)))
  d2$indices <- 1:nrow(d2)
  
  merge(d1,d2,by.x=c('plotID1','indices'),by.y=c('siteID','indices'),all.x = T)
})

combineddata12 <- do.call(rbind,combineddata12)
combineddata12$indices <- NULL

combineddata12$collectDate <- ymd(combineddata12$collectDate.x)
combineddata12$collectDate.x <- NULL

colnames(combineddata12)[100] <- "nearestDateSoilIn"
combineddata12 <- data.frame(combineddata12)


#soil Chemistry, Really doesn'tlike BARR for some reason 
combinedbarr <- combineddata12[combineddata12$plotID1 == "BARR",]

combineddata13 <- lapply(intersect(combineddata12$plotID1,soilChemistry$siteID),function(id) {
  d1 <- subset(combineddata12,plotID1==id)
  d2 <- subset(soilChemistry,siteID==id)
  
  d1$indices <- sapply(d1$collectDate,function(d) which.min(abs(d2$collectDate - d)))
  d2$indices <- 1:nrow(d2)
  
  merge(d1,d2,by.x=c('plotID1','indices'),by.y=c('siteID','indices'),all.x = T)
})

combineddata13 <- do.call(rbind,combineddata13)
combineddata13$indices <- NULL

combineddata13$collectDate <- ymd(combineddata13$collectDate.x)
combineddata13$collectDate.x <- NULL

colnames(combineddata13)[103] <- "nearestDateSoilChem"
combineddata13 <- bind_rows(combineddata13,combinedbarr)

#We need to average the values for perbiogeo, perbulk, and perhorizon:
#Works, but cant ignore NAs
# perbiogeo%>%group_by(siteID)%>%summarise_at(vars("coarseFrag2To5","coarseFrag5To20","sandTotal","siltTotal","clayTotal","carbonateClay",
#                                          "clayFineContent","siltFineContent","siltCoarseContent","sandVeryFineContent",
#                                          "sandFineContent","sandMediumContent","sandCoarseContent","sandVeryCoarseContent",
#                                          "carbonTot","nitrogenTot","sulfurTot","estimatedOC","alMjelm","caMjelm","feMjelm",
#                                          "kMjelm","mgMjelm","mnMjelm","naMjelm","pMjelm","siMjelm","srMjelm","tiMjelm",
#                                          "zrMjelm","phCacl2","phH2o","ec12pre","gypsumConc","caco3Conc","caNh4d","kNh4d",
#                                          "mgNh4d","naNh4d","cecdNh4","alSatCecd33","baseSumCecd10","bsesatCecd10","ececCecd33",
#                                          "alKcl","feKcl","mnKcl","bSatx","brSatx","caSatx","clSatx","co3Satx","ecSatp",
#                                          "flSatx","waterSatx","hco3Sx","kSatx","mgSatx","naSatx","no2Satx","no3Satx","pSatx",
#                                          "phSp","resist","so4Satx"),mean)
perbiogeo <- perbiogeo %>% group_by(siteID) %>% 
  summarise(mean_coarseFrag2To5=mean(coarseFrag2To5,na.rm = T),mean_coarseFrag5To20=mean(coarseFrag5To20,na.rm = T),mean_sandTotal=mean(sandTotal,na.rm = T),
            mean_siltTotal=mean(siltTotal,na.rm = T),mean_clayTotal=mean(clayTotal,na.rm = T),mean_carbonateClay=mean(carbonateClay,na.rm = T),mean_airDryOvenDry=mean(airDryOvenDry,na.rm = T),
            mean_clayFineContent=mean(clayFineContent,na.rm = T),mean_siltFineContent=mean(siltFineContent,na.rm = T),mean_siltCoarseContent=mean(siltCoarseContent,na.rm = T),
            mean_sandVeryFineContent=mean(sandVeryFineContent,na.rm = T),mean_sandFineContent=mean(sandFineContent,na.rm = T),mean_sandMediumContent=mean(sandMediumContent,na.rm = T),
            mean_sandCoarseContent=mean(sandCoarseContent,na.rm = T),mean_sandVeryCoarseContent=mean(sandVeryCoarseContent,na.rm = T),
            mean_carbonTot=mean(carbonTot,na.rm = T),mean_nitrogenTot=mean(nitrogenTot,na.rm = T),mean_sulfurTot=mean(sulfurTot,na.rm = T),
            mean_estimatedOC=mean(estimatedOC,na.rm = T),mean_alMjelm=mean(alMjelm,na.rm = T),mean_caMjelm=mean(caMjelm,na.rm = T),
            mean_feMjelm=mean(feMjelm,na.rm = T),mean_kMjelm=mean(kMjelm,na.rm = T),mean_mgMjelm=mean(mgMjelm,na.rm = T),
            mean_mnMjelm=mean(mnMjelm,na.rm = T),mean_naMjelm=mean(naMjelm,na.rm = T),mean_pMjelm=mean(pMjelm,na.rm = T),
            mean_siMjelm=mean(siMjelm,na.rm = T),mean_srMjelm=mean(srMjelm,na.rm = T),
            mean_tiMjelm=mean(tiMjelm,na.rm = T),mean_zrMjelm=mean(zrMjelm,na.rm = T),mean_phCacl2=mean(phCacl2,na.rm = T),
            mean_phH2o=mean(phH2o,na.rm = T),mean_ec12pre=mean(ec12pre,na.rm = T),mean_gypsumConc=mean(gypsumConc,na.rm = T),
            mean_caco3Conc=mean(caco3Conc,na.rm = T),mean_caNh4d=mean(caNh4d,na.rm = T),
            mean_kNh4d=mean(kNh4d,na.rm = T),mean_mgNh4d=mean(mgNh4d,na.rm = T),mean_naNh4d=mean(naNh4d,na.rm = T),
            mean_cecdNh4=mean(cecdNh4,na.rm = T),mean_alSatCecd33=mean(alSatCecd33,na.rm = T),mean_baseSumCecd10=mean(baseSumCecd10,na.rm = T),
            mean_bsesatCecd10=mean(bsesatCecd10,na.rm = T),mean_ececCecd33=mean(ececCecd33,na.rm = T),
            mean_alKcl=mean(alKcl,na.rm = T),mean_feKcl=mean(feKcl,na.rm = T),
            mean_mnKcl=mean(mnKcl,na.rm = T),mean_bSatx=mean(bSatx,na.rm = T),
            mean_brSatx=mean(brSatx,na.rm = T),mean_caSatx=mean(caSatx,na.rm = T),mean_clSatx=mean(clSatx,na.rm = T),
            mean_co3Satx=mean(co3Satx,na.rm = T),mean_ecSatp=mean(ecSatp,na.rm = T),
            mean_flSatx=mean(flSatx,na.rm = T),mean_waterSatx=mean(waterSatx,na.rm = T),mean_hco3Sx=mean(hco3Sx,na.rm = T),
            mean_kSatx=mean(kSatx,na.rm = T),mean_mgSatx=mean(mgSatx,na.rm = T),mean_naSatx=mean(naSatx,na.rm = T),
            mean_no2Satx=mean(no2Satx,na.rm = T),mean_no3Satx=mean(no3Satx,na.rm = T),
            mean_pSatx=mean(pSatx,na.rm = T),mean_phSp=mean(phSp,na.rm = T),
            mean_resist=mean(resist,na.rm = T),mean_so4Satx=mean(so4Satx,na.rm = T),
            .groups = 'drop') %>%
  as.data.frame()

perbulk <- perbulk %>% group_by(siteID) %>% 
  summarise(mean_bulkDensExclCoarseFrag=mean(bulkDensExclCoarseFrag,na.rm = T),
            .groups = 'drop') %>%
  as.data.frame()

perhorizon <- perhorizon %>% group_by(siteID) %>% 
  summarise(mean_horizonTopDepth=mean(horizonTopDepth,na.rm = T),mean_horizonBottomDepth=mean(horizonBottomDepth,na.rm = T),
            .groups = 'drop') %>%
  as.data.frame()

#combine with the dataset
combineddata3 <- merge(combineddata3,perbiogeo, by.x = 'plotID1', by.y = 'siteID',all.x = T)
combineddata3 <- merge(combineddata3,perbulk, by.x = 'plotID1', by.y = 'siteID',all.x = T)
combineddata3 <- merge(combineddata3,perhorizon, by.x = 'plotID1', by.y = 'siteID',all.x = T)
combineddata3 <- merge(combineddata3,megapit, by.x = 'plotID1', by.y = 'siteID',all.x = T)

#*EDDY COVARIANCE DATA####
# eddydata <- loadByProduct(dpID = 'DP4.00200.001')
# #wait on this for now, likely won't need
# View(eddydata)


#*COURSE DEBRIS DATA####
coursedata <- loadByProduct(dpID = 'DP1.10014.001')
View(coursedata)

densitydisk <- coursedata$cdw_densitydisk
densitylog <- coursedata$cdw_densitylog
#remove unneeded columns
densitydisk <- subset(densitydisk, select=-c(uid,domainID,siteID,diskID,startDate,ovenStartDate,ovenEndDate,dryingHours,weighDate,bagNumber,
                                         sampleID,sampleBarcode,subsampleID,subsampleCode,diskFreshMass,bulkDensVolume,sampleFreshMass,subsampleFreshMassRatio,
                                         dryMass,diskDryMass,qaSample,remarks,measuredBy,dataQF,publicationDate,release))

densitylog <- subset(densitylog, select=-c(uid,startDate,yearBoutBegan,eventID,domainID,siteID,plotType,decimalLatitude,decimalLongitude,
                                     geodeticDatum,coordinateUncertainty,elevation,elevationUncertainty,nlcdClass,
                                     samplingProtocolVersion,mappingMethod,pointID,logID,logAzimuth,logDistance,sampleEasting,
                                     sampleNorthing,vstTagID,sampleID,sampleBarcode,taxonID,identificationQualifier,
                                     leavesPresent,twigsPresent,branchBarkCover,logBarkCover,logHandBreakable,logHoldShape,
                                     remarks,recordedBy,measuredBy,dataQF,publicationDate,release))

#convert to dataframes (currently lists)
densitydisk <- data.frame(densitydisk)
densitylog <- data.frame(densitylog)

#We need to average the values for densitydisk and condense densitylog by scientific name:
densitydisk <- densitydisk %>% group_by(plotID) %>% 
  summarise(mean_diameter=mean(diameter,na.rm = T),mean_ninetyDiameter=mean(ninetyDiameter,na.rm = T),mean_maxDiskHeight=mean(maxDiskHeight,na.rm = T),
            mean_minDiskHeight=mean(minDiskHeight,na.rm = T),mean_aDiskHieght=mean(aDiskHeight,na.rm = T),mean_bDiskHeight=mean(bDiskHeight,na.rm = T),
            mean_bulkDensDisk=mean(bulkDensDisk,na.rm = T),
            .groups = 'drop') %>%
  as.data.frame()

densitylog <- densitylog %>% group_by(plotID)  %>% distinct(scientificName) %>%
  summarize(scientificName = paste(scientificName, collapse = ", ")) %>%
  as.data.frame()
#combine with the dataset, change name of scientificName column
colnames(densitylog)[2] <- "scientificNameLog"

combineddata3 <- merge(combineddata3,densitydisk, by = 'plotID',all.x = T)
combineddata3 <- merge(combineddata3,densitylog, by = 'plotID',all.x = T)

#*VEGETATION DATA####
plotvegdata <- loadByProduct(dpID = 'DP1.10017.001')
View(plotvegdata)
#separate datasets of interest:
vegperplot <- plotvegdata$dhp_perplot

#remove unneeded columns
vegperplot <- subset(vegperplot, select=-c(uid,domainID,namedLocation,siteID,geodeticDatum,
                                           decimalLatitude,decimalLongitude,coordinateUncertainty,
                                           utmZone,elevation,elevationUncertainty,nlcdClass,
                                           plotType,plotSize,startDate,aopCollectDate,samplingImpractical,
                                           biophysicalCriteria,eventID,sampleID,samplingProtocolVersion,
                                           snowPresent,remarks,measuredBy,recordedBy,publicationDate,release))
#convert to dataframes (currently lists)
vegperplot <- data.frame(vegperplot)

#We need to average the values for densitydisk and condense densitylog by scientific name:
vegperplot <- vegperplot %>% group_by(plotID) %>% 
  summarise(mean_slopeAspect=mean(slopeAspect,na.rm = T),mean_slopeGradient=mean(slopeGradient,na.rm = T),mean_understoryHeight=mean(understoryHeight,na.rm = T),
            mean_overstoryHeight=mean(overstoryHeight,na.rm = T),
            .groups = 'drop') %>%
  as.data.frame()

#combine with the dataset:
combineddata4 <- combineddata3
combineddata4 <- merge(combineddata4,vegperplot, by = 'plotID',all.x = T)

#*LITTERFALL AND FINE WOODY DEBRIS DATA####
finedata <- loadByProduct(dpID = 'DP1.10033.001')
View(finedata)

#separate datasets of interest:
finelitterCN <- finedata$ltr_litterCarbonNitrogen
finelign <- finedata$ltr_litterLignin

#remove unneeded columns
finelitterCN <- subset(finelitterCN, select=-c(uid,analysisDate,domainID,siteID,setDate,
                                           massSampleMixtureID,cnSampleID,cnSampleCode,plotType,
                                           sampleType,co2Trapped,CNratio,cnIsotopeQF,
                                           cnPercentQF,isotopeAccuracyQF,percentAccuracyQF,
                                           analyticalRepNumber,remarks,laboratoryName,testMethod,
                                           instrument,analyzedBy,reviewedBy,dataQF,publicationDate,
                                           release))
finelign <- subset(finelign, select=-c(uid,analysisDate,domainID,siteID,plotType,setDate,massSampleMixtureID,
                                       ligninSampleID,ligninSampleBarcode,analyticalRepNumber,dryMass,measurementQF,accuracyQF,
                                       remarks,laboratoryName,testMethod,analyzedBy,reviewedBy,dataQF,publicationDate,
                                       release))
#convert to dataframes (currently lists)
finelitterCN <- data.frame(finelitterCN)
finelign <- data.frame(finelign)

#We need to average the values for densitydisk and condense densitylog by scientific name:
finelitterCN <- finelitterCN %>% group_by(plotID) %>% 
  summarise(mean_d15N=mean(d15N,na.rm = T),mean_d13C=mean(d13C,na.rm = T),mean_nitrogenPercent=mean(nitrogenPercent,na.rm = T),
            mean_carbonPercent=mean(carbonPercent,na.rm = T),
            .groups = 'drop') %>%
  as.data.frame()

finelign <- finelign %>% group_by(plotID) %>% 
  summarise(mean_ligninPercent=mean(ligninPercent,na.rm = T),mean_cellulosePercent=mean(cellulosePercent,na.rm = T),
            .groups = 'drop') %>%
  as.data.frame()

#combine with the dataset:
combineddata5 <- combineddata4
combineddata5 <- merge(combineddata5,finelitterCN, by = 'plotID',all.x = TRUE)
combineddata5 <- merge(combineddata5,finelign, by = 'plotID', all.x = TRUE)

#*NDVI DATA####
#for combining the downloaded data. YOU MUST CHOOSE YOUR WORKING DIRECTORY FOR THIS:
file_names_ndvi <- dir('C:/Users/bigfo/OneDrive/Desktop/Research/NDVI/time series csv') #where you have your files

ndvi_frame <- do.call(rbind, lapply(file_names_ndvi, function(x) cbind(read.csv(x), name=strsplit(x,'\\.')[[1]][1]))) #read them all and combine into a single dataframe with file names

ndvi_frame$Date <- mdy(ndvi_frame$system.time_start) #convert dates to something R can understand

ndvi_frame$name <- toupper(ndvi_frame$name) #convert filenames to uppercase

#remove rows that contain NA from NDVI:
ndvi_frame <- na.omit(ndvi_frame)

#save if needed:
write.csv(temp_frame,"C:/Users/bigfo/OneDrive/Desktop/Research/NEON Data/Temperature Data Cleaned/prismdata.csv",row.names = FALSE)


#merge based on nearest date:
combineddata6 <- lapply(intersect(combineddata5$plotID,ndvi_frame$name),function(id) {
  d1 <- subset(combineddata5,plotID==id)
  d2 <- subset(ndvi_frame,name==id)
  
  d1$indices <- sapply(d1$collectDate,function(d) which.min(abs(d2$Date - d)))
  d2$indices <- 1:nrow(d2)
  
  merge(d1,d2,by.x=c('plotID','indices'),by.y=c('name','indices'),all.x = T)
})

combineddata6 <- do.call(rbind,combineddata6)
combineddata6$indices <- NULL
combineddata6$system.time_start <- NULL

#rename Date column to avoid issues later
colnames(combineddata6)[114] <- "nearestDateNDVI"

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
finalmammaldata1 <- aggregate(siteID~ collectDate + plotID, data = mammaldata_pertrap1, length)

finalmammaldata2 <- aggregate(nlcdClass~ collectDate + siteID, data = mammaldata_pertrap1, length)

colnames(finalmammaldata1)[1] <- "Date"
colnames(finalmammaldata1)[2] <- "namedLocation"
colnames(finalmammaldata1)[3] <- "count"

colnames(finalmammaldata2)[1] <- "Date"
colnames(finalmammaldata2)[2] <- "namedLocation"
colnames(finalmammaldata2)[3] <- "countMammalSite"

finalmammaldata1$Date <- ymd(finalmammaldata1$Date)

finalmammaldata2$Date <- ymd(finalmammaldata2$Date)

#merge based on nearest date:
combineddata7 <- lapply(intersect(combineddata6$plotID,finalmammaldata1$namedLocation),function(id) {
  d1 <- subset(combineddata6,plotID==id)
  d2 <- subset(finalmammaldata1,namedLocation==id)
  
  d1$indices <- sapply(d1$collectDate,function(d) which.min(abs(d2$Date - d)))
  d2$indices <- 1:nrow(d2)
  
  merge(d1,d2,by.x=c('plotID','indices'),by.y=c('namedLocation','indices'),all.x = T)
})

combineddata7 <- do.call(rbind,combineddata7)
combineddata7$indices <- NULL

#the above only includes that that are listed in namedLocation, thus we need to subset the original 
#dataframe and combine it with the new data
opposite_intersection <- combineddata6$plotID[!(combineddata6$plotID %in% finalmammaldata1$namedLocation)]
sub_combineddata6 <- subset(combineddata6, plotID %in% opposite_intersection)

#rename Date and add empty columns with same names to make merging easy
colnames(combineddata7)[115] <- "nearestDateMammal"
colnames(combineddata7)[116] <- "countMammal"

sub_combineddata6$nearestDateMammal = NA
sub_combineddata6$countMammal = NA

#make sure column names match
my_func <- function(x,y) {
  for (i in names(x)) {
    if (!(i %in% names(y))) {
      print('Warning: Names are not the same')
      break
    }  
    else if(i==tail(names(y),n=1)) {
      print('Names are identical')
    }
  }
}

my_func(combineddata7,sub_combineddata6)

#merge
combineddata7 <- merge(combineddata7,sub_combineddata6, all = T)

#now do it with counts aggregated to the site rather than the plot
#merge based on nearest date:
combineddata8 <- lapply(intersect(combineddata7$plotID1,finalmammaldata2$namedLocation),function(id) {
  d1 <- subset(combineddata7,plotID1==id)
  d2 <- subset(finalmammaldata2,namedLocation==id)
  
  d1$indices <- sapply(d1$collectDate,function(d) which.min(abs(d2$Date - d)))
  d2$indices <- 1:nrow(d2)
  
  merge(d1,d2,by.x=c('plotID1','indices'),by.y=c('namedLocation','indices'),all.x = T)
})

combineddata8 <- do.call(rbind,combineddata8)
combineddata8$indices <- NULL

#rename date column
colnames(combineddata8)[117] <- "nearestDateMammalSite"
#site names are matched throughout the dataset since we are using the 4 letter version, so nothing else is needed!

#*BIRD DATA####
birddata <- loadByProduct(dpID = 'DP1.10003.001')

birdcountdata <- birddata$brd_countdata

birdcountdata$startDate <- format(as.POSIXct(birdcountdata$startDate,format='%Y-%m-%d %H:%M:%S'),format='%Y-%m-%d')

#aggregate by site and plot 
birdcountdata1 <- aggregate(siteID~ startDate + plotID, data = birdcountdata, length)

birdcountdata2 <- aggregate(plotID~ startDate + siteID, data = birdcountdata, length)

colnames(birdcountdata1)[1] <- "nearestDateBird"
colnames(birdcountdata1)[2] <- "plotIDBird"
colnames(birdcountdata1)[3] <- "countBird"

colnames(birdcountdata2)[1] <- "nearestDateBirdSite"
colnames(birdcountdata2)[2] <- "siteIDBird"
colnames(birdcountdata2)[3] <- "countBirdSite"

birdcountdata1$nearestDateBird <- ymd(birdcountdata1$nearestDateBird)
birdcountdata2$nearestDateBirdSite <- ymd(birdcountdata2$nearestDateBirdSite)
#merge based on nearest date:
combineddata9 <- lapply(intersect(combineddata8$plotID,birdcountdata1$plotIDBird),function(id) {
  d1 <- subset(combineddata8,plotID==id)
  d2 <- subset(birdcountdata1,plotIDBird==id)
  
  d1$indices <- sapply(d1$collectDate,function(d) which.min(abs(d2$nearestDateBird - d)))
  d2$indices <- 1:nrow(d2)
  
  merge(d1,d2,by.x=c('plotID','indices'),by.y=c('plotIDBird','indices'),all.x = T)
})

combineddata9 <- do.call(rbind,combineddata9)
combineddata9$indices <- NULL
#the above only includes that that are listed in namedLocation, thus we need to subset the original 
#dataframe and combine it with the new data
opposite_intersection <- combineddata8$plotID[!(combineddata8$plotID %in% birdcountdata1$plotIDBird)]
sub_combineddata9 <- subset(combineddata8, plotID %in% opposite_intersection)

#add empty columns with same names to make merging easy
sub_combineddata9$nearestDateBird = NA
sub_combineddata9$countBird = NA

#make sure column names match
my_func(combineddata9,sub_combineddata9)

#merge
combineddata9 <- merge(combineddata9,sub_combineddata9, all = T)

#merge based on nearest date:
combineddata9 <- lapply(intersect(combineddata9$plotID1,birdcountdata2$siteIDBird),function(id) {
  d1 <- subset(combineddata9,plotID1==id)
  d2 <- subset(birdcountdata2,siteIDBird==id)
  
  d1$indices <- sapply(d1$collectDate,function(d) which.min(abs(d2$nearestDateBirdSite - d)))
  d2$indices <- 1:nrow(d2)
  
  merge(d1,d2,by.x=c('plotID1','indices'),by.y=c('siteIDBird','indices'),all.x = T)
})

combineddata9 <- do.call(rbind,combineddata9)
combineddata9$indices <- NULL
#*PLOTS####
#PLOTS FOR ENVIRO CLASS
#basic plot + count
ggplot(data=combineddata13, mapping = aes(x = nlcdClass, y = individualCount)) + geom_count(stat = "identity") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
combineddata13 %>% count(nlcdClass)
# Calculate the average individualCount per Envrio class
avgIndividualCountNLCD = combineddata13 %>%
  group_by(nlcdClass) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountNLCD, aes(x = nlcdClass, y = avgCount)) +
  geom_point() +
  xlab("Environment") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per Enivromental Class") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#PLOTS FOR ELEVATION
#convert to numeric
combineddata13$elevation <- as.numeric(combineddata13$elevation)
#basic plot and count per temp measurement
plot(combineddata13$elevation,combineddata13$individualCount)
combineddata13 %>% count(elevation)
# Calculate the average individualCount per avg temp
avgIndividualCountElevation = combineddata13 %>%
  group_by(elevation) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountElevation, aes(x = elevation, y = avgCount)) +
  geom_point() +
  xlab("Elevation") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per Elevation")

#PLOTS FOR ELEVATION PRISM
#convert to numeric
combineddata13$Elevation <- as.numeric(combineddata13$Elevation)
#basic plot and count per temp measurement
plot(combineddata13$Elevation,combineddata13$individualCount)
combineddata13 %>% count(Elevation)
# Calculate the average individualCount per avg temp
avgIndividualCountElevationPRISM = combineddata13 %>%
  group_by(Elevation) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountElevationPRISM, aes(x = Elevation, y = avgCount)) +
  geom_point() +
  xlab("Elevation") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per Elevation")

#PLOTS FOR TSA
plot(combineddata13$totalSampledArea,combineddata13$individualCount)
combineddata13 %>% count(totalSampledArea)
#convert totalsampledarea to numeric
combineddata13$totalSampledArea <- as.numeric(combineddata13$totalSampledArea)
# Calculate the average individualCount per totalSampledArea
avgIndividualCountTSA = combineddata13 %>%
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

#PLOTS FOR FIRE SEVERITY
#basic plot
ggplot(data=combineddata13, mapping = aes(x = fireSeverity, y = individualCount)) + geom_count(stat = "identity") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
combineddata13 %>% count(fireSeverity)
# Calculate the average individualCount per severity
avgIndividualCountSev = combineddata13 %>%
  group_by(fireSeverity) %>%
  summarize(avgCount = mean(individualCount))
avgIndividualCountSev[5,1] <- "No Burns"
# Create a scatter plot
ggplot(avgIndividualCountSev, aes(x = fireSeverity, y = avgCount)) +
  geom_point() +
  xlab("Fire Severity") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per Fire Severity") +
  scale_x_discrete(limits = c("low","medium","high","unknown","No Burns")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#PLOTS FOR PRECIP
#convert to numeric
combineddata13$ppt <- as.numeric(combineddata13$ppt)
#basic plot
plot(combineddata13$ppt,combineddata13$individualCount)
combineddata13 %>% count(ppt)
# Calculate the average individualCount per ppt
avgIndividualCountPPT = combineddata13 %>%
  group_by(ppt) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountPPT, aes(x = ppt, y = avgCount)) +
  geom_point() +
  xlab("Precipitation") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per Precipitation")
#add column for days since last precipitation?

#PLOTS FOR TEMP
#convert to numeric
combineddata13$tmean <- as.numeric(combineddata13$tmean)
#basic plot and count per temp measurement
plot(combineddata13$tmean,combineddata13$individualCount)
combineddata13 %>% count(tmean)
# Calculate the average individualCount per avg temp
avgIndividualCountTmean = combineddata13 %>%
  group_by(tmean) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountTmean, aes(x = tmean, y = avgCount)) +
  geom_point() +
  xlab("Average Temperature") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per Average Temp")

#PLOTS FOR  Min TEMP
#convert to numeric
combineddata13$tmin <- as.numeric(combineddata13$tmin)
#basic plot and count per temp measurement
plot(combineddata13$tmin,combineddata13$individualCount)
combineddata13 %>% count(tmin)
# Calculate the average individualCount per min temp
avgIndividualCountTmin = combineddata13 %>%
  group_by(tmin) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountTmin, aes(x = tmin, y = avgCount)) +
  geom_point() +
  xlab("Minimum Temperature") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per Min Temp")

#PLOTS FOR  Max TEMP
#convert to numeric
combineddata13$tmax <- as.numeric(combineddata13$tmax)
#basic plot and count per temp measurement
plot(combineddata13$tmax,combineddata13$individualCount)
combineddata13 %>% count(tmax)
# Calculate the average individualCount per min temp
avgIndividualCountTmax = combineddata13 %>%
  group_by(tmax) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountTmax, aes(x = tmax, y = avgCount)) +
  geom_point() +
  xlab("Maximum Temperature") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per Max Temp")

#PLOTS FOR  min vdp
#convert to numeric
combineddata13$vpdmin <- as.numeric(combineddata13$vpdmin)
#basic plot and count per temp measurement
plot(combineddata13$vpdmin,combineddata13$individualCount)
combineddata13 %>% count(vpdmin)
# Calculate the average individualCount per min temp
avgIndividualCountVPDmin = combineddata13 %>%
  group_by(vpdmin) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountVPDmin, aes(x = vpdmin, y = avgCount)) +
  geom_point() +
  xlab("Minimum VPD") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per Min VPD")

#PLOTS FOR  max vdp
#convert to numeric
combineddata13$vpdmax <- as.numeric(combineddata13$vpdmax)
#basic plot and count per temp measurement
plot(combineddata13$vpdmax,combineddata13$individualCount)
combineddata13 %>% count(vpdmax)
# Calculate the average individualCount per min temp
avgIndividualCountVPDmax = combineddata13 %>%
  group_by(vpdmax) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountVPDmax, aes(x = vpdmax, y = avgCount)) +
  geom_point() +
  xlab("Maximum VPD") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per Max VPD")

#PLOTS FOR DAYS SINCE LAST BURN
plot(combineddata13$daysSinceLastBurn,combineddata13$individualCount)
combineddata13 %>% count(daysSinceLastBurn)
# Calculate the average individualCount per #ofdaysSinceLastBurn
avgIndividualCountDSLB = combineddata13 %>%
  group_by(daysSinceLastBurn) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountDSLB, aes(x = daysSinceLastBurn, y = avgCount)) +
  geom_point() +
  xlab("Days Since Last Burn") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per # of Days Since Last Burn")

#PLOTS FOR course frag 2 to 5
plot(combineddata13$mean_coarseFrag2To5,combineddata13$individualCount)
combineddata13 %>% count(mean_coarseFrag2To5)
# Calculate the average individualCount per mean_coarseFrag2To5
avgIndividualCountCourse2to5 = combineddata13 %>%
  group_by(mean_coarseFrag2To5) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountCourse2to5, aes(x = mean_coarseFrag2To5, y = avgCount)) +
  geom_point() +
  xlab("Course Fragments 2 to 5") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per Course Frag 2 to 5")

#PLOTS FOR course frag 5 to 20
plot(combineddata13$mean_coarseFrag5To20,combineddata13$individualCount)
combineddata13 %>% count(mean_coarseFrag5To20)
# Calculate the average individualCount per mean_coarseFrag2To5
avgIndividualCountCourse5to20 = combineddata13 %>%
  group_by(mean_coarseFrag5To20) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountCourse5to20, aes(x = mean_coarseFrag5To20, y = avgCount)) +
  geom_point() +
  xlab("Course Fragments 5 to 20") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per Course Frag 5 to 20")

#PLOTS FOR sand total
plot(combineddata13$mean_sandTotal,combineddata13$individualCount)
combineddata13 %>% count(mean_sandTotal)
# Calculate the average individualCount per sandTotal
avgIndividualCountSandTotal = combineddata13 %>%
  group_by(mean_sandTotal) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountSandTotal, aes(x = mean_sandTotal, y = avgCount)) +
  geom_point() +
  xlab("Sand Total") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per Sand Total")

#PLOTS FOR silt total
plot(combineddata13$mean_siltTotal,combineddata13$individualCount)
combineddata13 %>% count(mean_siltTotal)
# Calculate the average individualCount per sandTotal
avgIndividualCountSiltTotal = combineddata13 %>%
  group_by(mean_siltTotal) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountSiltTotal, aes(x = mean_siltTotal, y = avgCount)) +
  geom_point() +
  xlab("Silt Total") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per Silt Total")

#PLOTS FOR clay total
plot(combineddata13$mean_clayTotal,combineddata13$individualCount)
combineddata13 %>% count(mean_clayTotal)
# Calculate the average individualCount per sandTotal
avgIndividualCountClayTotal = combineddata13 %>%
  group_by(mean_clayTotal) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountClayTotal, aes(x = mean_clayTotal, y = avgCount)) +
  geom_point() +
  xlab("Clay Total") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per Clay Total")

#PLOTS FOR carbonate clay total
plot(combineddata13$mean_carbonateClay,combineddata13$individualCount)
combineddata13 %>% count(mean_carbonateClay)
# Calculate the average individualCount per carbonateClay
avgIndividualCountCarbonateClay = combineddata13 %>%
  group_by(mean_carbonateClay) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountCarbonateClay, aes(x = mean_carbonateClay, y = avgCount)) +
  geom_point() +
  xlab("Carbonate Clay") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per Carbonate Clay")

#PLOTS FOR fine clay #NO VLAUES, CAN REMOVE
plot(combineddata13$mean_clayFineContent,combineddata13$individualCount)
combineddata13 %>% count(mean_clayFineContent)
# Calculate the average individualCount per clayFineContent
avgIndividualCountFineClay = combineddata13 %>%
  group_by(mean_clayFineContent) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountFineClay, aes(x = mean_clayFineContent, y = avgCount)) +
  geom_point() +
  xlab("Fine Clay") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per Fine Clay")

#PLOTS FOR fine silt
plot(combineddata13$mean_siltFineContent,combineddata13$individualCount)
combineddata13 %>% count(mean_siltFineContent)
# Calculate the average individualCount per clayFineContent
avgIndividualCountFineSilt = combineddata13 %>%
  group_by(mean_siltFineContent) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountFineSilt, aes(x = mean_siltFineContent, y = avgCount)) +
  geom_point() +
  xlab("Fine Silt") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per Fine Silt")

#PLOTS FOR coarse silt
plot(combineddata13$mean_siltCoarseContent,combineddata13$individualCount)
combineddata13 %>% count(mean_siltCoarseContent)
# Calculate the average individualCount per clayFineContent
avgIndividualCountCoarseSilt = combineddata13 %>%
  group_by(mean_siltCoarseContent) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountCoarseSilt, aes(x = mean_siltCoarseContent, y = avgCount)) +
  geom_point() +
  xlab("Coarse Silt") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per Coarse Silt")

#PLOTS FOR sandVeryFine
plot(combineddata13$mean_sandVeryFineContent,combineddata13$individualCount)
combineddata13 %>% count(mean_sandVeryFineContent)
# Calculate the average individualCount per sandVeryFineContent
avgIndividualCountVeryFineSand = combineddata13 %>%
  group_by(mean_sandVeryFineContent) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountVeryFineSand, aes(x = mean_sandVeryFineContent, y = avgCount)) +
  geom_point() +
  xlab("Very Fine Sand") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per Very Fine Sand")

#PLOTS FOR sandFine
plot(combineddata13$mean_sandFineContent,combineddata13$individualCount)
combineddata13 %>% count(mean_sandFineContent)
# Calculate the average individualCount per sandFineContent
avgIndividualCountFineSand = combineddata13 %>%
  group_by(mean_sandFineContent) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountFineSand, aes(x = mean_sandFineContent, y = avgCount)) +
  geom_point() +
  xlab("Fine Sand") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per Fine Sand")

#PLOTS FOR sandMedium
plot(combineddata13$mean_sandMediumContent,combineddata13$individualCount)
combineddata13 %>% count(mean_sandMediumContent)
# Calculate the average individualCount per sandVeryFineContent
avgIndividualCountMediumSand = combineddata13 %>%
  group_by(mean_sandMediumContent) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountMediumSand, aes(x = mean_sandMediumContent, y = avgCount)) +
  geom_point() +
  xlab("Medium Sand") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per Medium Sand")

#PLOTS FOR sandCoarse
plot(combineddata13$mean_sandCoarseContent,combineddata13$individualCount)
combineddata13 %>% count(mean_sandCoarseContent)
# Calculate the average individualCount per sandCoarseContent
avgIndividualCountCoarseSand = combineddata13 %>%
  group_by(mean_sandCoarseContent) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountCoarseSand, aes(x = mean_sandCoarseContent, y = avgCount)) +
  geom_point() +
  xlab("Coarse Sand") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per Coarse Sand")

#PLOTS FOR sandVeryCoarse
plot(combineddata13$mean_sandVeryCoarseContent,combineddata13$individualCount)
combineddata13 %>% count(mean_sandVeryCoarseContent)
# Calculate the average individualCount per sandCoarseContent
avgIndividualCountVeryCoarseSand = combineddata13 %>%
  group_by(mean_sandVeryCoarseContent) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountVeryCoarseSand, aes(x = mean_sandVeryCoarseContent, y = avgCount)) +
  geom_point() +
  xlab("Very Coarse Sand") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per Very Coarse Sand")

#PLOTS FOR carbon Total
plot(combineddata13$mean_carbonTot,combineddata13$individualCount)
combineddata13 %>% count(mean_carbonTot)
# Calculate the average individualCount per sandCoarseContent
avgIndividualCountcarbonTotal = combineddata13 %>%
  group_by(mean_carbonTot) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountcarbonTotal, aes(x = mean_carbonTot, y = avgCount)) +
  geom_point() +
  xlab("Carbon Total") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per Carbon Total")

#PLOTS FOR nitrogen Total
plot(combineddata13$mean_nitrogenTot,combineddata13$individualCount)
combineddata13 %>% count(mean_nitrogenTot)
# Calculate the average individualCount per sandCoarseContent
avgIndividualCountnitrogenTotal = combineddata13 %>%
  group_by(mean_nitrogenTot) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountnitrogenTotal, aes(x = mean_nitrogenTot, y = avgCount)) +
  geom_point() +
  xlab("Nitrogen Total") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per Nitrogen Total")

#PLOTS FOR sulfur Total
plot(combineddata13$mean_sulfurTot,combineddata13$individualCount)
combineddata13 %>% count(mean_sulfurTot)
# Calculate the average individualCount per sandCoarseContent
avgIndividualCountsulfurTotal = combineddata13 %>%
  group_by(mean_sulfurTot) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountsulfurTotal, aes(x = mean_sulfurTot, y = avgCount)) +
  geom_point() +
  xlab("Sulfur Total") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per Sulfur Total")

#PLOTS FOR est OC
plot(combineddata13$mean_estimatedOC,combineddata13$individualCount)
combineddata13 %>% count(mean_estimatedOC)
# Calculate the average individualCount per sandCoarseContent
avgIndividualCountestimatedOC = combineddata13 %>%
  group_by(mean_estimatedOC) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountestimatedOC, aes(x = mean_estimatedOC, y = avgCount)) +
  geom_point() +
  xlab("Estimated OC") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per Estimated OC")

#PLOTS FOR alMjelm
plot(combineddata13$mean_alMjelm,combineddata13$individualCount)
combineddata13 %>% count(mean_alMjelm)
# Calculate the average individualCount per alMjelm
avgIndividualCountalMjelm = combineddata13 %>%
  group_by(mean_alMjelm) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountalMjelm, aes(x = mean_alMjelm, y = avgCount)) +
  geom_point() +
  xlab("alMjelm") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per alMjelm")

#PLOTS FOR caMjelm
plot(combineddata13$mean_caMjelm,combineddata13$individualCount)
combineddata13 %>% count(mean_caMjelm)
# Calculate the average individualCount per alMjelm
avgIndividualCountcaMjelm = combineddata13 %>%
  group_by(mean_caMjelm) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountcaMjelm, aes(x = mean_caMjelm, y = avgCount)) +
  geom_point() +
  xlab("caMjelm") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per caMjelm")

#PLOTS FOR feMjelm
plot(combineddata13$mean_feMjelm,combineddata13$individualCount)
combineddata13 %>% count(mean_feMjelm)
# Calculate the average individualCount per alMjelm
avgIndividualCountfeMjelm = combineddata13 %>%
  group_by(mean_feMjelm) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountfeMjelm, aes(x = mean_feMjelm, y = avgCount)) +
  geom_point() +
  xlab("feMjelm") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per feMjelm")

#PLOTS FOR kMjelm
plot(combineddata13$mean_kMjelm,combineddata13$individualCount)
combineddata13 %>% count(mean_kMjelm)
# Calculate the average individualCount per alMjelm
avgIndividualCountkMjelm = combineddata13 %>%
  group_by(mean_kMjelm) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountkMjelm, aes(x = mean_kMjelm, y = avgCount)) +
  geom_point() +
  xlab("kMjelm") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per kMjelm")

#PLOTS FOR mgMjelm
plot(combineddata13$mean_mgMjelm,combineddata13$individualCount)
combineddata13 %>% count(mean_mgMjelm)
# Calculate the average individualCount per alMjelm
avgIndividualCountmgMjelm = combineddata13 %>%
  group_by(mean_mgMjelm) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountmgMjelm, aes(x = mean_mgMjelm, y = avgCount)) +
  geom_point() +
  xlab("mgMjelm") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per mgMjelm")

#PLOTS FOR mnMjelm
plot(combineddata13$mean_mnMjelm,combineddata13$individualCount)
combineddata13 %>% count(mean_mnMjelm)
# Calculate the average individualCount per alMjelm
avgIndividualCountmnMjelm = combineddata13 %>%
  group_by(mean_mnMjelm) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountmnMjelm, aes(x = mean_mnMjelm, y = avgCount)) +
  geom_point() +
  xlab("mnMjelm") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per mnMjelm")

#PLOTS FOR naMjelm
plot(combineddata13$mean_naMjelm,combineddata13$individualCount)
combineddata13 %>% count(mean_naMjelm)
# Calculate the average individualCount per alMjelm
avgIndividualCountnaMjelm = combineddata13 %>%
  group_by(mean_naMjelm) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountnaMjelm, aes(x = mean_naMjelm, y = avgCount)) +
  geom_point() +
  xlab("naMjelm") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per naMjelm")

#PLOTS FOR pMjelm
plot(combineddata13$mean_pMjelm,combineddata13$individualCount)
combineddata13 %>% count(mean_pMjelm)
# Calculate the average individualCount per alMjelm
avgIndividualCountpMjelm = combineddata13 %>%
  group_by(mean_pMjelm) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountpMjelm, aes(x = mean_pMjelm, y = avgCount)) +
  geom_point() +
  xlab("pMjelm") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per pMjelm")

#PLOTS FOR siMjelm
plot(combineddata13$mean_siMjelm,combineddata13$individualCount)
combineddata13 %>% count(mean_siMjelm)
# Calculate the average individualCount per alMjelm
avgIndividualCountsiMjelm = combineddata13 %>%
  group_by(mean_siMjelm) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountsiMjelm, aes(x = mean_siMjelm, y = avgCount)) +
  geom_point() +
  xlab("siMjelm") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per siMjelm")

#PLOTS FOR srMjelm
plot(combineddata13$mean_srMjelm,combineddata13$individualCount)
combineddata13 %>% count(mean_srMjelm)
# Calculate the average individualCount per alMjelm
avgIndividualCountsrMjelm = combineddata13 %>%
  group_by(mean_srMjelm) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountsrMjelm, aes(x = mean_srMjelm, y = avgCount)) +
  geom_point() +
  xlab("srMjelm") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per srMjelm")

#PLOTS FOR tiMjelm
plot(combineddata13$mean_tiMjelm,combineddata13$individualCount)
combineddata13 %>% count(mean_tiMjelm)
# Calculate the average individualCount per tiMjelm
avgIndividualCounttiMjelm = combineddata13 %>%
  group_by(mean_tiMjelm) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCounttiMjelm, aes(x = mean_tiMjelm, y = avgCount)) +
  geom_point() +
  xlab("tiMjelm") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per tiMjelm")

#PLOTS FOR zrMjelm
plot(combineddata13$mean_zrMjelm,combineddata13$individualCount)
combineddata13 %>% count(mean_zrMjelm)
# Calculate the average individualCount per zrMjelm
avgIndividualCountzrMjelm = combineddata13 %>%
  group_by(mean_zrMjelm) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountzrMjelm, aes(x = mean_zrMjelm, y = avgCount)) +
  geom_point() +
  xlab("zrMjelm") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per zrMjelm")

#PLOTS FOR phCacl2
plot(combineddata13$mean_phCacl2,combineddata13$individualCount)
combineddata13 %>% count(mean_phCacl2)
# Calculate the average individualCount per zrMjelm
avgIndividualCountphCacl2 = combineddata13 %>%
  group_by(mean_phCacl2) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountphCacl2, aes(x = mean_phCacl2, y = avgCount)) +
  geom_point() +
  xlab("phCacl2") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per phCacl2")

#PLOTS FOR phH2o
plot(combineddata13$mean_phH2o,combineddata13$individualCount)
combineddata13 %>% count(mean_phH2o)
# Calculate the average individualCount per phH2o
avgIndividualCountphH2o = combineddata13 %>%
  group_by(mean_phH2o) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountphH2o, aes(x = mean_phH2o, y = avgCount)) +
  geom_point() +
  xlab("phH2o") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per phH2o")

#PLOTS FOR ec12pre
plot(combineddata13$mean_ec12pre,combineddata13$individualCount)
combineddata13 %>% count(mean_ec12pre)
# Calculate the average individualCount per ec12pre
avgIndividualCountec12pre = combineddata13 %>%
  group_by(mean_ec12pre) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountec12pre, aes(x = mean_ec12pre, y = avgCount)) +
  geom_point() +
  xlab("ec12pre") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per ec12pre")

#PLOTS FOR gypsumConc
plot(combineddata13$mean_gypsumConc,combineddata13$individualCount)
combineddata13 %>% count(mean_gypsumConc)
# Calculate the average individualCount per gypsumConc
avgIndividualCountgypsumConc = combineddata13 %>%
  group_by(mean_gypsumConc) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountgypsumConc, aes(x = mean_gypsumConc, y = avgCount)) +
  geom_point() +
  xlab("gypsumConc") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per gypsumConc")

#PLOTS FOR caco3Conc
plot(combineddata13$mean_caco3Conc,combineddata13$individualCount)
combineddata13 %>% count(mean_caco3Conc)
# Calculate the average individualCount per caco3Conc
avgIndividualCountcaco3Conc = combineddata13 %>%
  group_by(mean_caco3Conc) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountcaco3Conc, aes(x = mean_caco3Conc, y = avgCount)) +
  geom_point() +
  xlab("caco3Conc") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per caco3Conc")

#PLOTS FOR caNh4d
plot(combineddata13$mean_caNh4d,combineddata13$individualCount)
combineddata13 %>% count(mean_caNh4d)
# Calculate the average individualCount per caNh4d
avgIndividualCountcaNh4d = combineddata13 %>%
  group_by(mean_caNh4d) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountcaNh4d, aes(x = mean_caNh4d, y = avgCount)) +
  geom_point() +
  xlab("caNh4d") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per caNh4d")

#PLOTS FOR kNh4d
plot(combineddata13$mean_kNh4d,combineddata13$individualCount)
combineddata13 %>% count(mean_kNh4d)
# Calculate the average individualCount per kNh4d
avgIndividualCountkNh4d = combineddata13 %>%
  group_by(mean_kNh4d) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountkNh4d, aes(x = mean_kNh4d, y = avgCount)) +
  geom_point() +
  xlab("kNh4d") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per kNh4d")

#PLOTS FOR mgNh4d
plot(combineddata13$mean_mgNh4d,combineddata13$individualCount)
combineddata13 %>% count(mean_mgNh4d)
# Calculate the average individualCount per mgNh4d
avgIndividualCountmgNh4d = combineddata13 %>%
  group_by(mean_mgNh4d) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountmgNh4d, aes(x = mean_mgNh4d, y = avgCount)) +
  geom_point() +
  xlab("mgNh4d") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per mgNh4d")

#PLOTS FOR naNh4d
plot(combineddata13$mean_naNh4d,combineddata13$individualCount)
combineddata13 %>% count(mean_naNh4d)
# Calculate the average individualCount per naNh4d
avgIndividualCountnaNh4d = combineddata13 %>%
  group_by(mean_naNh4d) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountnaNh4d, aes(x = mean_naNh4d, y = avgCount)) +
  geom_point() +
  xlab("naNh4d") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per naNh4d")

#PLOTS FOR cecdNh4
plot(combineddata13$mean_cecdNh4,combineddata13$individualCount)
combineddata13 %>% count(mean_cecdNh4)
# Calculate the average individualCount per cecdNh4
avgIndividualCountcecdNh4 = combineddata13 %>%
  group_by(mean_cecdNh4) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountcecdNh4, aes(x = mean_cecdNh4, y = avgCount)) +
  geom_point() +
  xlab("cecdNh4") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per cecdNh4")

#PLOTS FOR alSatCecd33
plot(combineddata13$mean_alSatCecd33,combineddata13$individualCount)
combineddata13 %>% count(mean_alSatCecd33)
# Calculate the average individualCount per alSatCecd33
avgIndividualCountalSatCecd33 = combineddata13 %>%
  group_by(mean_alSatCecd33) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountalSatCecd33, aes(x = mean_alSatCecd33, y = avgCount)) +
  geom_point() +
  xlab("alSatCecd33") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per alSatCecd33")

#PLOTS FOR baseSumCecd10
plot(combineddata13$mean_baseSumCecd10,combineddata13$individualCount)
combineddata13 %>% count(mean_baseSumCecd10)
# Calculate the average individualCount per baseSumCecd10
avgIndividualCountbaseSumCecd10 = combineddata13 %>%
  group_by(mean_baseSumCecd10) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountbaseSumCecd10, aes(x = mean_baseSumCecd10, y = avgCount)) +
  geom_point() +
  xlab("baseSumCecd10") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per baseSumCecd10")

#PLOTS FOR bsesatCecd10
plot(combineddata13$mean_bsesatCecd10,combineddata13$individualCount)
combineddata13 %>% count(mean_bsesatCecd10)
# Calculate the average individualCount per bsesatCecd10
avgIndividualCountbsesatCecd10 = combineddata13 %>%
  group_by(mean_bsesatCecd10) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountbsesatCecd10, aes(x = mean_bsesatCecd10, y = avgCount)) +
  geom_point() +
  xlab("bsesatCecd10") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per bsesatCecd10")

#PLOTS FOR ececCecd33
plot(combineddata13$mean_ececCecd33,combineddata13$individualCount)
combineddata13 %>% count(mean_ececCecd33)
# Calculate the average individualCount per ececCecd33
avgIndividualCountececCecd33 = combineddata13 %>%
  group_by(mean_ececCecd33) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountececCecd33, aes(x = mean_ececCecd33, y = avgCount)) +
  geom_point() +
  xlab("ececCecd33") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per ececCecd33")

#PLOTS FOR alKcl
plot(combineddata13$mean_alKcl,combineddata13$individualCount)
combineddata13 %>% count(mean_alKcl)
# Calculate the average individualCount per alKcl
avgIndividualCountalKcl = combineddata13 %>%
  group_by(mean_alKcl) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountalKcl, aes(x = mean_alKcl, y = avgCount)) +
  geom_point() +
  xlab("alKcl") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per alKcl")

#PLOTS FOR feKcl, #CAN REMOVE, ONLY EMPTY VALUES
plot(combineddata13$mean_feKcl,combineddata13$individualCount)
combineddata13 %>% count(mean_feKcl)
# Calculate the average individualCount per feKcl
avgIndividualCountfeKcl = combineddata13 %>%
  group_by(mean_feKcl) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountfeKcl, aes(x = mean_feKcl, y = avgCount)) +
  geom_point() +
  xlab("feKcl") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per feKcl")

#PLOTS FOR mnKcl
plot(combineddata13$mean_mnKcl,combineddata13$individualCount)
combineddata13 %>% count(mean_mnKcl)
# Calculate the average individualCount per mnKcl
avgIndividualCountmnKcl = combineddata13 %>%
  group_by(mean_mnKcl) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountmnKcl, aes(x = mean_mnKcl, y = avgCount)) +
  geom_point() +
  xlab("mnKcl") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per mnKcl")

#PLOTS FOR bSatx, CAN REMOVE ONLY EMPTY VALUES
plot(combineddata13$mean_bSatx,combineddata13$individualCount)
combineddata13 %>% count(mean_bSatx)
# Calculate the average individualCount per bSatx
avgIndividualCountbSatx = combineddata13 %>%
  group_by(mean_bSatx) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountbSatx, aes(x = mean_bSatx, y = avgCount)) +
  geom_point() +
  xlab("bSatx") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per bSatx")

#PLOTS FOR brSatx
plot(combineddata13$mean_brSatx,combineddata13$individualCount)
combineddata13 %>% count(mean_brSatx)
# Calculate the average individualCount per brSatx
avgIndividualCountbrSatx = combineddata13 %>%
  group_by(mean_brSatx) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountbrSatx, aes(x = mean_brSatx, y = avgCount)) +
  geom_point() +
  xlab("brSatx") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per brSatx")

#PLOTS FOR caSatx
plot(combineddata13$mean_caSatx,combineddata13$individualCount)
combineddata13 %>% count(mean_caSatx)
# Calculate the average individualCount per caSatx
avgIndividualCountcaSatx = combineddata13 %>%
  group_by(mean_caSatx) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountcaSatx, aes(x = mean_caSatx, y = avgCount)) +
  geom_point() +
  xlab("caSatx") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per caSatx")

#PLOTS FOR clSatx
plot(combineddata13$mean_clSatx,combineddata13$individualCount)
combineddata13 %>% count(mean_clSatx)
# Calculate the average individualCount per clSatx
avgIndividualCountclSatx = combineddata13 %>%
  group_by(mean_clSatx) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountclSatx, aes(x = mean_clSatx, y = avgCount)) +
  geom_point() +
  xlab("clSatx") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per clSatx")

#PLOTS FOR co3Satx, ONLY ONE VALUE FOR OVER 7k entries
plot(combineddata13$mean_co3Satx,combineddata13$individualCount)
combineddata13 %>% count(mean_co3Satx)
# Calculate the average individualCount per co3Satx
avgIndividualCountco3Satx = combineddata13 %>%
  group_by(mean_co3Satx) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountco3Satx, aes(x = mean_co3Satx, y = avgCount)) +
  geom_point() +
  xlab("co3Satx") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per co3Satx")

#PLOTS FOR ecSatp
plot(combineddata13$mean_ecSatp,combineddata13$individualCount)
combineddata13 %>% count(mean_ecSatp)
# Calculate the average individualCount per ecSatp
avgIndividualCountecSatp = combineddata13 %>%
  group_by(mean_ecSatp) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountecSatp, aes(x = mean_ecSatp, y = avgCount)) +
  geom_point() +
  xlab("ecSatp") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per ecSatp")

#PLOTS FOR flSatx
plot(combineddata13$mean_flSatx,combineddata13$individualCount)
combineddata13 %>% count(mean_flSatx)
# Calculate the average individualCount per flSatx
avgIndividualCountflSatx = combineddata13 %>%
  group_by(mean_flSatx) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountflSatx, aes(x = mean_flSatx, y = avgCount)) +
  geom_point() +
  xlab("flSatx") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per flSatx")

#PLOTS FOR waterSatx
plot(combineddata13$mean_waterSatx,combineddata13$individualCount)
combineddata13 %>% count(mean_waterSatx)
# Calculate the average individualCount per waterSatx
avgIndividualCountwaterSatx = combineddata13 %>%
  group_by(mean_waterSatx) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountwaterSatx, aes(x = mean_waterSatx, y = avgCount)) +
  geom_point() +
  xlab("waterSatx") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per waterSatx")

#PLOTS FOR hco3Sx
plot(combineddata13$mean_hco3Sx,combineddata13$individualCount)
combineddata13 %>% count(mean_hco3Sx)
# Calculate the average individualCount per hco3Sx
avgIndividualCounthco3Sx = combineddata13 %>%
  group_by(mean_hco3Sx) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCounthco3Sx, aes(x = mean_hco3Sx, y = avgCount)) +
  geom_point() +
  xlab("hco3Sx") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per hco3Sx")

#PLOTS FOR kSatx
plot(combineddata13$mean_kSatx,combineddata13$individualCount)
combineddata13 %>% count(mean_kSatx)
# Calculate the average individualCount per kSatx
avgIndividualCountkSatx = combineddata13 %>%
  group_by(mean_kSatx) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountkSatx, aes(x = mean_kSatx, y = avgCount)) +
  geom_point() +
  xlab("kSatx") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per kSatx")

#PLOTS FOR mgSatx
plot(combineddata13$mean_mgSatx,combineddata13$individualCount)
combineddata13 %>% count(mean_mgSatx)
# Calculate the average individualCount per mgSatx
avgIndividualCountmgSatx = combineddata13 %>%
  group_by(mean_mgSatx) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountmgSatx, aes(x = mean_mgSatx, y = avgCount)) +
  geom_point() +
  xlab("mgSatx") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per mgSatx")

#PLOTS FOR naSatx
plot(combineddata13$mean_naSatx,combineddata13$individualCount)
combineddata13 %>% count(mean_naSatx)
# Calculate the average individualCount per naSatx
avgIndividualCountnaSatx = combineddata13 %>%
  group_by(mean_naSatx) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountnaSatx, aes(x = mean_naSatx, y = avgCount)) +
  geom_point() +
  xlab("naSatx") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per naSatx")

#PLOTS FOR no2Satx
plot(combineddata13$mean_no2Satx,combineddata13$individualCount)
combineddata13 %>% count(mean_no2Satx)
# Calculate the average individualCount per no2Satx
avgIndividualCountno2Satx = combineddata13 %>%
  group_by(mean_no2Satx) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountno2Satx, aes(x = mean_no2Satx, y = avgCount)) +
  geom_point() +
  xlab("no2Satx") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per no2Satx")

#PLOTS FOR no3Satx
plot(combineddata13$mean_no3Satx,combineddata13$individualCount)
combineddata13 %>% count(mean_no3Satx)
# Calculate the average individualCount per no3Satx
avgIndividualCountno3Satx = combineddata13 %>%
  group_by(mean_no3Satx) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountno3Satx, aes(x = mean_no3Satx, y = avgCount)) +
  geom_point() +
  xlab("no3Satx") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per no3Satx")

#PLOTS FOR pSatx
plot(combineddata13$mean_pSatx,combineddata13$individualCount)
combineddata13 %>% count(mean_pSatx)
# Calculate the average individualCount per pSatx
avgIndividualCountpSatx = combineddata13 %>%
  group_by(mean_pSatx) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountpSatx, aes(x = mean_pSatx, y = avgCount)) +
  geom_point() +
  xlab("pSatx") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per pSatx")

#PLOTS FOR phSp, MIGHT BE A GOOD ONE FOR A POSITIVE RELATIONSHIP
plot(combineddata13$mean_phSp,combineddata13$individualCount)
combineddata13 %>% count(mean_phSp)
# Calculate the average individualCount per phSp
avgIndividualCountphSp = combineddata13 %>%
  group_by(mean_phSp) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountphSp, aes(x = mean_phSp, y = avgCount)) +
  geom_point() +
  xlab("phSp") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per phSp")

#PLOTS FOR resist, CAN REMOVE, ONLY NA
plot(combineddata13$mean_resist,combineddata13$individualCount)
combineddata13 %>% count(mean_resist)
# Calculate the average individualCount per resist
avgIndividualCountresist = combineddata13 %>%
  group_by(mean_resist) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountresist, aes(x = mean_resist, y = avgCount)) +
  geom_point() +
  xlab("resist") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per resist")

#PLOTS FOR so4Satx
plot(combineddata13$mean_so4Satx,combineddata13$individualCount)
combineddata13 %>% count(mean_so4Satx)
# Calculate the average individualCount per so4Satx
avgIndividualCountso4Satx = combineddata13 %>%
  group_by(mean_so4Satx) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountso4Satx, aes(x = mean_so4Satx, y = avgCount)) +
  geom_point() +
  xlab("so4Satx") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per so4Satx")

#PLOTS FOR diameter
plot(combineddata13$mean_diameter,combineddata13$individualCount)
combineddata13 %>% count(mean_diameter)
# Calculate the average individualCount per diameter
avgIndividualCountdiameter = combineddata13 %>%
  group_by(mean_diameter) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountdiameter, aes(x = mean_diameter, y = avgCount)) +
  geom_point() +
  xlab("diameter") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per diameter")

#PLOTS FOR ninetyDiameter
plot(combineddata13$mean_ninetyDiameter,combineddata13$individualCount)
combineddata13 %>% count(mean_ninetyDiameter)
# Calculate the average individualCount per ninetyDiameter
avgIndividualCountninetyDiameter = combineddata13 %>%
  group_by(mean_ninetyDiameter) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountninetyDiameter, aes(x = mean_ninetyDiameter, y = avgCount)) +
  geom_point() +
  xlab("Ninety Diameter") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per ninety Diameter")

#PLOTS FOR maxDiskHeight, MIGHT BE GOOD FOR POSITIVE ASSOCIATION
plot(combineddata13$mean_maxDiskHeight,combineddata13$individualCount)
combineddata13 %>% count(mean_maxDiskHeight)
# Calculate the average individualCount per maxDiskHeight
avgIndividualCountmaxDiskHeight = combineddata13 %>%
  group_by(mean_maxDiskHeight) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountmaxDiskHeight, aes(x = mean_maxDiskHeight, y = avgCount)) +
  geom_point() +
  xlab("Max Disk Height") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per max Disk Height")

#PLOTS FOR minDiskHeight
plot(combineddata13$mean_minDiskHeight,combineddata13$individualCount)
combineddata13 %>% count(mean_minDiskHeight)
# Calculate the average individualCount per minDiskHeight
avgIndividualCountminDiskHeight = combineddata13 %>%
  group_by(mean_minDiskHeight) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountminDiskHeight, aes(x = mean_minDiskHeight, y = avgCount)) +
  geom_point() +
  xlab("Min Disk Height") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per min Disk Height")

#PLOTS FOR aDiskHeight
plot(combineddata13$mean_aDiskHieght,combineddata13$individualCount)
combineddata13 %>% count(mean_aDiskHieght)
# Calculate the average individualCount per aDiskHieght
avgIndividualCountaDiskHieght = combineddata13 %>%
  group_by(mean_aDiskHieght) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountaDiskHieght, aes(x = mean_aDiskHieght, y = avgCount)) +
  geom_point() +
  xlab("a Disk Hieght") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per a Disk Hieght")

#PLOTS FOR bDiskHeight
plot(combineddata13$mean_bDiskHeight,combineddata13$individualCount)
combineddata13 %>% count(mean_bDiskHeight)
# Calculate the average individualCount per bDiskHeight
avgIndividualCountbDiskHeight = combineddata13 %>%
  group_by(mean_bDiskHeight) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountbDiskHeight, aes(x = mean_bDiskHeight, y = avgCount)) +
  geom_point() +
  xlab("b Disk Height") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per b Disk Height")

#PLOTS FOR bulkDensDisk, POSSIBLE NON LINEAR RELATIONSHIP
plot(combineddata13$mean_bulkDensDisk,combineddata13$individualCount)
combineddata13 %>% count(mean_bulkDensDisk)
# Calculate the average individualCount per bulkDensDisk
avgIndividualCountbulkDensDisk = combineddata13 %>%
  group_by(mean_bulkDensDisk) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountbulkDensDisk, aes(x = mean_bulkDensDisk, y = avgCount)) +
  geom_point() +
  xlab("bulk Density Disk") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per bulk Density Disk")

#PLOTS FOR slopeAspect
plot(combineddata13$mean_slopeAspect,combineddata13$individualCount)
combineddata13 %>% count(mean_slopeAspect)
# Calculate the average individualCount per slopeAspect
avgIndividualCountslopeAspect = combineddata13 %>%
  group_by(mean_slopeAspect) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountslopeAspect, aes(x = mean_slopeAspect, y = avgCount)) +
  geom_point() +
  xlab("slope Aspect") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per slope Aspect")

#PLOTS FOR slopeGradient
plot(combineddata13$mean_slopeGradient,combineddata13$individualCount)
combineddata13 %>% count(mean_slopeGradient)
# Calculate the average individualCount per slopeGradient
avgIndividualCountslopeGradient = combineddata13 %>%
  group_by(mean_slopeGradient) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountslopeGradient, aes(x = mean_slopeGradient, y = avgCount)) +
  geom_point() +
  xlab("slope Gradient") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per slope Gradient")

#PLOTS FOR understoryHeight
plot(combineddata13$mean_understoryHeight,combineddata13$individualCount)
combineddata13 %>% count(mean_understoryHeight)
# Calculate the average individualCount per understoryHeight
avgIndividualCountunderstoryHeight = combineddata13 %>%
  group_by(mean_understoryHeight) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountunderstoryHeight, aes(x = mean_understoryHeight, y = avgCount)) +
  geom_point() +
  xlab("understory Height") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per understory Height")

#PLOTS FOR overstoryHeight
plot(combineddata13$mean_overstoryHeight,combineddata13$individualCount)
combineddata13 %>% count(mean_overstoryHeight)
# Calculate the average individualCount per overstoryHeight
avgIndividualCountoverstoryHeight = combineddata13 %>%
  group_by(mean_overstoryHeight) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountoverstoryHeight, aes(x = mean_overstoryHeight, y = avgCount)) +
  geom_point() +
  xlab("overstory Height") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per overstory Height")

#PLOTS FOR d15N, CAN REMOVE, ONLY NA
plot(combineddata13$mean_d15N,combineddata13$individualCount)
combineddata13 %>% count(mean_d15N)
# Calculate the average individualCount per d15N
avgIndividualCountd15N = combineddata13 %>%
  group_by(mean_d15N) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountd15N, aes(x = mean_d15N, y = avgCount)) +
  geom_point() +
  xlab("d15N") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per d15N")

#PLOTS FOR d13C, CAN REMOVE, ONLY NA
plot(combineddata13$mean_d13C,combineddata13$individualCount)
combineddata13 %>% count(mean_d13C)
# Calculate the average individualCount per d13C
avgIndividualCountd13C = combineddata13 %>%
  group_by(mean_d13C) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountd13C, aes(x = mean_d13C, y = avgCount)) +
  geom_point() +
  xlab("d13C") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per d13C")

#PLOTS FOR nitrogenPercent, CAN REMOVE, ONLY NA
plot(combineddata13$mean_nitrogenPercent,combineddata13$individualCount)
combineddata13 %>% count(mean_nitrogenPercent)
# Calculate the average individualCount per nitrogenPercent
avgIndividualCountnitrogenPercent = combineddata13 %>%
  group_by(mean_nitrogenPercent) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountnitrogenPercent, aes(x = mean_nitrogenPercent, y = avgCount)) +
  geom_point() +
  xlab("nitrogen Percent") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per nitrogen Percent")

#PLOTS FOR carbonPercent, CAN REMOVE
plot(combineddata13$mean_carbonPercent,combineddata13$individualCount)
combineddata13 %>% count(mean_carbonPercent)
# Calculate the average individualCount per carbonPercent
avgIndividualCountcarbonPercent = combineddata13 %>%
  group_by(mean_carbonPercent) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountcarbonPercent, aes(x = mean_carbonPercent, y = avgCount)) +
  geom_point() +
  xlab("carbon Percent") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per carbon Percent")

#PLOTS FOR ligninPercent, CAN REMOVE
plot(combineddata13$mean_ligninPercent,combineddata13$individualCount)
combineddata13 %>% count(mean_ligninPercent)
# Calculate the average individualCount per ligninPercent
avgIndividualCountligninPercent = combineddata13 %>%
  group_by(mean_ligninPercent) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountligninPercent, aes(x = mean_ligninPercent, y = avgCount)) +
  geom_point() +
  xlab("lignin Percent") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per lignin Percent")

#PLOTS FOR cellulosePercent, CAN REMOVE
plot(combineddata13$mean_cellulosePercent,combineddata13$individualCount)
combineddata13 %>% count(mean_cellulosePercent)
# Calculate the average individualCount per cellulosePercent
avgIndividualCountcellulosePercent = combineddata13 %>%
  group_by(mean_cellulosePercent) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountcellulosePercent, aes(x = mean_cellulosePercent, y = avgCount)) +
  geom_point() +
  xlab("cellulose Percent") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per cellulose Percent")

#PLOTS FOR NDVI
plot(combineddata13$NDVI,combineddata13$individualCount)
combineddata13 %>% count(NDVI)
# Calculate the average individualCount per NDVI
avgIndividualCountNDVI = combineddata13 %>%
  group_by(NDVI) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountNDVI, aes(x = NDVI, y = avgCount)) +
  geom_point() +
  xlab("NDVI") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per NDVI")

#PLOTS FOR countMammal
plot(combineddata13$countMammal,combineddata13$individualCount)
combineddata13 %>% count(countMammal)
# Calculate the average individualCount per NDVI
avgIndividualCountcountMammal = combineddata13 %>%
  group_by(countMammal) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountcountMammal, aes(x = countMammal, y = avgCount)) +
  geom_point() +
  xlab("count Mammal") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per count Mammal")

#PLOTS FOR countMammalSite
plot(combineddata13$countMammalSite,combineddata13$individualCount)
combineddata13 %>% count(countMammalSite)
# Calculate the average individualCount per NDVI
avgIndividualCountcountMammalSite = combineddata13 %>%
  group_by(countMammalSite) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountcountMammalSite, aes(x = countMammalSite, y = avgCount)) +
  geom_point() +
  xlab("count Mammal Site") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per count Mammal Site")

#PLOTS FOR countBird
plot(combineddata13$countBird,combineddata13$individualCount)
combineddata13 %>% count(countBird)
# Calculate the average individualCount per Bird
avgIndividualCountcountBird = combineddata13 %>%
  group_by(countBird) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountcountBird, aes(x = countBird, y = avgCount)) +
  geom_point() +
  xlab("count Bird") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per count Bird")

#PLOTS FOR countBirdSite
plot(combineddata13$countBirdSite,combineddata13$individualCount)
combineddata13 %>% count(countBirdSite)
# Calculate the average individualCount per BirdSite
avgIndividualCountcountBirdSite = combineddata13 %>%
  group_by(countBirdSite) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountcountBirdSite, aes(x = countBirdSite, y = avgCount)) +
  geom_point() +
  xlab("count Bird Site") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per count Bird Site")

#PLOTS FOR ovendry
plot(combineddata13$perbiogeo.mean_airDryOvenDry,combineddata13$individualCount)
combineddata13 %>% count(perbiogeo.mean_airDryOvenDry)
# Calculate the average individualCount per perbiogeo.mean_airDryOvenDry
avgIndividualCountperbiogeo.mean_airDryOvenDry = combineddata13 %>%
  group_by(perbiogeo.mean_airDryOvenDry) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountperbiogeo.mean_airDryOvenDry, aes(x = perbiogeo.mean_airDryOvenDry, y = avgCount)) +
  geom_point() +
  xlab("perbiogeo.mean_airDryOvenDry") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per perbiogeo.mean_airDryOvenDry")

#PLOTS FOR soilMoisture
plot(combineddata13$soilMoisture,combineddata13$individualCount)
combineddata13 %>% count(soilMoisture)
# Calculate the average individualCount per perbiogeo.mean_airDryOvenDry
avgIndividualCountsoilMoisture = combineddata13 %>%
  group_by(soilMoisture) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountsoilMoisture, aes(x = soilMoisture, y = avgCount)) +
  geom_point() +
  xlab("soilMoisture") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per soilMoisture")

#PLOTS FOR soilTemp
plot(combineddata13$soilTemp,combineddata13$individualCount)
combineddata13 %>% count(soilTemp)
# Calculate the average individualCount per perbiogeo.mean_airDryOvenDry
avgIndividualCountsoilTemp = combineddata13 %>%
  group_by(soilTemp) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountsoilTemp, aes(x = soilTemp, y = avgCount)) +
  geom_point() +
  xlab("soilTemp") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per soilTemp")

#PLOTS FOR litterDepth
plot(combineddata13$litterDepth,combineddata13$individualCount)
combineddata13 %>% count(litterDepth)
# Calculate the average individualCount per litterDepth
avgIndividualCountlitterDepth = combineddata13 %>%
  group_by(litterDepth) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountlitterDepth, aes(x = litterDepth, y = avgCount)) +
  geom_point() +
  xlab("litterDepth") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per litterDepth")

#PLOTS FOR soilInWaterpH
plot(combineddata13$soilInWaterpH,combineddata13$individualCount)
combineddata13 %>% count(soilInWaterpH)
# Calculate the average individualCount per soilInWaterpH
avgIndividualCountsoilInWaterpH = combineddata13 %>%
  group_by(soilInWaterpH) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountsoilInWaterpH, aes(x = soilInWaterpH, y = avgCount)) +
  geom_point() +
  xlab("soilInWaterpH") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per soilInWaterpH")

#PLOTS FOR soilInCaClpH
plot(combineddata13$soilInCaClpH,combineddata13$individualCount)
combineddata13 %>% count(soilInCaClpH)
# Calculate the average individualCount per soilInCaClpH
avgIndividualCountsoilInCaClpH = combineddata13 %>%
  group_by(soilInCaClpH) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountsoilInCaClpH, aes(x = soilInCaClpH, y = avgCount)) +
  geom_point() +
  xlab("soilInCaClpH") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per soilInCaClpH")

#PLOTS FOR d15N
plot(combineddata13$d15N,combineddata13$individualCount)
combineddata13 %>% count(d15N)
# Calculate the average individualCount per d15N
avgIndividualCountd15N = combineddata13 %>%
  group_by(d15N) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountd15N, aes(x = d15N, y = avgCount)) +
  geom_point() +
  xlab("d15N") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per d15N")

#PLOTS FOR organicd13C
plot(combineddata13$organicd13C,combineddata13$individualCount)
combineddata13 %>% count(organicd13C)
# Calculate the average individualCount per organicd13C
avgIndividualCountorganicd13C = combineddata13 %>%
  group_by(organicd13C) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountorganicd13C, aes(x = organicd13C, y = avgCount)) +
  geom_point() +
  xlab("organicd13C") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per organicd13C")

#PLOTS FOR nitrogenPercent
plot(combineddata13$nitrogenPercent,combineddata13$individualCount)
combineddata13 %>% count(nitrogenPercent)
# Calculate the average individualCount per nitrogenPercent
avgIndividualCountnitrogenPercent = combineddata13 %>%
  group_by(nitrogenPercent) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountnitrogenPercent, aes(x = nitrogenPercent, y = avgCount)) +
  geom_point() +
  xlab("nitrogenPercent") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per nitrogenPercent")

#PLOTS FOR organicCPercent
plot(combineddata13$organicCPercent,combineddata13$individualCount)
combineddata13 %>% count(organicCPercent)
# Calculate the average individualCount per organicCPercent
avgIndividualCountorganicCPercent = combineddata13 %>%
  group_by(organicCPercent) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountorganicCPercent, aes(x = organicCPercent, y = avgCount)) +
  geom_point() +
  xlab("organicCPercent") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per organicCPercent")

#PLOTS FOR CNratio
plot(combineddata13$CNratio,combineddata13$individualCount)
combineddata13 %>% count(CNratio)
# Calculate the average individualCount per CNratio
avgIndividualCountCNratio = combineddata13 %>%
  group_by(CNratio) %>%
  summarize(avgCount = mean(individualCount))
# Create a scatter plot
ggplot(avgIndividualCountCNratio, aes(x = CNratio, y = avgCount)) +
  geom_point() +
  xlab("CNratio") +
  ylab("Average Individual Count") +
  ggtitle("Average Individual Count per CNratio")
#*REMOVED VARIABLES####
combineddata9$mean_clayFineContent <- NULL
combineddata9$mean_gypsumConc <- NULL
combineddata9$mean_caco3Conc <- NULL
combineddata9$mean_feKcl <- NULL
combineddata9$mean_bSatx <- NULL
combineddata9$mean_brSatx <- NULL
combineddata9$mean_caSatx <- NULL
combineddata9$mean_clSatx <- NULL
combineddata9$mean_co3Satx <- NULL
combineddata9$mean_waterSatx <- NULL
combineddata9$mean_hco3Sx <- NULL
combineddata9$mean_kSatx <- NULL
combineddata9$mean_mgSatx <- NULL
combineddata9$mean_naSatx <- NULL
combineddata9$mean_no2Satx <- NULL
combineddata9$mean_no3Satx <- NULL
combineddata9$mean_pSatx <- NULL
combineddata9$mean_resist <- NULL
combineddata9$mean_so4Satx <- NULL
combineddata9$mean_aDiskHieght <- NULL
combineddata9$mean_bDiskHeight <- NULL
combineddata9$mean_d15N <- NULL
combineddata9$mean_d13C <- NULL
combineddata9$mean_nitrogenPercent <- NULL
combineddata9$mean_carbonPercent <- NULL
combineddata9$mean_ligninPercent <- NULL
combineddata9$mean_cellulosePercent <- NULL


