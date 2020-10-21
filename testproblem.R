library(here)
library(unmarked)
library(lubridate)
library(suncalc)


sdat <- read.csv("CallbackSurvey_DataEntry.csv")

#get the FULL list of sites, not just the ones where birds occur


head(sdat)


#format times and dates in standard way, calculate time since sunrise
sdat$Date <- dmy(sdat$SurveyDate)
sdat$DateTime <- paste(sdat$Date,sdat$SurveyTime, sep = " ")
sdat$DateTime <- parse_date_time(sdat$DateTime, tz="EST", 'ymd %I:%M:%p')

#find and replace all x's w/ 1's
sdat[,17:30] <- sapply(sdat[,17:30], function(x) as.numeric(gsub("X", 1, x)))

#create new 'stacked' site which is a combo of site x visit number
sdat$SiteXVisit <- paste(sdat$PointID, sdat$VisitNum, sep=".")
sdat$SiteXVisit <- as.factor(sdat$SiteXVisit)
AllSites <- unique(sdat$SiteXVisit) #full list of all possible SiteXVisit possibilities

#work on sunrise time later
sunrisetime <- getSunlightTimes(date=sdat$Date, keep=c("sunrise"), 39.385937, -76.371187, tz="EST")
sdat$sunrisetime <- sunrisetime[,4] #just the date+time of sunrise


sdat$TSLSR <- difftime(sdat$DateTime, sdat$sunrisetime, tz="EST", units="min") #time since last sunrise, in minutes

#Julian Day
sdat$JulianDate <- format(sdat$Date, "%j")

#write a copy of the updated .csv for all birds
write.csv(sdat, file="FullTestData.csv")

#practice just with VIRA data
VIRA <- as.data.frame(subset(sdat, sdat$AlphaCode=="VIRA"))

write.csv(VIRA, file="VIRA.csv")
#select and format into an unmarked style set of data frames

#detection data
y <- VIRA[,c(17:30,34)] #all the one minute interval column counts
#aggregate counts by SitexVisit
y <- aggregate(y[,1:14], by=list(y$SiteXVisit), FUN=sum)
row.names(y) <- y$Group.1
y <- y[,-1]

#remove the NA columns
y <- subset(y, select=-c(SORA,AMBI))

#add in zeroes--sites where VIRA does NOT occur 
AllSites

Sites_MissVIRA <- AllSites[!(AllSites %in% VIRA$SiteXVisit)] #length is 660
zeroes <- matrix(0L, nrow = length(Sites_MissVIRA), ncol = dim(y)[2])#number of rows = length of Sites_MissVIRA, number of columns matches y matrix
colnames(zeroes) <- names(y)
row.names(zeroes) <- Sites_MissVIRA #this is what can be added to y
y <- rbind(y, zeroes) #full matrix with presence AND absence

## Note: have not yet figured out how to include distance bands properly--will probably have to run the models multiple ways, splitting across the different classes and looking for variation in detection probability ##

#site level covariates -- just used PointID here, but veg data would go here as well; also I think now that site and time are stacked, that the time variables need to be here as well
blgr.site <- VIRA[,c("SiteXVisit","PointID", "JulianDate","Observer","TSLSR")] 
blgr.site <- unique(blgr.site) #remove redundant rows

#now add in all the site information from sites where VIRA was NOT observed
temp <- sdat[(sdat$SiteXVisit %in% Sites_MissVIRA),] #which sites are not part of the VIRA positive set
sitecovs_VIRAzero <- sitecovs_VIRAzero[,c("SiteXVisit","PointID", "JulianDate", "Observer")] #just the columns of interest
sitecovs_VIRAzero <- unique(sitecovs_VIRAzero) #remove redundant rows
#some SiteXVisits are repeated because there are different TSLSR times nested within them, but everything else is the same; to address this, will aggregate and average TSLSR times within SiteXVisit combo
TSLSR <- aggregate(temp$TSLSR, by=list(temp$SiteXVisit), FUN=mean)

sitecovs_VIRAzero <- sitecovs_VIRAzero[order(sitecovs_VIRAzero$SiteXVisit),]#order alphabetically by sitexvisit

#manual check to ensure that these are in the same order
identical(sitecovs_VIRAzero$SiteXVisit, TSLSR$Group.1)

#combine presence and absence VIRA site data together
sitecovs_VIRAzero <- cbind(sitecovs_VIRAzero, TSLSR$x)
names(sitecovs_VIRAzero)[5] <- "TSLSR" 


blgr.site <- rbind(blgr.site,sitecovs_VIRAzero) #combined, full dataframe w sites missing VIRA too


#observation level covariates
blgr.obs <- NULL #? I think this is right, because all the time variables are now part of site

blgr <- unmarkedFrameOccu(y = y, siteCovs = blgr.site, obsCovs=NULL)
summary(blgr)

fm1<-occu(~1 ~1,blgr) #model w/ no covariates
#model<-occu(~detection_formula ~occupancy_formula, dataframe)
# When writing formula, detection covariates follow first tilde, then come abundance covariates

fm2 <- occu(~Observer ~PointID,blgr)

backTransform(fm1,'det') #back transform to get detection estimate
backTransform(fm1,"state") #back transform to get occupancy estimate
