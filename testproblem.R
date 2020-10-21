library(here)
library(unmarked)
library(lubridate)
library(suncalc)


dat <- read.csv(here("CallbackSurvey_DataEntry.csv"))
head(dat)

sdat <- subset(dat, dat$AlphaCode %in% c("COMO","VIRA","LEBI")) #range of obs numbers, COMO=4, LEBI=46, VIRA=154
#write.csv(sdat, "TestData_COMO_VIRA_LEBI.csv")

#format times and dates in standard way, calculate time since sunrise

sdat$Date <- dmy(sdat$SurveyDate) 

sunrisetime <- getSunlightTimes(date=sdat$Date, keep=c("sunrise"), 39.385937, -76.371187, tz="CET")

names(sdat)
