##########################################################################################################################
###                                                                                                                    ###
###                                                                                                                    ###
###                              Police: Crimes, Outcomes and Stop and Search data                                     ###
###                                                                                                                    ###
###                                                                                                                    ###
##########################################################################################################################

## Created by: LA
## Creation date: 05/08/2026

## Modified on: 07/07/2026


## Load in the required libraries
library(tidyverse)



## Read in the data

## Street Level Data

temp = list.files(path="../Crime Data Download/Street",pattern="*.csv", full.names = TRUE)
myfiles = lapply(temp, read.csv)


crime_data_street_all<-data.frame()


for(i in 1:72){
  
  df<-myfiles[[i]]
  
  crime_data_street_all<-rbind(crime_data_street_all,df)
  
}



## Outcomes Data

temp = list.files(path="../Crime Data Download/Outcomes",pattern="*.csv", full.names = TRUE)
myfiles = lapply(temp, read.csv)


crime_data_outcomes_all<-data.frame()


for(i in 1:72){
  
  df<-myfiles[[i]]
  
  crime_data_outcomes_all<-rbind(crime_data_outcomes_all,df)
  
}



## Stop and Search Data

## Read in the data
temp = list.files(path="../Crime Data Download/Stop and Search",pattern="*.csv", full.names = TRUE)
myfiles = lapply(temp, read.csv)


crime_data_stopandsearch_all<-data.frame()


for(i in 1:72){
  
  df<-myfiles[[i]]
  
  crime_data_stopandsearch_all<-rbind(crime_data_stopandsearch_all,df)
  
}

## Clean up the Environment

rm(temp)
rm(i)
rm(df)
rm(myfiles)

## Use the LSOA names to filter the data to obtain the relevant areas. Some crimes have no location recorded so this
## data will be removed as we cannot attribute it to a location of interest.

## For Stonehouse: Plymouth 034A, Plymouth 034B, Plymouth 034C, Plymouth 034D, Plymouth 034E
## For Hastings: Hastings 009A, Hastings 009B, Hastings 009C, Hastings 009D

plymouth_lsoas<-c("Plymouth 034A", "Plymouth 034B", "Plymouth 034C", "Plymouth 034D", "Plymouth 034E")

hastings_lsoas<-c("Hastings 009A", "Hastings 009B", "Hastings 009C", "Hastings 009D")

crime_data_street_plymouth<-crime_data_street_all |> filter(LSOA.name %in% plymouth_lsoas)

crime_data_outcomes_plymouth<-crime_data_outcomes_all |> filter(LSOA.name %in% plymouth_lsoas)

## Use the latitude and longitudes from the subsets above to filter the stop and search data

max(crime_data_street_plymouth$Longitude) ## -4.144637
min(crime_data_street_plymouth$Longitude) ## -4.164925

max(crime_data_street_plymouth$Latitude) ## 50.37173
min(crime_data_street_plymouth$Latitude) ## 50.36098

## Check for any differences with the outcome data

max(crime_data_outcomes_plymouth$Longitude) ## -4.144637
min(crime_data_outcomes_plymouth$Longitude) ## -4.164925

max(crime_data_outcomes_plymouth$Latitude) ## 50.37173
min(crime_data_outcomes_plymouth$Latitude) ## 50.36098

## Both the same

crime_data_stopandsearch_plymouth<-crime_data_stopandsearch_all |> filter(Latitude>50.36098 & Latitude<50.37173) |> 
  filter(Longitude>-4.164925 & Longitude< -4.144637)

## Convert some of the data types

str(crime_data_street_plymouth)

crime_data_street_plymouth$Month<-as.Date(paste(crime_data_street_plymouth$Month, "-01", sep=""))


## Street level analysis

crime_data_street_plymouth |> group_by()