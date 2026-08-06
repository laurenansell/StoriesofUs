##########################################################################################################################
###                                                                                                                    ###
###                                                                                                                    ###
###                              Police: Crimes, Outcomes and Stop and Search data                                     ###
###                                                                                                                    ###
###                                                                                                                    ###
##########################################################################################################################

## Created by: LA
## Creation date: 05/08/2026

## Modified on:


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
## For Hastings: Hastings 001A, Hastings 001B, Hastings 001C, Hastings 005A, Hastings 005B, Hastings 002A, Hastings 008A,
## Hastings 008B, Hastings 005C, Hastings 009A, Hastings 009B, Hastings 009C, Hastings 009D, Hastings 011A, Hastings 011B,
## Hastings 011C, Hastings 011D, Hastings 002B, Hastings 001D, Hastings 002C, Hastings 011E, Hastings 008C, Hastings 008D,
## Hastings 008E, Hastings 003A, Hastings 003B, Hastings 003C, Hastings 003D, Hastings 010A, Hastings 010B, Hastings 010C, 
## Hastings 007A, Hastings 007B, Hastings 007C, Hastings 007D, Hastings 004A, Hastings 004B, Hastings 004C, Hastings 002D,
## Hastings 002E, Hastings 002F, Hastings 006A, Hastings 006B, Hastings 006C, Hastings 004D, Hastings 007E, Hastings 005D,
## Hastings 010D, Hastings 010E, Hastings 010F, Hastings 003E, Hastings 006D, Hastings 006E

plymouth_lsoas<-c("Plymouth 034A", "Plymouth 034B", "Plymouth 034C", "Plymouth 034D", "Plymouth 034E")

crime_data_street_plymouth<-crime_data_street_all |> filter(LSOA.name %in% plymouth_lsoas)

crime_data_outcomes_plymouth<-crime_data_outcomes_all |> filter(LSOA.name %in% plymouth_lsoas)
