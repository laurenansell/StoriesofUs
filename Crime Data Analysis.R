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

################################ Street Level Data #######################################################################

## Read in the data
temp = list.files(path="./Crime Data Download/Street",pattern="*.csv", full.names = TRUE)
myfiles = lapply(temp, read.csv)


crime_data_street_all<-data.frame()


for(i in 1:72){
  
  df<-myfiles[[i]]
  
  crime_data_street_all<-rbind(crime_data_street_all,df)
  
}



################################ Outcomes Data ##########################################################################

## Read in the data
temp = list.files(path="./Crime Data Download/Outcomes",pattern="*.csv", full.names = TRUE)
myfiles = lapply(temp, read.csv)


crime_data_outcomes_all<-data.frame()


for(i in 1:72){
  
  df<-myfiles[[i]]
  
  crime_data_outcomes_all<-rbind(crime_data_outcomes_all,df)
  
}



################################ Stop and Search Data ###################################################################

## Read in the data
temp = list.files(path="./Crime Data Download/Stop and Search",pattern="*.csv", full.names = TRUE)
myfiles = lapply(temp, read.csv)


crime_data_stopandsearch_all<-data.frame()


for(i in 1:72){
  
  df<-myfiles[[i]]
  
  crime_data_stopandsearch_all<-rbind(crime_data_stopandsearch_all,df)
  
}

