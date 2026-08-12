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
##              10/08/2026
##              12/08/2026

## Load in the required libraries
library(tidyverse)
library(sf)
library(mapview)
library(leaflet)
library(leaflet.extras)

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


## Hastings

crime_data_street_hastings<-crime_data_street_all |> filter(LSOA.name %in% hastings_lsoas)

crime_data_outcomes_hastings<-crime_data_outcomes_all |> filter(LSOA.name %in% hastings_lsoas)

## Use the latitude and longitudes from the subsets above to filter the stop and search data

max(crime_data_street_hastings$Longitude) ## 0.590257
min(crime_data_street_hastings$Longitude) ## 0.570389

max(crime_data_street_hastings$Latitude) ## 50.86436
min(crime_data_street_hastings$Latitude) ## 50.85371

## Check for any differences with the outcome data

max(crime_data_outcomes_hastings$Longitude) ## 0.590257
min(crime_data_outcomes_hastings$Longitude) ## 0.570389

max(crime_data_outcomes_hastings$Latitude) ## 50.86436
min(crime_data_outcomes_hastings$Latitude) ## 50.85371

## Both the same

crime_data_stopandsearch_hastings<-crime_data_stopandsearch_all |> filter(Latitude>50.85371 & Latitude<50.86436) |> 
  filter(Longitude>0.570389 & Longitude<0.590257)


## Save data for the explainer packs

write.csv(crime_data_street_hastings,"../Data for Explainer Pack/hastings_street.csv",row.names = FALSE)
write.csv(crime_data_stopandsearch_hastings,"../Data for Explainer Pack/hastings_stopandsearch.csv",row.names = FALSE)
write.csv(crime_data_outcomes_hastings,"../Data for Explainer Pack/hastings_outcomes.csv",row.names = FALSE)


## Convert some of the data types

str(crime_data_street_hastings)

crime_data_street_hastings$Month<-as.Date(paste(crime_data_street_hastings$Month, "-01", sep=""))


## Street level analysis

crime_data_street_hastings |> group_by(Crime.type) |> count() |> 
  ggplot(aes(x=Crime.type,y=n))+geom_bar(stat = "identity")+
  coord_flip()


mapview(crime_data_street_hastings, xcol = "Longitude", ycol = "Latitude", colour="Crime.type", 
        crs = 4269, grid = FALSE)


leaflet() |> setView(lng = 0.580323, lat = 50.85903, zoom = 15) |> 
  addTiles()


factpal <- colorFactor(topo.colors(15), crime_data_street_hastings$Crime.type)
factpal1 <- colorFactor(topo.colors(10), crime_data_stopandsearch_hastings$Object.of.search)

leaflet(data = crime_data_street_hastings) |> addTiles() |>
  addCircleMarkers(~Longitude, ~Latitude, popup = ~as.character(Crime.type),
                   color = ~factpal(Crime.type)) |> 
  addLegend(pal = factpal, values = ~Crime.type, opacity = 1,position = "topleft")

leaflet(data =crime_data_stopandsearch_hastings) |> addTiles() |>
  addCircleMarkers(~Longitude, ~Latitude, popup = ~as.character(Object.of.search),
             color=~factpal1(Object.of.search)) |> 
  addLegend(pal = factpal1, values = ~Object.of.search, opacity = 1,position = "topleft")

leaflet(crime_data_stopandsearch_hastings) %>%
  addTiles() %>%  # Add default OpenStreetMap background
  addHeatmap(
    lng = ~Longitude, 
    lat = ~Latitude, 
    blur = 20, 
    max = 0.05, 
    radius = 15
  )


leaflet(crime_data_street_hastings) %>%
  addTiles() %>%  # Add default OpenStreetMap background
  addHeatmap(
    lng = ~Longitude, 
    lat = ~Latitude, 
    blur = 20, 
    max = 0.05, 
    radius = 15
  )


## Plymouth

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