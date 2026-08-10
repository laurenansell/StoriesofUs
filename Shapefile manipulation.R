##########################################################################################################################
###                                                                                                                    ###
###                                                                                                                    ###
###                                         Shapefile manipulation for plots                                           ###
###                                                                                                                    ###
###                                                                                                                    ###
##########################################################################################################################

## Created by: LA
## Creation date: 10/08/2026

## Modified on: 

## Load in the required libraries
library(tidyverse)
library(sf)


## Read in the data
lsoa_sf <- read_sf("../Shapefiles/LSOA_2021_EW_BSC_V4.shp")

plot(lsoa_sf)

plymouth_lsoas<-c("Plymouth 034A", "Plymouth 034B", "Plymouth 034C", "Plymouth 034D", "Plymouth 034E")

hastings_lsoas<-c("Hastings 009A", "Hastings 009B", "Hastings 009C", "Hastings 009D")

lsoa_sf |> filter(LSOA21NM %in% hastings_lsoas) |> plot()

hastings_lsoas_shapefile<-lsoa_sf |> filter(LSOA21NM %in% hastings_lsoas)

plymouth_lsoas_shapefile<-lsoa_sf |> filter(LSOA21NM %in% plymouth_lsoas)

plot(plymouth_lsoas_shapefile)


st_write(hastings_lsoas_shapefile, "../Shapefiles/Hastings_LSOAs.shp")

st_write(plymouth_lsoas_shapefile, "../Shapefiles/Plymouth_LSOAs.shp")
