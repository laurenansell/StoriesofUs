##########################################################################################################################
###                                                                                                                    ###
###                                                                                                                    ###
###                           Community Life Survey Analysis for Data Explainer Pack                                   ###
###                                                                                                                    ###
###                                                                                                                    ###
##########################################################################################################################

## Created by: LA
## Creation date: 05/08/2026

## Modified on:


## Load in the required libraries
library(tidyverse)
library(haven)

## Read in the data
data<-read_sav("community_life_survey_2024_25_annual_data_safeguard.sav")

## View the data
View(data)

## Column which contains geographic information:  lad25nm - GSS Local Authority District Name (2025)
## value 113 for Hastings and 186 for Plymouth.

## Have checked - there is no missing data in these columns

## First remove all other areas and create a dataset for Plymouth and for Hastings

Hastings_CLS<-data |> filter(lad25nm==113) ## 464 observations for Hastings

Plymouth_CLS<-data |> filter(lad25nm==186) ## 657 observations for Plymouth (will need to filter down for (Stonehouse)

## Filtering down to Stonehouse is may possible as there is no variable for MSOA or LSOA - but could use the IMD decile
## to convert?

## Columns to keep:
##  SBeNeigh
##  SPull
##  SPullL
##  LocAtt
##  FUnPd1A (-N)
##  CivAct21 (-28)
##  Assets2A_a (-Assets3_M)
##  LocInvNa (-LocInvNg)

