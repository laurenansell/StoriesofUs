##########################################################################################################################
###                                                                                                                    ###
###                                                                                                                    ###
###                           Community Life Survey Analysis for Data Explainer Pack                                   ###
###                                                                                                                    ###
###                                                                                                                    ###
##########################################################################################################################

## Created by: LA
## Creation date: 05/08/2026

## Modified on: 11/08/2026


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

## Clean the emvironment

rm(data)

## Filtering down to Stonehouse is may possible as there is no variable for MSOA or LSOA - but could use the IMD decile
## to convert? - keep at the level it is.

## Columns to keep:
##  SBeNeigh
##  SPull
##  SPullL
##  LocAtt
##  FUnPd1A (-N)
##  CivAct21 (-28)
##  Assets2A_a (-Assets3_M)
##  LocInvNa (-LocInvNg)

Hastings_CLS_reduced<-Hastings_CLS |> select(SBeNeigh,SPull,SPullL,LocAtt,FUnPd1A,FUnPd1B,FUnPd1C,FUnPd1D,FUnPd1E,FUnPd1F,
                                             FUnPd1G,FUnPd1H,FUnPd1I,FUnPd1J,FUnPd1K,FUnPd1L,FUnPd1M,FUnPd1N,
                                             CivAct21,CivAct22,CivAct23,CivAct24,CivAct25,CivAct26,CivAct27,CivAct28,
                                             Assets2A_a,Assets2A_b,Assets2A_c,Assets2A_d,Assets2B_a,Assets2B_b,
                                             Assets2B_c,Assets2B_d,Assets2C_a,Assets2C_b,Assets2C_c,Assets2C_d,
                                             Assets3_A,Assets3_B,Assets3_C,Assets3_D,Assets3_E,Assets3_F,Assets3_G,
                                             Assets3_H,Assets3_I,Assets3_J,Assets3_K,Assets3_L,Assets3_M,
                                             LocInvNa,LocInvNb,LocInvNc,LocInvNd,LocInvNe,LocInvNf,LocInvNg)