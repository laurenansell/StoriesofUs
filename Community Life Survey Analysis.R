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
##              12/08/2026


## Load in the required libraries
library(tidyverse)
library(haven)
library(likert)


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
##  Assets2A_a (-Assets2L_d)
##  LocInvNa (-LocInvNg)

Hastings_CLS_reduced<-Hastings_CLS |> select(SBeNeigh,SPull,SPullL,LocAtt,FUnPd1A,FUnPd1B,FUnPd1C,FUnPd1D,FUnPd1E,FUnPd1F,
                                             FUnPd1G,FUnPd1H,FUnPd1I,FUnPd1J,FUnPd1K,FUnPd1L,FUnPd1M,FUnPd1N,
                                             CivAct21,CivAct22,CivAct23,CivAct24,CivAct25,CivAct26,CivAct27,CivAct28,
                                             Assets2A_a,Assets2A_b,Assets2A_c,Assets2A_d,Assets2B_a,Assets2B_b,
                                             Assets2B_c,Assets2B_d,Assets2C_a,Assets2C_b,Assets2C_c,Assets2C_d,
                                             Assets2D_a,Assets2D_b,Assets2D_c,Assets2D_d,Assets2E_a,Assets2E_b,
                                             Assets2E_c,Assets2E_d,Assets2F_a,Assets2F_b,Assets2F_c,Assets2F_d,
                                             Assets2G_a,Assets2G_b,Assets2G_c,Assets2G_d,Assets2H_a,Assets2H_b,
                                             Assets2H_c,Assets2H_d,Assets2I_a,Assets2I_b,Assets2I_c,Assets2I_d,
                                             Assets2J_a,Assets2J_b,Assets2J_c,Assets2J_d,Assets2K_a,Assets2K_b,
                                             Assets2K_c,Assets2K_d,Assets2K_e,Assets2L_a,Assets2L_b,Assets2L_c,
                                             Assets2L_d,
                                             LocInvNa,LocInvNb,LocInvNc,LocInvNd,LocInvNe,LocInvNf,LocInvNg,lad25cd)

## Save data for the explainer packs


write.csv(Hastings_CLS_reduced,"../Data for Explainer Pack/hastings_cls.csv",row.names = FALSE)

table(Hastings_CLS_reduced$SBeNeigh)

SBeNeigh_f<-Hastings_CLS_reduced$SBeNeigh |> as.factor() |> as.data.frame()


names(SBeNeigh_f) <- "How strongly do you feel you belong to your immediate neighbourhood"


lik <- likert(items=SBeNeigh_f[,1,drop=FALSE])

plot(lik)


Hastings_CLS_reduced |> group_by(SPull) |> count() |> 
  ggplot(aes(x=SPull,y=n))+geom_bar(stat="identity")
