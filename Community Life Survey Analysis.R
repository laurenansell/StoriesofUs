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
##              20/08/2026
##              24/08/2026
##              25/08/2026
##              03/09/2026
##              15/09/2026

## Load in the required libraries
library(tidyverse)
library(haven)
library(likert)
library(igraph)
library(ggraph)
library(fmsb)
library(cowplot)

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

## Clean the environment

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

## Hastings

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


## Pull together the multiple answer questions

## Unpaid help

Hastings_CLS_unpaidhelp<-Hastings_CLS_reduced |> select(FUnPd1A,FUnPd1B,FUnPd1C,FUnPd1D,FUnPd1E,FUnPd1F,
                                                        FUnPd1G,FUnPd1H,FUnPd1I,FUnPd1J,FUnPd1K,FUnPd1L,
                                                        FUnPd1M,FUnPd1N)

## Lots of NAs

Hastings_CLS_unpaidhelp<-Hastings_CLS_unpaidhelp[complete.cases(Hastings_CLS_unpaidhelp), ]


## Removing all the NAs leaves a total of 276 observations.

## The total organisations that people support

Hastings_CLS_unpaidhelp$total<-rowSums(Hastings_CLS_unpaidhelp[,1:13]) ## don't include none of the above


Hastings_CLS_unpaidhelp |> group_by(total) |> count() |> 
  ggplot(aes(x=total,y=n))+geom_bar(stat="identity") ## remove the first entry for report


## Which is the most popular for support

colSums(Hastings_CLS_unpaidhelp)

## Create a dataframe for these results

help_type<-c("Raising or handling money/taking part in sponsored events",
             "Leading a group/member of a committee",
             "Getting other people involved",
             "Organising or helping to run an activity or event",
             "Visiting people",
             "Befriending or mentoring people",
             "Giving advice/information/counselling",
             "Secretarial, admin or clerical work",
             "Providing transport/driving",
             "Representing",
             "Campaigning",
             "Other practical help (e.g. helping out at school, shopping)",
             "Any other help",
             "None of the above")

Hastings_CLS_unpaidhelp$total<-rowSums(Hastings_CLS_unpaidhelp[,1:14])

help_total<-colSums(Hastings_CLS_unpaidhelp[,1:14])

unpaid_hastings<-cbind(help_type,help_total) |> as.data.frame()


ggplot(unpaid_hastings,aes(x=help_type,y=help_total))+geom_bar(stat="identity")+
  coord_flip()



write.csv(Hastings_CLS_unpaidhelp,"../Data for Explainer Pack/hastings_cls_unpaidhelp.csv",row.names = FALSE)

## Network plot to see the relationships

# Remove total and none column
groups <- Hastings_CLS_unpaidhelp[,1:13]

# Create co-occurrence matrix
co_mat <- t(as.matrix(groups)) %*% as.matrix(groups)

# Remove self-connections
diag(co_mat) <- 0

# Convert to edge list
edges <- as.data.frame(as.table(co_mat)) |>
  rename(from = Var1,
         to = Var2,
         weight = Freq) |>
  filter(weight > 0)

# Remove duplicate edges
edges <- edges |>
  rowwise() |>
  mutate(pair = paste(sort(c(from, to)), collapse = "_")) |>
  ungroup() |>
  distinct(pair, .keep_all = TRUE) |>
  select(-pair)

# Node frequencies
nodes <- data.frame(
  name = colnames(groups),
  freq = colSums(groups)
)

# Create graph
g <- graph_from_data_frame(
  d = edges,
  vertices = nodes,
  directed = FALSE
)

# Plot
ggraph(g, layout = "fr") +
  geom_edge_link(aes(width = weight),
                 alpha = 0.5,
                 colour = "steelblue") +
  geom_node_point(aes(size = freq),
                  colour = "orange") +
  geom_node_text(aes(label = name),
                 repel = TRUE) +
  scale_edge_width(range = c(0.5, 5)) +
  theme_void()


edges <- edges |>
  filter(weight >= 3)

labels <- c(
  FUnPd1A = "Raising money",
  FUnPd1B = "Committee member",
  FUnPd1C = "Encouraging others",
  FUnPd1D = "Helping to run",
  FUnPd1E = "Visiting people",
  FUnPd1F = "Mentoring",
  FUnPd1G = "Giving advice",
  FUnPd1H = "Admin",
  FUnPd1I = "Transport",
  FUnPd1J = "Representing",
  FUnPd1K = "Campaigning",
  FUnPd1L = "Other practical help",
  FUnPd1M = "Other help"
)

nodes<-cbind(labels,nodes)

nodes<-nodes |> select(labels,freq)

# Plot
ggraph(g, layout = "fr") +
  geom_edge_link(aes(width = weight),
                 alpha = 0.5,
                 colour = "steelblue") +
  geom_node_point(aes(size = freq),
                  colour = "orange") +
  geom_node_text(aes(label = labels),
                 repel = TRUE) +
  scale_edge_width(range = c(0.5, 5)) +
  theme_void()


## Assets information


Hastings_CLS_assets<-Hastings_CLS_reduced |> select(Assets2A_a,Assets2A_b,Assets2A_c,Assets2A_d,Assets2B_a,Assets2B_b,
                                                    Assets2B_c,Assets2B_d,Assets2C_a,Assets2C_b,Assets2C_c,Assets2C_d,
                                                    Assets2D_a,Assets2D_b,Assets2D_c,Assets2D_d,Assets2E_a,Assets2E_b,
                                                    Assets2E_c,Assets2E_d,Assets2F_a,Assets2F_b,Assets2F_c,Assets2F_d,
                                                    Assets2G_a,Assets2G_b,Assets2G_c,Assets2G_d,Assets2H_a,Assets2H_b,
                                                    Assets2H_c,Assets2H_d,Assets2I_a,Assets2I_b,Assets2I_c,Assets2I_d,
                                                    Assets2J_a,Assets2J_b,Assets2J_c,Assets2J_d,Assets2K_a,Assets2K_b,
                                                    Assets2K_c,Assets2K_d,Assets2K_e,Assets2L_a,Assets2L_b,Assets2L_c,
                                                    Assets2L_d)

## will have to handle the NAs differently here, change the NAs to 0.

Hastings_CLS_assets <- Hastings_CLS_assets %>% replace(is.na(.), 0)

## group the responses depending on which assets they are associated with

distance <- c("15-20 mintues", "Further but still local","None","Not sure/Don't know")


## General/grocery shop (Assets2A)

hastings_generalshop<-Hastings_CLS_assets |> select(Assets2A_a,Assets2A_b,Assets2A_c,Assets2A_d)

hastings_generalshop_total<-colSums(hastings_generalshop)

generalshop_hastings<-cbind(distance,hastings_generalshop_total) |> as.data.frame()

generalshop_hastings$hastings_generalshop_total<-as.numeric(generalshop_hastings$hastings_generalshop_total)

generalshop_hastings<-generalshop_hastings |> 
  mutate(Percentage = (hastings_generalshop_total/sum(hastings_generalshop_total))*100)

ggplot(generalshop_hastings,aes(x=distance,y=hastings_generalshop_total))+geom_bar(stat="identity")+
  coord_flip()
  

generalshop_hastings_scores <- data.frame(
  Minutes = 87.3983740,
  Still_local = 9.7560976,
  None = 0.8130081,
  Not_sure = 2.0325203
)

radar_data1 <- rbind(
  c(100, 100, 100, 100),
  c(0, 0, 0, 0),
  generalshop_hastings_scores
)

colnames(radar_data1) <- c(
  "15-20 mintues",
  "Further but still local",
  "None",
  "Not sure/Don't know"
)

radarchart(
  radar_data1,
  axistype = 1,
  pcol = "blue",
  pfcol = rgb(0, 0, 1, 0.3),
  plwd = 2,
  cglcol = "grey",
  cglty = 1,
  axislabcol = "black",
  vlcex = 0.9
)


## Pub/bar (Assets2B)

hastings_bar<-Hastings_CLS_assets |> select(Assets2B_a,Assets2B_b,Assets2B_c,Assets2B_d)

hastings_bar_total<-colSums(hastings_bar)

bar_hastings<-cbind(distance,hastings_bar_total) |> as.data.frame()

bar_hastings$hastings_bar_total<-as.numeric(bar_hastings$hastings_bar_total)

bar_hastings<-bar_hastings |> 
  mutate(Percentage = (hastings_bar_total/sum(hastings_bar_total))*100)

ggplot(bar_hastings,aes(x=distance,y=hastings_bar_total))+geom_bar(stat="identity")+
  coord_flip()


bar_hastings_scores <- data.frame(
  Minutes = 74.186992,
  Still_local = 18.699187,
  None = 3.455285,
  Not_sure = 3.658537
)

radar_data2 <- rbind(
  c(100, 100, 100, 100),
  c(0, 0, 0, 0),
  bar_hastings_scores
)

colnames(radar_data2) <- c(
  "15-20 mintues",
  "Further but still local",
  "None",
  "Not sure/Don't know"
)

radarchart(
  radar_data2,
  axistype = 1,
  pcol = "blue",
  pfcol = rgb(0, 0, 1, 0.3),
  plwd = 2,
  cglcol = "grey",
  cglty = 1,
  axislabcol = "black",
  vlcex = 0.9
)


## Park (Assets2C)

hastings_park<-Hastings_CLS_assets |> select(Assets2C_a,Assets2C_b,Assets2C_c,Assets2C_d)

hastings_park_total<-colSums(hastings_park)

park_hastings<-cbind(distance,hastings_park_total) |> as.data.frame()

park_hastings$hastings_park_total<-as.numeric(park_hastings$hastings_park_total)

park_hastings<-park_hastings |> 
  mutate(Percentage = (hastings_park_total/sum(hastings_park_total))*100)

ggplot(park_hastings,aes(x=distance,y=hastings_park_total))+geom_bar(stat="identity")+
  coord_flip()

park_hastings_scores <- data.frame(
  Minutes = 70.247934,
  Still_local = 22.727273,
  None = 4.545455,
  Not_sure = 2.479339
)

radar_data3 <- rbind(
  c(100, 100, 100, 100),
  c(0, 0, 0, 0),
  park_hastings_scores
)

colnames(radar_data3) <- c(
  "15-20 mintues",
  "Further but still local",
  "None",
  "Not sure/Don't know"
)

radarchart(
  radar_data3,
  axistype = 1,
  pcol = "blue",
  pfcol = rgb(0, 0, 1, 0.3),
  plwd = 2,
  cglcol = "grey",
  cglty = 1,
  axislabcol = "black",
  vlcex = 0.9
)

## Library (Assets2D)

hastings_library<-Hastings_CLS_assets |> select(Assets2D_a,Assets2D_b,Assets2D_c,Assets2D_d)

hastings_library_total<-colSums(hastings_library)

library_hastings<-cbind(distance,hastings_library_total) |> as.data.frame()

library_hastings$hastings_library_total<-as.numeric(library_hastings$hastings_library_total)

library_hastings<-library_hastings |> 
  mutate(Percentage = (hastings_library_total/sum(hastings_library_total))*100)

ggplot(library_hastings,aes(x=distance,y=hastings_library_total))+geom_bar(stat="identity")+
  coord_flip()

library_hastings_scores <- data.frame(
  Minutes = 39.658849,
  Still_local = 37.739872,
  None = 14.925373,
  Not_sure = 7.675906
)

radar_data4 <- rbind(
  c(100, 100, 100, 100),
  c(0, 0, 0, 0),
  library_hastings_scores
)

colnames(radar_data4) <- c(
  "15-20 mintues",
  "Further but still local",
  "None",
  "Not sure/Don't know"
)

radarchart(
  radar_data4,
  axistype = 1,
  pcol = "blue",
  pfcol = rgb(0, 0, 1, 0.3),
  plwd = 2,
  cglcol = "grey",
  cglty = 1,
  axislabcol = "black",
  vlcex = 0.9
)


## Restaurant/cafe (Assets2E)

hastings_cafe<-Hastings_CLS_assets |> select(Assets2E_a,Assets2E_b,Assets2E_c,Assets2E_d)

hastings_cafe_total<-colSums(hastings_cafe)

cafe_hastings<-cbind(distance,hastings_cafe_total) |> as.data.frame()

cafe_hastings$hastings_cafe_total<-as.numeric(cafe_hastings$hastings_cafe_total)

cafe_hastings<-cafe_hastings |> 
  mutate(Percentage = (hastings_cafe_total/sum(hastings_cafe_total))*100)

ggplot(cafe_hastings,aes(x=distance,y=hastings_cafe_total))+geom_bar(stat="identity")+
  coord_flip()


cafe_hastings_scores <- data.frame(
  Minutes = 66.8008048,
  Still_local = 26.7605634,
  None = 5.8350101,
  Not_sure = 0.6036217
)

radar_data5 <- rbind(
  c(100, 100, 100, 100),
  c(0, 0, 0, 0),
  cafe_hastings_scores
)

colnames(radar_data5) <- c(
  "15-20 mintues",
  "Further but still local",
  "None",
  "Not sure/Don't know"
)

radarchart(
  radar_data5,
  axistype = 1,
  pcol = "blue",
  pfcol = rgb(0, 0, 1, 0.3),
  plwd = 2,
  cglcol = "grey",
  cglty = 1,
  axislabcol = "black",
  vlcex = 0.9
)


## Community centre/hall (Assets2F)

hastings_communityhall<-Hastings_CLS_assets |> select(Assets2F_a,Assets2F_b,Assets2F_c,Assets2F_d)

hastings_communityhall_total<-colSums(hastings_communityhall)

communityhall_hastings<-cbind(distance,hastings_communityhall_total) |> as.data.frame()

communityhall_hastings$hastings_communityhall_total<-as.numeric(communityhall_hastings$hastings_communityhall_total)

communityhall_hastings<-communityhall_hastings |> 
  mutate(Percentage = (hastings_communityhall_total/sum(hastings_communityhall_total))*100)

ggplot(communityhall_hastings,aes(x=distance,y=hastings_communityhall_total))+geom_bar(stat="identity")+
  coord_flip()

communityhall_hastings_scores <- data.frame(
  Minutes = 56.935818,
  Still_local = 21.739130,
  None = 7.039337,
  Not_sure = 14.285714
)

radar_data6 <- rbind(
  c(100, 100, 100, 100),
  c(0, 0, 0, 0),
  communityhall_hastings_scores
)

colnames(radar_data6) <- c(
  "15-20 mintues",
  "Further but still local",
  "None",
  "Not sure/Don't know"
)

radarchart(
  radar_data6,
  axistype = 1,
  pcol = "blue",
  pfcol = rgb(0, 0, 1, 0.3),
  plwd = 2,
  cglcol = "grey",
  cglty = 1,
  axislabcol = "black",
  vlcex = 0.9
)


## Sports facilities (Assets2G)

hastings_sportsfacilities<-Hastings_CLS_assets |> select(Assets2G_a,Assets2G_b,Assets2G_c,Assets2G_d)

hastings_sportsfacilities_total<-colSums(hastings_sportsfacilities)

sportsfacilities_hastings<-cbind(distance,hastings_sportsfacilities_total) |> as.data.frame()

sportsfacilities_hastings$hastings_sportsfacilities_total<-as.numeric(sportsfacilities_hastings$hastings_sportsfacilities_total)

sportsfacilities_hastings<-sportsfacilities_hastings |> 
  mutate(Percentage = (hastings_sportsfacilities_total/sum(hastings_sportsfacilities_total))*100)

ggplot(sportsfacilities_hastings,aes(x=distance,y=hastings_sportsfacilities_total))+geom_bar(stat="identity")+
  coord_flip()


sportsfacilities_hastings_scores <- data.frame(
  Minutes = 38.75000,
  Still_local = 42.08333,
  None = 12.29167,
  Not_sure = 6.87500
)

radar_data7 <- rbind(
  c(100, 100, 100, 100),
  c(0, 0, 0, 0),
  sportsfacilities_hastings_scores
)

colnames(radar_data7) <- c(
  "15-20 mintues",
  "Further but still local",
  "None",
  "Not sure/Don't know"
)

radarchart(
  radar_data7,
  axistype = 1,
  pcol = "blue",
  pfcol = rgb(0, 0, 1, 0.3),
  plwd = 2,
  cglcol = "grey",
  cglty = 1,
  axislabcol = "black",
  vlcex = 0.9
)

## Health centre/GP (Assets2H)

hastings_gp<-Hastings_CLS_assets |> select(Assets2H_a,Assets2H_b,Assets2H_c,Assets2H_d)

hastings_gp_total<-colSums(hastings_gp)

gp_hastings<-cbind(distance,hastings_gp_total) |> as.data.frame()

gp_hastings$hastings_gp_total<-as.numeric(gp_hastings$hastings_gp_total)

gp_hastings<-gp_hastings |> 
  mutate(Percentage = (hastings_gp_total/sum(hastings_gp_total))*100)

ggplot(gp_hastings,aes(x=distance,y=hastings_gp_total))+geom_bar(stat="identity")+
  coord_flip()

gp_hastings_scores <- data.frame(
  Minutes = 57.809331,
  Still_local = 35.294118,
  None = 4.462475,
  Not_sure = 2.434077
)

radar_data8 <- rbind(
  c(100, 100, 100, 100),
  c(0, 0, 0, 0),
  gp_hastings_scores
)

colnames(radar_data8) <- c(
  "15-20 mintues",
  "Further but still local",
  "None",
  "Not sure/Don't know"
)

radarchart(
  radar_data8,
  axistype = 1,
  pcol = "blue",
  pfcol = rgb(0, 0, 1, 0.3),
  plwd = 2,
  cglcol = "grey",
  cglty = 1,
  axislabcol = "black",
  vlcex = 0.9
)

## Chemist or pharmacy (Assets2I)

hastings_chemist<-Hastings_CLS_assets |> select(Assets2I_a,Assets2I_b,Assets2I_c,Assets2I_d)

hastings_chemist_total<-colSums(hastings_chemist)

chemist_hastings<-cbind(distance,hastings_chemist_total) |> as.data.frame()

chemist_hastings$hastings_chemist_total<-as.numeric(chemist_hastings$hastings_chemist_total)

chemist_hastings<-chemist_hastings |> 
  mutate(Percentage = (hastings_chemist_total/sum(hastings_chemist_total))*100)

ggplot(chemist_hastings,aes(x=distance,y=hastings_chemist_total))+geom_bar(stat="identity")+
  coord_flip()

chemist_hastings_scores <- data.frame(
  Minutes = 76.659960,
  Still_local = 20.321932,
  None = 2.213280,
  Not_sure = 0.804829
)

radar_data9 <- rbind(
  c(100, 100, 100, 100),
  c(0, 0, 0, 0),
  chemist_hastings_scores
)

colnames(radar_data9) <- c(
  "15-20 mintues",
  "Further but still local",
  "None",
  "Not sure/Don't know"
)

radarchart(
  radar_data9,
  axistype = 1,
  pcol = "blue",
  pfcol = rgb(0, 0, 1, 0.3),
  plwd = 2,
  cglcol = "grey",
  cglty = 1,
  axislabcol = "black",
  vlcex = 0.9
)

## Post Office (Assets2J)

hastings_postoffice<-Hastings_CLS_assets |> select(Assets2J_a,Assets2J_b,Assets2J_c,Assets2J_d)

hastings_postoffice_total<-colSums(hastings_postoffice)

postoffice_hastings<-cbind(distance,hastings_postoffice_total) |> as.data.frame()

postoffice_hastings$hastings_postoffice_total<-as.numeric(postoffice_hastings$hastings_postoffice_total)

postoffice_hastings<-postoffice_hastings |> 
  mutate(Percentage = (hastings_postoffice_total/sum(hastings_postoffice_total))*100)

ggplot(postoffice_hastings,aes(x=distance,y=hastings_postoffice_total))+geom_bar(stat="identity")+
  coord_flip()

postoffice_hastings_scores <- data.frame(
  Minutes = 68.801653,
  Still_local = 23.347107,
  None = 6.198347,
  Not_sure = 1.652893
)

radar_data10 <- rbind(
  c(100, 100, 100, 100),
  c(0, 0, 0, 0),
  postoffice_hastings_scores
)

colnames(radar_data10) <- c(
  "15-20 mintues",
  "Further but still local",
  "None",
  "Not sure/Don't know"
)

radarchart(
  radar_data10,
  axistype = 1,
  pcol = "blue",
  pfcol = rgb(0, 0, 1, 0.3),
  plwd = 2,
  cglcol = "grey",
  cglty = 1,
  axislabcol = "black",
  vlcex = 0.9
)

## Place of worship for my faith or religion, such as a church, mosque, temple (Assets2K)

hastings_worship<-Hastings_CLS_assets |> select(Assets2K_a,Assets2K_b,Assets2K_c,Assets2K_d)

hastings_worship_total<-colSums(hastings_worship)

worship_hastings<-cbind(distance,hastings_worship_total) |> as.data.frame()

worship_hastings$hastings_worship_total<-as.numeric(worship_hastings$hastings_worship_total)

worship_hastings<-worship_hastings |> 
  mutate(Percentage = (hastings_worship_total/sum(hastings_worship_total))*100)

ggplot(worship_hastings,aes(x=distance,y=hastings_worship_total))+geom_bar(stat="identity")+
  coord_flip()

worship_hastings_scores <- data.frame(
  Minutes = 71.348315,
  Still_local = 19.101124,
  None = 3.370787,
  Not_sure = 6.179775
)

radar_data11 <- rbind(
  c(100, 100, 100, 100),
  c(0, 0, 0, 0),
  worship_hastings_scores
)

colnames(radar_data11) <- c(
  "15-20 mintues",
  "Further but still local",
  "None",
  "Not sure/Don't know"
)

radarchart(
  radar_data11,
  axistype = 1,
  pcol = "blue",
  pfcol = rgb(0, 0, 1, 0.3),
  plwd = 2,
  cglcol = "grey",
  cglty = 1,
  axislabcol = "black",
  vlcex = 0.9
)

## Public transport links (Assets2L)	

hastings_publictransport<-Hastings_CLS_assets |> select(Assets2L_a,Assets2L_b,Assets2L_c,Assets2L_d)

hastings_publictransport_total<-colSums(hastings_publictransport)

publictransport_hastings<-cbind(distance,hastings_publictransport_total) |> as.data.frame()

publictransport_hastings$hastings_publictransport_total<-as.numeric(publictransport_hastings$hastings_publictransport_total)

publictransport_hastings<-publictransport_hastings |> 
  mutate(Percentage = (hastings_publictransport_total/sum(hastings_publictransport_total))*100)

ggplot(publictransport_hastings,aes(x=distance,y=hastings_publictransport_total))+geom_bar(stat="identity")+
  coord_flip()

publictransport_hastings_scores <- data.frame(
  Minutes = 86.1932939,
  Still_local = 11.8343195,
  None = 0.7889546,
  Not_sure = 1.1834320
)

radar_data12 <- rbind(
  c(100, 100, 100, 100),
  c(0, 0, 0, 0),
  publictransport_hastings_scores
)

colnames(radar_data12) <- c(
  "15-20 mintues",
  "Further but still local",
  "None",
  "Not sure/Don't know"
)

radarchart(
  radar_data12,
  axistype = 1,
  pcol = "blue",
  pfcol = rgb(0, 0, 1, 0.3),
  plwd = 2,
  cglcol = "grey",
  cglty = 1,
  axislabcol = "black",
  vlcex = 0.9
)

## Combined radar plots

## Which are the most insteresting to compare:
## General shops
## Library
## Parks

asset_data <- data.frame(
  Minutes = c(87.3983740,39.658849, 70.247934),
  Still_local = c(9.7560976,37.739872,22.727273),
  None = c(0.8130081,14.925373,4.545455),
  Not_sure = c(2.0325203, 7.675906,4.545455)
)

rownames(asset_data) <- c(
  "General shop",
  "Library",
  "Park"
)


radar_data <- rbind(
  rep(100, ncol(asset_data)),
  rep(0, ncol(asset_data)),
  asset_data
)

radarchart(
  radar_data,
  axistype = 1,
  
  # Colours for each ward
  pcol = c("red", "blue", "darkgreen"),
  pfcol = c(
    rgb(1,0,0,0.2),
    rgb(0,0,1,0.2),
    rgb(0,0.5,0,0.2)
  ),
  plwd = 2,
  plty = 1,
  
  cglcol = "grey",
  cglty = 1,
  axislabcol = "black",
  vlcex = 0.9
)

legend(
  "topright",
  legend = rownames(radar_data[3:5,]),
  col = c("red", "blue", "darkgreen"),
  lty = 1,
  lwd = 2,
  bty = "n"
)



## Plymouth

Plymouth_CLS_reduced<-Plymouth_CLS |> select(SBeNeigh,SPull,SPullL,LocAtt,FUnPd1A,FUnPd1B,FUnPd1C,FUnPd1D,FUnPd1E,FUnPd1F,
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


write.csv(Plymouth_CLS_reduced,"../Data for Explainer Pack/Plymouth_cls.csv",row.names = FALSE)

table(Plymouth_CLS_reduced$SBeNeigh)

SBeNeigh_f<-Plymouth_CLS_reduced$SBeNeigh |> as.factor() |> as.data.frame()


names(SBeNeigh_f) <- "How strongly do you feel you belong to your immediate neighbourhood"


lik <- likert(items=SBeNeigh_f[,1,drop=FALSE])

plot(lik)


Plymouth_CLS_reduced |> group_by(SPull) |> count() |> 
  ggplot(aes(x=SPull,y=n))+geom_bar(stat="identity")


## Pull together the multiple answer questions

## Unpaid help

Plymouth_CLS_unpaidhelp<-Plymouth_CLS_reduced |> select(FUnPd1A,FUnPd1B,FUnPd1C,FUnPd1D,FUnPd1E,FUnPd1F,
                                                        FUnPd1G,FUnPd1H,FUnPd1I,FUnPd1J,FUnPd1K,FUnPd1L,
                                                        FUnPd1M,FUnPd1N)

## Lots of NAs

Plymouth_CLS_unpaidhelp<-Plymouth_CLS_unpaidhelp[complete.cases(Plymouth_CLS_unpaidhelp), ]


## Removing all the NAs leaves a total of 276 observations.

## The total organisations that people support

Plymouth_CLS_unpaidhelp$total<-rowSums(Plymouth_CLS_unpaidhelp[,1:13]) ## don't include none of the above


Plymouth_CLS_unpaidhelp |> group_by(total) |> count() |> 
  ggplot(aes(x=total,y=n))+geom_bar(stat="identity") ## remove the first entry for report


## Which is the most popular for support

colSums(Plymouth_CLS_unpaidhelp)

## Create a dataframe for these results

help_type<-c("Raising or handling money/taking part in sponsored events",
             "Leading a group/member of a committee",
             "Getting other people involved",
             "Organising or helping to run an activity or event",
             "Visiting people",
             "Befriending or mentoring people",
             "Giving advice/information/counselling",
             "Secretarial, admin or clerical work",
             "Providing transport/driving",
             "Representing",
             "Campaigning",
             "Other practical help (e.g. helping out at school, shopping)",
             "Any other help",
             "None of the above")

Plymouth_CLS_unpaidhelp$total<-rowSums(Plymouth_CLS_unpaidhelp[,1:14])

help_total<-colSums(Plymouth_CLS_unpaidhelp[,1:14])

unpaid_Plymouth<-cbind(help_type,help_total) |> as.data.frame()


ggplot(unpaid_Plymouth,aes(x=help_type,y=help_total))+geom_bar(stat="identity")+
  coord_flip()



write.csv(Plymouth_CLS_unpaidhelp,"../Data for Explainer Pack/Plymouth_cls_unpaidhelp.csv",row.names = FALSE)

## Network plot to see the relationships

# Remove total and none column
groups <- Plymouth_CLS_unpaidhelp[,1:13]

# Create co-occurrence matrix
co_mat <- t(as.matrix(groups)) %*% as.matrix(groups)

# Remove self-connections
diag(co_mat) <- 0

# Convert to edge list
edges <- as.data.frame(as.table(co_mat)) |>
  rename(from = Var1,
         to = Var2,
         weight = Freq) |>
  filter(weight > 0)

# Remove duplicate edges
edges <- edges |>
  rowwise() |>
  mutate(pair = paste(sort(c(from, to)), collapse = "_")) |>
  ungroup() |>
  distinct(pair, .keep_all = TRUE) |>
  select(-pair)

# Node frequencies
nodes <- data.frame(
  name = colnames(groups),
  freq = colSums(groups)
)

# Create graph
g <- graph_from_data_frame(
  d = edges,
  vertices = nodes,
  directed = FALSE
)

# Plot
ggraph(g, layout = "fr") +
  geom_edge_link(aes(width = weight),
                 alpha = 0.5,
                 colour = "steelblue") +
  geom_node_point(aes(size = freq),
                  colour = "orange") +
  geom_node_text(aes(label = name),
                 repel = TRUE) +
  scale_edge_width(range = c(0.5, 5)) +
  theme_void()


edges <- edges |>
  filter(weight >= 3)

labels <- c(
  FUnPd1A = "Raising money",
  FUnPd1B = "Committee member",
  FUnPd1C = "Encouraging others",
  FUnPd1D = "Helping to run",
  FUnPd1E = "Visiting people",
  FUnPd1F = "Mentoring",
  FUnPd1G = "Giving advice",
  FUnPd1H = "Admin",
  FUnPd1I = "Transport",
  FUnPd1J = "Representing",
  FUnPd1K = "Campaigning",
  FUnPd1L = "Other practical help",
  FUnPd1M = "Other help"
)

nodes<-cbind(labels,nodes)

nodes<-nodes |> select(labels,freq)

# Plot
ggraph(g, layout = "fr") +
  geom_edge_link(aes(width = weight),
                 alpha = 0.5,
                 colour = "steelblue") +
  geom_node_point(aes(size = freq),
                  colour = "orange") +
  geom_node_text(aes(label = labels),
                 repel = TRUE) +
  scale_edge_width(range = c(0.5, 5)) +
  theme_void()


## Assets information


Plymouth_CLS_assets<-Plymouth_CLS_reduced |> select(Assets2A_a,Assets2A_b,Assets2A_c,Assets2A_d,Assets2B_a,Assets2B_b,
                                                    Assets2B_c,Assets2B_d,Assets2C_a,Assets2C_b,Assets2C_c,Assets2C_d,
                                                    Assets2D_a,Assets2D_b,Assets2D_c,Assets2D_d,Assets2E_a,Assets2E_b,
                                                    Assets2E_c,Assets2E_d,Assets2F_a,Assets2F_b,Assets2F_c,Assets2F_d,
                                                    Assets2G_a,Assets2G_b,Assets2G_c,Assets2G_d,Assets2H_a,Assets2H_b,
                                                    Assets2H_c,Assets2H_d,Assets2I_a,Assets2I_b,Assets2I_c,Assets2I_d,
                                                    Assets2J_a,Assets2J_b,Assets2J_c,Assets2J_d,Assets2K_a,Assets2K_b,
                                                    Assets2K_c,Assets2K_d,Assets2K_e,Assets2L_a,Assets2L_b,Assets2L_c,
                                                    Assets2L_d)

## will have to handle the NAs differently here, change the NAs to 0.

Plymouth_CLS_assets <- Plymouth_CLS_assets %>% replace(is.na(.), 0)

## group the responses depending on which assets they are associated with

distance <- c("15-20 mintues", "Further but still local","None","Not sure/Don't know")

## General/grocery shop (Assets2A)

Plymouth_generalshop<-Plymouth_CLS_assets |> select(Assets2A_a,Assets2A_b,Assets2A_c,Assets2A_d)

Plymouth_generalshop_total<-colSums(Plymouth_generalshop)

generalshop_Plymouth<-cbind(distance,Plymouth_generalshop_total) |> as.data.frame()

generalshop_Plymouth$Plymouth_generalshop_total<-as.numeric(generalshop_Plymouth$Plymouth_generalshop_total)

ggplot(generalshop_Plymouth,aes(x=distance,y=Plymouth_generalshop_total))+geom_bar(stat="identity")+
  coord_flip()

generalshop_Plymouth<-generalshop_Plymouth |> 
  mutate(Percentage = (Plymouth_generalshop_total/sum(Plymouth_generalshop_total))*100)


generalshop_plymouth_scores <- data.frame(
  Minutes = 87.804878,
  Still_local = 9.038737,
  None = 1.434720,
  Not_sure = 1.721664
)

radar_data1 <- rbind(
  c(100, 100, 100, 100),
  c(0, 0, 0, 0),
  generalshop_plymouth_scores
)

colnames(radar_data1) <- c(
  "15-20 mintues",
  "Further but still local",
  "None",
  "Not sure/Don't know"
)

radarchart(
  radar_data1,
  axistype = 1,
  pcol = "blue",
  pfcol = rgb(0, 0, 1, 0.3),
  plwd = 2,
  cglcol = "grey",
  cglty = 1,
  axislabcol = "black",
  vlcex = 0.9
)

## Pub/bar (Assets2B)

Plymouth_bar<-Plymouth_CLS_assets |> select(Assets2B_a,Assets2B_b,Assets2B_c,Assets2B_d)

Plymouth_bar_total<-colSums(Plymouth_bar)

bar_Plymouth<-cbind(distance,Plymouth_bar_total) |> as.data.frame()

bar_Plymouth$Plymouth_bar_total<-as.numeric(bar_Plymouth$Plymouth_bar_total)

ggplot(bar_Plymouth,aes(x=distance,y=Plymouth_bar_total))+geom_bar(stat="identity")+
  coord_flip()

bar_Plymouth<-bar_Plymouth |> 
  mutate(Percentage = (Plymouth_bar_total/sum(Plymouth_bar_total))*100)


bar_plymouth_scores <- data.frame(
  Minutes = 79.106628,
  Still_local = 12.968300,
  None = 4.178674,
  Not_sure = 3.746398
)

radar_data2 <- rbind(
  c(100, 100, 100, 100),
  c(0, 0, 0, 0),
  bar_plymouth_scores
)

colnames(radar_data2) <- c(
  "15-20 mintues",
  "Further but still local",
  "None",
  "Not sure/Don't know"
)

radarchart(
  radar_data2,
  axistype = 1,
  pcol = "blue",
  pfcol = rgb(0, 0, 1, 0.3),
  plwd = 2,
  cglcol = "grey",
  cglty = 1,
  axislabcol = "black",
  vlcex = 0.9
)


## Park (Assets2C)

Plymouth_park<-Plymouth_CLS_assets |> select(Assets2C_a,Assets2C_b,Assets2C_c,Assets2C_d)

Plymouth_park_total<-colSums(Plymouth_park)

park_Plymouth<-cbind(distance,Plymouth_park_total) |> as.data.frame()

park_Plymouth$Plymouth_park_total<-as.numeric(park_Plymouth$Plymouth_park_total)

ggplot(park_Plymouth,aes(x=distance,y=Plymouth_park_total))+geom_bar(stat="identity")+
  coord_flip()

park_Plymouth<-park_Plymouth |> 
  mutate(Percentage = (Plymouth_park_total/sum(Plymouth_park_total))*100)


park_plymouth_scores <- data.frame(
  Minutes = 83.641536,
  Still_local = 12.091038,
  None = 1.991465,
  Not_sure = 2.275960
)

radar_data3 <- rbind(
  c(100, 100, 100, 100),
  c(0, 0, 0, 0),
  park_plymouth_scores
)

colnames(radar_data3) <- c(
  "15-20 mintues",
  "Further but still local",
  "None",
  "Not sure/Don't know"
)

radarchart(
  radar_data3,
  axistype = 1,
  pcol = "blue",
  pfcol = rgb(0, 0, 1, 0.3),
  plwd = 2,
  cglcol = "grey",
  cglty = 1,
  axislabcol = "black",
  vlcex = 0.9
)


## Library (Assets2D)

Plymouth_library<-Plymouth_CLS_assets |> select(Assets2D_a,Assets2D_b,Assets2D_c,Assets2D_d)

Plymouth_library_total<-colSums(Plymouth_library)

library_Plymouth<-cbind(distance,Plymouth_library_total) |> as.data.frame()

library_Plymouth$Plymouth_library_total<-as.numeric(library_Plymouth$Plymouth_library_total)

ggplot(library_Plymouth,aes(x=distance,y=Plymouth_library_total))+geom_bar(stat="identity")+
  coord_flip()

library_Plymouth<-library_Plymouth |> 
  mutate(Percentage = (Plymouth_library_total/sum(Plymouth_library_total))*100)

library_plymouth_scores <- data.frame(
  Minutes =  48.961424,
  Still_local = 28.783383,
  None = 14.540059,
  Not_sure = 7.715134
)

radar_data4 <- rbind(
  c(100, 100, 100, 100),
  c(0, 0, 0, 0),
  library_plymouth_scores
)

colnames(radar_data4) <- c(
  "15-20 mintues",
  "Further but still local",
  "None",
  "Not sure/Don't know"
)

radarchart(
  radar_data4,
  axistype = 1,
  pcol = "blue",
  pfcol = rgb(0, 0, 1, 0.3),
  plwd = 2,
  cglcol = "grey",
  cglty = 1,
  axislabcol = "black",
  vlcex = 0.9
)



## Restaurant/cafe (Assets2E)

Plymouth_cafe<-Plymouth_CLS_assets |> select(Assets2E_a,Assets2E_b,Assets2E_c,Assets2E_d)

Plymouth_cafe_total<-colSums(Plymouth_cafe)

cafe_Plymouth<-cbind(distance,Plymouth_cafe_total) |> as.data.frame()

cafe_Plymouth$Plymouth_cafe_total<-as.numeric(cafe_Plymouth$Plymouth_cafe_total)

ggplot(cafe_Plymouth,aes(x=distance,y=Plymouth_cafe_total))+geom_bar(stat="identity")+
  coord_flip()

cafe_Plymouth<-cafe_Plymouth |> 
  mutate(Percentage = (Plymouth_cafe_total/sum(Plymouth_cafe_total))*100)

cafe_plymouth_scores <- data.frame(
  Minutes = 63.936782,
  Still_local = 21.551724,
  None = 10.919540,
  Not_sure = 3.591954
)

radar_data5 <- rbind(
  c(100, 100, 100, 100),
  c(0, 0, 0, 0),
  cafe_plymouth_scores
)

colnames(radar_data5) <- c(
  "15-20 mintues",
  "Further but still local",
  "None",
  "Not sure/Don't know"
)

radarchart(
  radar_data5,
  axistype = 1,
  pcol = "blue",
  pfcol = rgb(0, 0, 1, 0.3),
  plwd = 2,
  cglcol = "grey",
  cglty = 1,
  axislabcol = "black",
  vlcex = 0.9
)


## Community centre/hall (Assets2F)

Plymouth_communityhall<-Plymouth_CLS_assets |> select(Assets2F_a,Assets2F_b,Assets2F_c,Assets2F_d)

Plymouth_communityhall_total<-colSums(Plymouth_communityhall)

communityhall_Plymouth<-cbind(distance,Plymouth_communityhall_total) |> as.data.frame()

communityhall_Plymouth$Plymouth_communityhall_total<-as.numeric(communityhall_Plymouth$Plymouth_communityhall_total)

ggplot(communityhall_Plymouth,aes(x=distance,y=Plymouth_communityhall_total))+geom_bar(stat="identity")+
  coord_flip()

communityhall_Plymouth<-communityhall_Plymouth |> 
  mutate(Percentage = (Plymouth_communityhall_total/sum(Plymouth_communityhall_total))*100)

communityhall_plymouth_scores <- data.frame(
  Minutes = 58.918129,
  Still_local = 16.081871,
  None = 9.210526,
  Not_sure = 15.789474
)

radar_data6 <- rbind(
  c(100, 100, 100, 100),
  c(0, 0, 0, 0),
  communityhall_plymouth_scores
)

colnames(radar_data6) <- c(
  "15-20 mintues",
  "Further but still local",
  "None",
  "Not sure/Don't know"
)

radarchart(
  radar_data6,
  axistype = 1,
  pcol = "blue",
  pfcol = rgb(0, 0, 1, 0.3),
  plwd = 2,
  cglcol = "grey",
  cglty = 1,
  axislabcol = "black",
  vlcex = 0.9
)


## Sports facilities (Assets2G)

Plymouth_sportsfacilities<-Plymouth_CLS_assets |> select(Assets2G_a,Assets2G_b,Assets2G_c,Assets2G_d)

Plymouth_sportsfacilities_total<-colSums(Plymouth_sportsfacilities)

sportsfacilities_Plymouth<-cbind(distance,Plymouth_sportsfacilities_total) |> as.data.frame()

sportsfacilities_Plymouth$Plymouth_sportsfacilities_total<-as.numeric(sportsfacilities_Plymouth$Plymouth_sportsfacilities_total)

ggplot(sportsfacilities_Plymouth,aes(x=distance,y=Plymouth_sportsfacilities_total))+geom_bar(stat="identity")+
  coord_flip()
sportsfacilities_Plymouth<-sportsfacilities_Plymouth |> 
  mutate(Percentage = (Plymouth_sportsfacilities_total/sum(Plymouth_sportsfacilities_total))*100)

sportsfacilities_plymouth_scores <- data.frame(
  Minutes = 48.057554,
  Still_local = 30.215827,
  None = 14.820144,
  Not_sure = 6.906475
)

radar_data7 <- rbind(
  c(100, 100, 100, 100),
  c(0, 0, 0, 0),
  sportsfacilities_plymouth_scores
)

colnames(radar_data7) <- c(
  "15-20 mintues",
  "Further but still local",
  "None",
  "Not sure/Don't know"
)

radarchart(
  radar_data7,
  axistype = 1,
  pcol = "blue",
  pfcol = rgb(0, 0, 1, 0.3),
  plwd = 2,
  cglcol = "grey",
  cglty = 1,
  axislabcol = "black",
  vlcex = 0.9
)


## Health centre/GP (Assets2H)

Plymouth_gp<-Plymouth_CLS_assets |> select(Assets2H_a,Assets2H_b,Assets2H_c,Assets2H_d)

Plymouth_gp_total<-colSums(Plymouth_gp)

gp_Plymouth<-cbind(distance,Plymouth_gp_total) |> as.data.frame()

gp_Plymouth$Plymouth_gp_total<-as.numeric(gp_Plymouth$Plymouth_gp_total)

ggplot(gp_Plymouth,aes(x=distance,y=Plymouth_gp_total))+geom_bar(stat="identity")+
  coord_flip()

gp_Plymouth<-gp_Plymouth |> 
  mutate(Percentage = (Plymouth_gp_total/sum(Plymouth_gp_total))*100)

gp_plymouth_scores <- data.frame(
  Minutes = 71.875000,
  Still_local = 20.454545,
  None = 4.403409,
  Not_sure = 3.267045
)

radar_data8 <- rbind(
  c(100, 100, 100, 100),
  c(0, 0, 0, 0),
  gp_plymouth_scores
)

colnames(radar_data8) <- c(
  "15-20 mintues",
  "Further but still local",
  "None",
  "Not sure/Don't know"
)

radarchart(
  radar_data8,
  axistype = 1,
  pcol = "blue",
  pfcol = rgb(0, 0, 1, 0.3),
  plwd = 2,
  cglcol = "grey",
  cglty = 1,
  axislabcol = "black",
  vlcex = 0.9
)

## Chemist or pharmacy (Assets2I)

Plymouth_chemist<-Plymouth_CLS_assets |> select(Assets2I_a,Assets2I_b,Assets2I_c,Assets2I_d)

Plymouth_chemist_total<-colSums(Plymouth_chemist)

chemist_Plymouth<-cbind(distance,Plymouth_chemist_total) |> as.data.frame()

chemist_Plymouth$Plymouth_chemist_total<-as.numeric(chemist_Plymouth$Plymouth_chemist_total)

ggplot(chemist_Plymouth,aes(x=distance,y=Plymouth_chemist_total))+geom_bar(stat="identity")+
  coord_flip()

chemist_Plymouth<-chemist_Plymouth |> 
  mutate(Percentage = (Plymouth_chemist_total/sum(Plymouth_chemist_total))*100)

chemist_plymouth_scores <- data.frame(
  Minutes = 73.714286,
  Still_local = 18.714286,
  None =  4.428571,
  Not_sure = 3.142857
)

radar_data9 <- rbind(
  c(100, 100, 100, 100),
  c(0, 0, 0, 0),
  chemist_plymouth_scores
)

colnames(radar_data9) <- c(
  "15-20 mintues",
  "Further but still local",
  "None",
  "Not sure/Don't know"
)

radarchart(
  radar_data9,
  axistype = 1,
  pcol = "blue",
  pfcol = rgb(0, 0, 1, 0.3),
  plwd = 2,
  cglcol = "grey",
  cglty = 1,
  axislabcol = "black",
  vlcex = 0.9
)

## Post Office (Assets2J)

Plymouth_postoffice<-Plymouth_CLS_assets |> select(Assets2J_a,Assets2J_b,Assets2J_c,Assets2J_d)

Plymouth_postoffice_total<-colSums(Plymouth_postoffice)

postoffice_Plymouth<-cbind(distance,Plymouth_postoffice_total) |> as.data.frame()

postoffice_Plymouth$Plymouth_postoffice_total<-as.numeric(postoffice_Plymouth$Plymouth_postoffice_total)

ggplot(postoffice_Plymouth,aes(x=distance,y=Plymouth_postoffice_total))+geom_bar(stat="identity")+
  coord_flip()

postoffice_Plymouth<-postoffice_Plymouth |> 
  mutate(Percentage = (Plymouth_postoffice_total/sum(Plymouth_postoffice_total))*100)

postoffice_plymouth_scores <- data.frame(
  Minutes = 67.194245,
  Still_local = 21.294964,
  None = 8.345324,
  Not_sure = 3.165468
)

radar_data10 <- rbind(
  c(100, 100, 100, 100),
  c(0, 0, 0, 0),
  postoffice_plymouth_scores
)

colnames(radar_data10) <- c(
  "15-20 mintues",
  "Further but still local",
  "None",
  "Not sure/Don't know"
)

radarchart(
  radar_data10,
  axistype = 1,
  pcol = "blue",
  pfcol = rgb(0, 0, 1, 0.3),
  plwd = 2,
  cglcol = "grey",
  cglty = 1,
  axislabcol = "black",
  vlcex = 0.9
)


## Place of worship for my faith or religion, such as a church, mosque, temple (Assets2K)

Plymouth_worship<-Plymouth_CLS_assets |> select(Assets2K_a,Assets2K_b,Assets2K_c,Assets2K_d)

Plymouth_worship_total<-colSums(Plymouth_worship)

worship_Plymouth<-cbind(distance,Plymouth_worship_total) |> as.data.frame()

worship_Plymouth$Plymouth_worship_total<-as.numeric(worship_Plymouth$Plymouth_worship_total)

ggplot(worship_Plymouth,aes(x=distance,y=Plymouth_worship_total))+geom_bar(stat="identity")+
  coord_flip()

worship_Plymouth<-worship_Plymouth |> 
  mutate(Percentage = (Plymouth_worship_total/sum(Plymouth_worship_total))*100)

worship_plymouth_scores <- data.frame(
  Minutes = 71.564885,
  Still_local = 16.030534,
  None = 5.725191,
  Not_sure = 6.679389
)

radar_data11 <- rbind(
  c(100, 100, 100, 100),
  c(0, 0, 0, 0),
  worship_plymouth_scores
)

colnames(radar_data11) <- c(
  "15-20 mintues",
  "Further but still local",
  "None",
  "Not sure/Don't know"
)

radarchart(
  radar_data11,
  axistype = 1,
  pcol = "blue",
  pfcol = rgb(0, 0, 1, 0.3),
  plwd = 2,
  cglcol = "grey",
  cglty = 1,
  axislabcol = "black",
  vlcex = 0.9
)


## Public transport links (Assets2L)	

Plymouth_publictransport<-Plymouth_CLS_assets |> select(Assets2L_a,Assets2L_b,Assets2L_c,Assets2L_d)

Plymouth_publictransport_total<-colSums(Plymouth_publictransport)

publictransport_Plymouth<-cbind(distance,Plymouth_publictransport_total) |> as.data.frame()

publictransport_Plymouth$Plymouth_publictransport_total<-as.numeric(publictransport_Plymouth$Plymouth_publictransport_total)

ggplot(publictransport_Plymouth,aes(x=distance,y=Plymouth_publictransport_total))+geom_bar(stat="identity")+
  coord_flip()

publictransport_Plymouth<-publictransport_Plymouth |> 
  mutate(Percentage = (Plymouth_publictransport_total/sum(Plymouth_publictransport_total))*100)

publictransport_plymouth_scores <- data.frame(
  Minutes = 87.2881356,
  Still_local = 10.1694915,
  None = 0.5649718,
  Not_sure = 1.9774011
)

radar_data12 <- rbind(
  c(100, 100, 100, 100),
  c(0, 0, 0, 0),
  publictransport_plymouth_scores
)

colnames(radar_data12) <- c(
  "15-20 mintues",
  "Further but still local",
  "None",
  "Not sure/Don't know"
)

radarchart(
  radar_data12,
  axistype = 1,
  pcol = "blue",
  pfcol = rgb(0, 0, 1, 0.3),
  plwd = 2,
  cglcol = "grey",
  cglty = 1,
  axislabcol = "black",
  vlcex = 0.9
)