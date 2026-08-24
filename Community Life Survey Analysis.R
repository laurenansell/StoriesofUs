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

## Load in the required libraries
library(tidyverse)
library(haven)
library(likert)
library(igraph)
library(ggraph)


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

ggplot(generalshop_hastings,aes(x=distance,y=hastings_generalshop_total))+geom_bar(stat="identity")+
  coord_flip()
  

## Pub/bar (Assets2B)

hastings_bar<-Hastings_CLS_assets |> select(Assets2B_a,Assets2B_b,Assets2B_c,Assets2B_d)

hastings_bar_total<-colSums(hastings_bar)

bar_hastings<-cbind(distance,hastings_bar_total) |> as.data.frame()

bar_hastings$hastings_bar_total<-as.numeric(bar_hastings$hastings_bar_total)

ggplot(bar_hastings,aes(x=distance,y=hastings_bar_total))+geom_bar(stat="identity")+
  coord_flip()

## Park (Assets2C)

hastings_park<-Hastings_CLS_assets |> select(Assets2C_a,Assets2C_b,Assets2C_c,Assets2C_d)

hastings_park_total<-colSums(hastings_park)

park_hastings<-cbind(distance,hastings_park_total) |> as.data.frame()

park_hastings$hastings_park_total<-as.numeric(park_hastings$hastings_park_total)

ggplot(park_hastings,aes(x=distance,y=hastings_park_total))+geom_bar(stat="identity")+
  coord_flip()

## Library (Assets2D)

hastings_library<-Hastings_CLS_assets |> select(Assets2D_a,Assets2D_b,Assets2D_c,Assets2D_d)

hastings_library_total<-colSums(hastings_library)

library_hastings<-cbind(distance,hastings_library_total) |> as.data.frame()

library_hastings$hastings_library_total<-as.numeric(library_hastings$hastings_library_total)

ggplot(library_hastings,aes(x=distance,y=hastings_library_total))+geom_bar(stat="identity")+
  coord_flip()

## Restaurant/cafe (Assets2E)

hastings_cafe<-Hastings_CLS_assets |> select(Assets2E_a,Assets2E_b,Assets2E_c,Assets2E_d)

hastings_cafe_total<-colSums(hastings_cafe)

cafe_hastings<-cbind(distance,hastings_cafe_total) |> as.data.frame()

cafe_hastings$hastings_cafe_total<-as.numeric(cafe_hastings$hastings_cafe_total)

ggplot(cafe_hastings,aes(x=distance,y=hastings_cafe_total))+geom_bar(stat="identity")+
  coord_flip()

## Community centre/hall (Assets2F)

hastings_communityhall<-Hastings_CLS_assets |> select(Assets2F_a,Assets2F_b,Assets2F_c,Assets2F_d)

hastings_communityhall_total<-colSums(hastings_communityhall)

communityhall_hastings<-cbind(distance,hastings_communityhall_total) |> as.data.frame()

communityhall_hastings$hastings_communityhall_total<-as.numeric(communityhall_hastings$hastings_communityhall_total)

ggplot(communityhall_hastings,aes(x=distance,y=hastings_communityhall_total))+geom_bar(stat="identity")+
  coord_flip()

## Sports facilities (Assets2G)

hastings_sportsfacilities<-Hastings_CLS_assets |> select(Assets2G_a,Assets2G_b,Assets2G_c,Assets2G_d)

hastings_sportsfacilities_total<-colSums(hastings_sportsfacilities)

sportsfacilities_hastings<-cbind(distance,hastings_sportsfacilities_total) |> as.data.frame()

sportsfacilities_hastings$hastings_sportsfacilities_total<-as.numeric(sportsfacilities_hastings$hastings_sportsfacilities_total)

ggplot(sportsfacilities_hastings,aes(x=distance,y=hastings_sportsfacilities_total))+geom_bar(stat="identity")+
  coord_flip()

## Health centre/GP (Assets2H)

hastings_gp<-Hastings_CLS_assets |> select(Assets2H_a,Assets2H_b,Assets2H_c,Assets2H_d)

hastings_gp_total<-colSums(hastings_gp)

gp_hastings<-cbind(distance,hastings_gp_total) |> as.data.frame()

gp_hastings$hastings_gp_total<-as.numeric(gp_hastings$hastings_gp_total)

ggplot(gp_hastings,aes(x=distance,y=hastings_gp_total))+geom_bar(stat="identity")+
  coord_flip()

## Chemist or pharmacy (Assets2I)

hastings_chemist<-Hastings_CLS_assets |> select(Assets2I_a,Assets2I_b,Assets2I_c,Assets2I_d)

hastings_chemist_total<-colSums(hastings_chemist)

chemist_hastings<-cbind(distance,hastings_chemist_total) |> as.data.frame()

chemist_hastings$hastings_chemist_total<-as.numeric(chemist_hastings$hastings_chemist_total)

ggplot(chemist_hastings,aes(x=distance,y=hastings_chemist_total))+geom_bar(stat="identity")+
  coord_flip()

## Post Office (Assets2J)

hastings_postoffice<-Hastings_CLS_assets |> select(Assets2J_a,Assets2J_b,Assets2J_c,Assets2J_d)

hastings_postoffice_total<-colSums(hastings_postoffice)

postoffice_hastings<-cbind(distance,hastings_postoffice_total) |> as.data.frame()

postoffice_hastings$hastings_postoffice_total<-as.numeric(postoffice_hastings$hastings_postoffice_total)

ggplot(postoffice_hastings,aes(x=distance,y=hastings_postoffice_total))+geom_bar(stat="identity")+
  coord_flip()

## Place of worship for my faith or religion, such as a church, mosque, temple (Assets2K)

hastings_worship<-Hastings_CLS_assets |> select(Assets2K_a,Assets2K_b,Assets2K_c,Assets2K_d)

hastings_worship_total<-colSums(hastings_worship)

worship_hastings<-cbind(distance,hastings_worship_total) |> as.data.frame()

worship_hastings$hastings_worship_total<-as.numeric(worship_hastings$hastings_worship_total)

ggplot(worship_hastings,aes(x=distance,y=hastings_worship_total))+geom_bar(stat="identity")+
  coord_flip()

## Public transport links (Assets2L)	

hastings_publictransport<-Hastings_CLS_assets |> select(Assets2L_a,Assets2L_b,Assets2L_c,Assets2L_d)

hastings_publictransport_total<-colSums(hastings_publictransport)

publictransport_hastings<-cbind(distance,hastings_publictransport_total) |> as.data.frame()

publictransport_hastings$hastings_publictransport_total<-as.numeric(publictransport_hastings$hastings_publictransport_total)

ggplot(publictransport_hastings,aes(x=distance,y=hastings_publictransport_total))+geom_bar(stat="identity")+
  coord_flip()

