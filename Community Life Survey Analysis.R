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































