##########################################################################################################################
###                                                                                                                    ###
###                                                                                                                    ###
###                                                   IMD Data Analysis                                                ###
###                                                                                                                    ###
###                                                                                                                    ###
##########################################################################################################################

## Created by: LA
## Creation date: 12/08/2026

## Modified on: 

## Load in the required libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(OpenStreetMap)
library(osmdata)
library(sp)
library(sf)
library(tmap)
library(rJava)
library(maptools)
library(devtools)

## Load in the data

hastings_shape<-read_sf( "../Shapefiles/Hastings_LSOAs.shp")

IMD<-read.csv("File_7_IoD2025_All_Ranks_Scores_Deciles_Population_Denominators.csv")

## Filter the IMD data to only contain the LSOAs of interest

plymouth_lsoas<-c("Plymouth 034A", "Plymouth 034B", "Plymouth 034C", "Plymouth 034D", "Plymouth 034E")

hastings_lsoas<-c("Hastings 009A", "Hastings 009B", "Hastings 009C", "Hastings 009D")

IMD_hastings<-IMD |> filter(LSOA.name..2021. %in% hastings_lsoas)

IMD_plymouth<-IMD |> filter(LSOA.name..2021. %in% plymouth_lsoas)

## Save data for the explainer packs

write.csv(IMD_hastings,"../Data for Explainer Pack/hastings_IMD.csv",row.names = FALSE)
write.csv(IMD_plymouth,"../Data for Explainer Pack/plymouth_IMD.csv",row.names = FALSE)


hastings_shape<-cbind(hastings_shape,IMD_hastings)

qtm(hastings_shape,fill ="Health.Deprivation.and.Disability.Rank..where.1.is.most.deprived.",
    fill.title="Index of Multiple Deprivation (Rank)")


ggplot(hastings_shape) + geom_sf(aes(fill = Health.Deprivation.and.Disability.Rank..where.1.is.most.deprived.)) +
  scale_fill_viridis_c() +theme_minimal()

# Convert from BNG to WGS84
hastings_shape <- st_transform(hastings_shape, 4326)

pal <- colorNumeric(
  palette = "viridis",
  domain = hastings_shape$Health.Deprivation.and.Disability.Rank..where.1.is.most.deprived.
)

leaflet(hastings_shape) |>
  addTiles() |>
  addPolygons(
    fillColor = ~pal(Health.Deprivation.and.Disability.Rank..where.1.is.most.deprived.),
    fillOpacity = 0.7,
    color = "white",
    weight = 1,
    popup = ~paste(
      LSOA.name..2021.,
      "<br>IMD:", Health.Deprivation.and.Disability.Rank..where.1.is.most.deprived.
    )
  ) |>
  addLegend(
    pal = pal,
    values = ~Health.Deprivation.and.Disability.Rank..where.1.is.most.deprived.,
    title = "IMD Score"
  )
