##########################################################################################################################
###                                                                                                                    ###
###                                                                                                                    ###
###                                  Census Data Analysis for Data Explainer Pack                                      ###
###                                                                                                                    ###
###                                                                                                                    ###
##########################################################################################################################

## Created by: LA
## Creation date: 07/08/2026

## Modified on:


## Load in the required libraries
library(tidyverse)
library(rayshader)

## Read in the data
age_data<-read.csv("../Census Data Download/Age.csv")
disability_data<-read.csv("../Census Data Download/Disability.csv")
education_data<-read.csv("../Census Data Download/Education.csv")
ethnicity_data<-read.csv("../Census Data Download/Ethnicity.csv")

age_sex_data<-read.csv("../Census Data Download/Age_Sex.csv")
disability_sex_data<-read.csv("../Census Data Download/Disability_Sex.csv")
education_sex_data<-read.csv("../Census Data Download/Education_Sex.csv")
ethnicity_sex_data<-read.csv("../Census Data Download/Ethnicity_Sex.csv")

plymouth_lsoas<-c("Plymouth 034A", "Plymouth 034B", "Plymouth 034C", "Plymouth 034D", "Plymouth 034E")

hastings_lsoas<-c("Hastings 009A", "Hastings 009B", "Hastings 009C", "Hastings 009D")

age_hastings<-age_data |> filter(Lower.layer.Super.Output.Areas %in% hastings_lsoas)

age_sex_hastings<-age_sex_data |> filter(Lower.layer.Super.Output.Areas %in% hastings_lsoas)

disability_hastings<-disability_data |> filter(Lower.layer.Super.Output.Areas %in% hastings_lsoas)

disability_sex_hastings<-disability_sex_data |> filter(Lower.layer.Super.Output.Areas %in% hastings_lsoas)

education_sex_hastings<-education_sex_data |> filter(Lower.layer.Super.Output.Areas %in% hastings_lsoas)

ethnicity_sex_hastings<-ethnicity_sex_data |> filter(Lower.layer.Super.Output.Areas %in% hastings_lsoas)


age_hastings_total<-age_hastings |> group_by(Age..6.categories.) |> summarise(total=sum(Observation)) |> 
  mutate(proportion=(total/6876)*100)


age_sex_hastings_total<-age_sex_hastings |> group_by(Sex..2.categories.,Age..6.categories.) |> 
  summarise(total=sum(Observation))

ggplot(age_hastings_total,aes(x=Age..6.categories.,y=total))+geom_bar(stat = "identity")

ggplot(age_sex_hastings_total,aes(x=Age..6.categories.,y=total,fill=Sex..2.categories.))+
  geom_bar(stat = "identity",position = position_dodge(width = 0.9))

# Order by percentage
age_hastings_total$Age..6.categories. <- factor(
  age_hastings_total$Age..6.categories.,
  levels = age_hastings_total$Age..6.categories.[order(age_hastings_total$proportion)]
)

ggplot(age_hastings_total, aes(x = proportion, y = Age..6.categories.)) +
  geom_segment(aes(x = 0, xend = proportion,
                   y = Age..6.categories., yend = Age..6.categories.),
               colour = "grey70",
               linewidth = 1) +
  geom_point(size = 5, colour = "#0072B2") +
  geom_text(aes(label = sprintf("%.1f%%", proportion)),
            hjust = -0.3, size = 4) +
  scale_x_continuous(
    limits = c(0, 25),
    expand = expansion(mult = c(0, 0.1))
  ) +
  labs(
    title = "Age Group Profile",
    subtitle = "Percentage of population by age group",
    x = "Percentage (%)",
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )


# Order by percentage
age_sex_hastings_total$Age..6.categories. <- factor(
  age_sex_hastings_total$Age..6.categories.,
  levels = age_hastings_total$Age..6.categories.[order(age_hastings_total$proportion)]
)

ggplot(age_sex_hastings_total, aes(x = total, y = Age..6.categories.,fill = Sex..2.categories.)) +
  geom_segment(aes(x = 0, xend = total,
                   y = Age..6.categories., yend = Age..6.categories.),
               colour = "grey70",
               linewidth = 1) +
  geom_point(size = 5, colour = "#0072B2") +
  geom_text(aes(label = total),
            hjust = -0.3, size = 4) +
  scale_x_continuous(
    limits = c(0, 1000),
    expand = expansion(mult = c(0, 0.1))
  ) +
  labs(
    title = "Age Group Profile",
    subtitle = "Percentage of population by age group",
    x = "Percentage (%)",
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )
