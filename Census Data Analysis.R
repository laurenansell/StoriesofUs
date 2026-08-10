##########################################################################################################################
###                                                                                                                    ###
###                                                                                                                    ###
###                                  Census Data Analysis for Data Explainer Pack                                      ###
###                                                                                                                    ###
###                                                                                                                    ###
##########################################################################################################################

## Created by: LA
## Creation date: 07/08/2026

## Modified on: 10/07/2026


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

education_hastings<-education_data |> filter(Lower.layer.Super.Output.Areas %in% hastings_lsoas)

education_sex_hastings<-education_sex_data |> filter(Lower.layer.Super.Output.Areas %in% hastings_lsoas)

ethnicity_hastings<-ethnicity_data |> filter(Lower.layer.Super.Output.Areas %in% hastings_lsoas)

ethnicity_sex_hastings<-ethnicity_sex_data |> filter(Lower.layer.Super.Output.Areas %in% hastings_lsoas)



## Age


age_hastings_total<-age_hastings |> group_by(Age..6.categories.) |> summarise(total=sum(Observation)) |> 
  mutate(proportion=(total/6876)*100)


age_sex_hastings_total<-age_sex_hastings |> group_by(Sex..2.categories.,Age..6.categories.) |> 
  summarise(total=sum(Observation))

ggplot(age_hastings_total,aes(x=Age..6.categories.,y=total))+geom_bar(stat = "identity")

ggplot(age_sex_hastings_total,aes(x=Age..6.categories.,y=total,fill=Sex..2.categories.))+
  geom_bar(stat = "identity",position = position_dodge(width = 0.9))

## Order by percentage
age_hastings_total$Age..6.categories_f <- factor(
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


## Order by percentage
age_sex_hastings_total$Age..6.categories. <- factor(
  age_sex_hastings_total$Age..6.categories.,
  levels = age_hastings_total$Age..6.categories.[order(age_hastings_total$proportion)]
)

ggplot(age_sex_hastings_total, aes(x = total, y = Age..6.categories.,colour = Sex..2.categories.)) +
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


age_sex_hastings_total$Age..6.categories_f <- factor(
  age_sex_hastings_total$Age..6.categories.,
  levels = age_hastings_total$Age..6.categories.[order(age_hastings_total$Age..6.categories.)]
)

# Order by age

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


ggplot(age_sex_hastings_total)+
  geom_linerange(aes(x = Age..6.categories., ymin = 0, ymax = total, colour = Sex..2.categories.), 
                 position = position_dodge(width = 1))+
  geom_point(aes(x = Age..6.categories., y = total, colour = Sex..2.categories.),
             position = position_dodge(width = 1))+
  coord_flip()


## Ethnicity

ethnicity_hastings_total<-ethnicity_hastings |> group_by(Ethnic.group..20.categories.) |> summarise(total=sum(Observation)) |> 
  mutate(proportion=(total/6869)*100)


ethnicity_sex_hastings_total<-ethnicity_sex_hastings |> group_by(Sex..2.categories.,Ethnic.group..20.categories.) |> 
  summarise(total=sum(Observation))

ggplot(ethnicity_hastings_total,aes(x=Ethnic.group..20.categories.,y=total))+geom_bar(stat = "identity")

ggplot(ethnicity_sex_hastings_total,aes(x=Ethnic.group..20.categories.,y=total,fill=Sex..2.categories.))+
  geom_bar(stat = "identity",position = position_dodge(width = 0.9))

# Order by percentage
ethnicity_hastings_total$ethnicity_f <- factor(
  ethnicity_hastings_total$Ethnic.group..20.categories.,
  levels = ethnicity_hastings_total$Ethnic.group..20.categories.[order(ethnicity_hastings_total$proportion)]
)

ggplot(ethnicity_hastings_total, aes(x = proportion, y = ethnicity_f)) +
  geom_segment(aes(x = 0, xend = proportion,
                   y = ethnicity_f, yend = ethnicity_f),
               colour = "grey70",
               linewidth = 1) +
  geom_point(size = 5, colour = "#0072B2") +
  geom_text(aes(label = sprintf("%.1f%%", proportion)),
            hjust = -0.3, size = 4) +
  scale_x_continuous(
    limits = c(0, 100),
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


ggplot(ethnicity_sex_hastings_total)+
  geom_linerange(aes(x = Ethnic.group..20.categories., ymin = 0, ymax = total, colour = Sex..2.categories.), 
                 position = position_dodge(width = 1))+
  geom_point(aes(x = Ethnic.group..20.categories., y = total, colour = Sex..2.categories.),
             position = position_dodge(width = 1))+
  coord_flip()


## Education

education_hastings_total<-education_hastings |> group_by(Highest.level.of.qualification..7.categories.) |> 
  summarise(total=sum(Observation)) |> 
  mutate(proportion=(total/6874)*100)


education_sex_hastings_total<-education_sex_hastings |> group_by(Sex..2.categories.,Highest.level.of.qualification..7.categories.) |> 
  summarise(total=sum(Observation))

ggplot(education_hastings_total,aes(x=Highest.level.of.qualification..7.categories.,y=total))+geom_bar(stat = "identity")

ggplot(education_sex_hastings_total,aes(x=Highest.level.of.qualification..7.categories.,y=total,fill=Sex..2.categories.))+
  geom_bar(stat = "identity",position = position_dodge(width = 0.9))

# Order by percentage
education_hastings_total$education_f <- factor(
  education_hastings_total$Highest.level.of.qualification..7.categories.,
  levels = education_hastings_total$Highest.level.of.qualification..7.categories.[order(education_hastings_total$proportion)]
)

ggplot(education_hastings_total, aes(x = proportion, y = education_f)) +
  geom_segment(aes(x = 0, xend = proportion,
                   y = education_f, yend = education_f),
               colour = "grey70",
               linewidth = 1) +
  geom_point(size = 5, colour = "#0072B2") +
  geom_text(aes(label = sprintf("%.1f%%", proportion)),
            hjust = -0.3, size = 4) +
  scale_x_continuous(
    limits = c(0, 100),
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


ggplot(education_sex_hastings_total)+
  geom_linerange(aes(x = Highest.level.of.qualification..7.categories., ymin = 0, ymax = total, colour = Sex..2.categories.), 
                 position = position_dodge(width = 1))+
  geom_point(aes(x = Highest.level.of.qualification..7.categories., y = total, colour = Sex..2.categories.),
             position = position_dodge(width = 1))+
  coord_flip()
 
## Note: shorten the names of the education labels to improve the plot for the report

