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
##              12/08/2026


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


## Save data for the explainer packs


write.csv(age_hastings,"../Data for Explainer Pack/hastings_age.csv",row.names = FALSE)
write.csv(age_sex_hastings,"../Data for Explainer Pack/hastings_age_sex.csv",row.names = FALSE)

write.csv(disability_hastings,"../Data for Explainer Pack/hastings_disability.csv",row.names = FALSE)
write.csv(disability_sex_hastings,"../Data for Explainer Pack/hastings_disability_sex.csv",row.names = FALSE)

write.csv(education_hastings,"../Data for Explainer Pack/hastings_education.csv",row.names = FALSE)
write.csv(education_sex_hastings,"../Data for Explainer Pack/hastings_education_sex.csv",row.names = FALSE)

write.csv(ethnicity_hastings,"../Data for Explainer Pack/hastings_ethnicity.csv",row.names = FALSE)
write.csv(ethnicity_sex_hastings,"../Data for Explainer Pack/hastings_ethnicity_sex.csv",row.names = FALSE)

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


## Disability

disability_hastings_total<-disability_hastings |> group_by(Disability..3.categories.) |> 
  summarise(total=sum(Observation)) |> 
  mutate(proportion=(total/6873)*100)


disability_sex_hastings_total<-disability_sex_hastings |> group_by(Sex..2.categories.,Disability..3.categories.) |> 
  summarise(total=sum(Observation))

ggplot(disability_hastings_total,aes(x=Disability..3.categories.,y=total))+geom_bar(stat = "identity")

ggplot(disability_sex_hastings_total,aes(x=Disability..3.categories.,y=total,fill=Sex..2.categories.))+
  geom_bar(stat = "identity",position = position_dodge(width = 0.9))

# Order by percentage
disability_hastings_total$disability_f <- factor(
  disability_hastings_total$Disability..3.categories.,
  levels = disability_hastings_total$Disability..3.categories.[order(disability_hastings_total$proportion)]
)

ggplot(disability_hastings_total, aes(x = proportion, y = disability_f)) +
  geom_segment(aes(x = 0, xend = proportion,
                   y = disability_f, yend = disability_f),
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


ggplot(disability_sex_hastings_total)+
  geom_linerange(aes(x = Disability..3.categories., ymin = 0, ymax = total, colour = Sex..2.categories.), 
                 position = position_dodge(width = 1))+
  geom_point(aes(x = Disability..3.categories., y = total, colour = Sex..2.categories.),
             position = position_dodge(width = 1))+
  coord_flip()


## dummy data for age violin plot:

## Age for all persons

## 50-64: 22%
## 35-49: 21.5%
## 25-34: 16.1%
## 65+: 15.4%
## <15: 14.2%
## 16-24: 10.8%

set.seed(2)
age_50_64<-sample(50:64,220, replace = TRUE)
age_35_49<-sample(35:49,215,replace = TRUE)
age_25_34<-sample(25:34,161,replace = TRUE)
age_65_over<-sample(65:90,154,replace = TRUE)
age_15_less<-sample(1:15,142,replace = TRUE)
age_16_24<-sample(16:24,108,replace = TRUE)


age<-c(age_15_less,age_16_24,age_25_34,age_35_49,age_50_64,age_65_over) |> as.data.frame()

names(age) <- c("age")

ggplot(age,aes(x="Age",y=age))+geom_violin()


## Age for split by sex

## Male

## 50-64: 22.2%
## 35-49: 22.1%
## 25-34: 16.3%
## 65+: 13.9%
## <15: 14.9%
## 16-24: 10.7%

## Female

## 50-64: 21.9%
## 35-49: 20.9%
## 25-34: 15.8%
## 65+: 17%
## <15: 13.5%
## 16-24: 11%


set.seed(2)
age_50_64_m<-sample(50:64,220, replace = TRUE)
age_35_49_m<-sample(35:49,221,replace = TRUE)
age_25_34_m<-sample(25:34,163,replace = TRUE)
age_65_over_m<-sample(65:90,139,replace = TRUE)
age_15_less_m<-sample(1:15,149,replace = TRUE)
age_16_24_m<-sample(16:24,107,replace = TRUE)

age_50_64_f<-sample(50:64,219, replace = TRUE)
age_35_49_f<-sample(35:49,209,replace = TRUE)
age_25_34_f<-sample(25:34,158,replace = TRUE)
age_65_over_f<-sample(65:90,170,replace = TRUE)
age_15_less_f<-sample(1:15,135,replace = TRUE)
age_16_24_f<-sample(16:24,110,replace = TRUE)



age_sex<-c(age_15_less_m,age_16_24_m,age_25_34_m,age_35_49_m,age_50_64_m,age_65_over_m,
       age_15_less_f,age_16_24_f,age_25_34_f,age_35_49_f,age_50_64_f,age_65_over_f)

sex<-rep(c("Male","Female"),each=1000)

age_sex_df<-cbind(age_sex,sex) |> as.data.frame()

age_sex_df$age_sex<-as.numeric(age_sex_df$age_sex)

ggplot(age_sex_df,aes(x=sex,y=age_sex))+geom_violin()



p <- ggplot(age_sex_df, aes(factor(sex), age_sex)) +
  geom_violin()+xlab("Sex")+ylab("Age (years)")

plot_gg(p)

writeOBJ("AgeViolin.obj")
