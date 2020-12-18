
##### wikipedia struggle shit #####

# rm(list=ls())
# 
# # load packages
# library(tidyverse)
# library(rvest)
# 
# 
# # Url for the schedule
# url <- "https://en.wikipedia.org/wiki/List_of_countries_by_firearm-related_death_rate"
# 
# 
# # Find the table from wikipedia and return a dataframe
# df <- url %>%
#   read_html() %>%
#   html_nodes('table') %>% 
#   html_table(header = NA, fill = TRUE) %>% 
#   .[[4]] %>% 
#   filter(row_number() <= n()-1) %>% 
#   select(Country, Year, Total, Homicide, Unintentional, Undetermined, "Guns per 100 inhabitants")
# 


##### using homicide xls #####

rm(list=ls())

# load packages
library(tidyverse)
library(rvest)
library(readxl)

# Read the happines data from the UN from the CSV and put it in a dataframe
# Using the data from 2017 as the wikipedia data is also from 2017
# Data gotten from kaggle
df2017 <- read_csv('2017.csv')

# Create a dataframe for all homicide worldwide and remove the source collums
# Remove the subregion rows aswell
# Data gotten from dataUNODC
homicide_df <- read_excel('homicide_all.xls') %>% 
  subset(., select = c(1:7)) %>% 
  filter(., Indicator == 'Firearms rate') %>% 
  mutate("Firearms_deathrate" = as.numeric(Value))

homicide_total <- read_excel('homicide_all.xls') %>% 
  subset(., select = c(1:7)) %>% 
  filter(., Indicator == 'Homicide rate') %>% 
  mutate("Homicide_rate" = as.numeric(Value))

# Find the table from wikipedia and return a dataframe
table <- "https://en.wikipedia.org/wiki/Estimated_number_of_civilian_guns_per_capita_by_country" %>%
  read_html() %>%
  html_nodes(xpath='//table[1]') %>% 
  html_table(header = TRUE, fill = TRUE) %>% 
  .[[1]] %>% 
  data.frame()

# Rename the variables in the wikipedia table df and merge it with the happines data
table <- table %>% 
  rename("Country" = Country..or.dependent.territory..subnational.area..etc..) %>% 
  merge(., df2017) %>% 
  rename("Happiness_score" = Happiness.Score) %>% 
  rename("GDP_percap" = Economy..GDP.per.Capita.) %>% 
  mutate("Firearms_per100" = as.numeric(gsub(",", "", Estimate.of.civilian.firearms.per.100.persons))) %>% 
  mutate("Firearms_estimate" = as.numeric(gsub(",", "", Estimate.of.firearms.in.civilian.possession))) %>% 
  mutate("Reg_firearm" = as.numeric(gsub(",", "", Registered.firearms))) %>% 
  mutate("Unreg_firearm" = as.numeric(gsub(",", "", Unregistered.firearms))) %>% 
  mutate(Population = as.numeric(gsub(",", "", Population.2017, fixed = FALSE))) %>% 
  select(Country, Firearms_per100, Firearms_estimate, Reg_firearm, Unreg_firearm, Population, Region, Happiness_score, GDP_percap) 


regional_differences <- homicide_df %>% 
  filter(Year <= 2015 & Year >= 2010, Level == "Country") %>% 
  group_by(Territory) %>% 
  summarize("Firearms_deathrate" = median(Firearms_deathrate)) %>% 
  select(Territory, Firearms_deathrate) %>% 
  rename(Country = Territory) %>% 
  merge(table)

stuff <- homicide_total %>% 
  filter(Year <= 2015 & Year >= 2010, Level == "Country") %>% 
  group_by(Territory) %>% 
  summarize("Homicide_rate" = median(Homicide_rate)) %>% 
  select(Territory, Homicide_rate) %>% 
  rename(Country = Territory) %>% 
  merge(table)


more_stuff <- stuff %>% 
  merge(regional_differences) %>% 
  mutate("Ratio" = Firearms_deathrate / Homicide_rate)



#### Kind of a proof of concept if anything, looking at specific slice of happiness vs firearm homicides ####
testing <- more_stuff %>% 
  filter(Happiness_score <= 7.5 & Happiness_score >= 6.5)

ggplot(testing, aes(Happiness_score, Firearms_deathrate, color = Region, size = Population)) +
  geom_jitter(aes(),
              show.legend = TRUE) +
  xlab("Happiness score") +
  ylab("Homicide rate by firearms") +
  geom_text(data=subset(testing, Firearms_deathrate > 0.5),
            aes(label = Country, y = Firearms_deathrate - 0.5),
            size = 4,
            show.legend = FALSE)


#### Plot number of firearms(per100) vs what % of homicides committed by firearms ####
ggplot(more_stuff, aes(Firearms_per100, Ratio, color = Region, size = Population)) +
  geom_jitter(aes(),
              show.legend = TRUE) +
  xlab("Firearms per 100 inhabitants") +
  ylab("Proportion of homicides by firearm") +
  scale_y_continuous(labels = scales::percent)


#### Plot number of firearms(per100) vs total homicide rate ####
ggplot(stuff, aes(Firearms_per100, Homicide_rate, color = Region, size = Population)) +
  geom_jitter(aes(),
              show.legend = TRUE) +
  xlab("Firearms per 100 inhabitants") +
  ylab("Homicide rate") +
  theme(axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y.left = element_blank(),
        axis.text.x = element_blank()) +
  geom_text(data=subset(regional_differences, Country == "Honduras"),
            aes(label = Country, y = Firearms_deathrate - 2),
            size = 4,
            show.legend = FALSE)



#### Plot number of firearms(per100) vs homicides by firearm ####
ggplot(regional_differences, aes(Firearms_per100, Firearms_deathrate, color = Region, size = Population)) +
  geom_jitter(aes(),
              show.legend = TRUE) +
  xlab("Firearms per 100 inhabitants") +
  ylab("Homicide rate by firearms") +
  theme(axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y.left = element_blank(),
        axis.text.x = element_blank()) +
  geom_text(data=subset(regional_differences, Country == "Honduras"),
            aes(label = Country, y = Firearms_deathrate - 2),
            size = 4,
            show.legend = FALSE)


#### Plot GDP vs Happiness ####
ggplot(regional_differences, aes(GDP_percap, Happiness_score, color = Region, size = Population)) +
  geom_jitter(aes(), show.legend = TRUE) +
  xlab("GDP per capita") +
  ylab("Happiness score") +
  theme(axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y.left = element_blank(),
        axis.text.x = element_blank())


