rm(list=ls())

# load packages
library(tidyverse)
library(rvest)
library(readxl)
library(maps)
library(ggmap)
library(mapdata)
library(gganimate)
library(omnibus)

# Read the happines data from the UN from the CSV and put it in a dataframe
# Using the data from 2017 as the wikipedia data is also from 2017
# Data gotten from kaggle
df2017 <- read_csv('2017.csv')

crosswalk_df <- read_csv('country_iso_codes_expanded.csv')

# Create a dataframe for all homicide worldwide and remove the source collums
# Remove the subregion rows aswell
# Data gotten from dataUNODC
homicide_df <- read_excel('homicide_all.xls') %>% 
  subset(., select = c(1:7)) %>% 
  filter(., Indicator == 'Firearms rate') %>% 
  rename("Firearm_deathrate" = Value) %>% 
  mutate("Firearm_deathrate" = as.numeric(Firearm_deathrate))

homicide_df$Territory <- replace(homicide_df$Territory, homicide_df$Territory == 'United States of America', 'USA')

# Find the table from wikipedia and return a dataframe
table <- "https://en.wikipedia.org/wiki/Estimated_number_of_civilian_guns_per_capita_by_country" %>%
  read_html() %>%
  html_nodes(xpath='//table[1]') %>% 
  html_table(header = TRUE, fill = TRUE) %>% 
  .[[1]] %>% 
  data.frame()


# Rename the variables in the wikipedia table df and merge it with the happines data
df <- table %>% 
  rename("Country" = Country..or.dependent.territory..subnational.area..etc..) %>% 
  left_join(., df2017, by = 'Country') %>% 
  rename("Happiness_score" = Happiness.Score) %>% 
  rename("GDP_percap" = Economy..GDP.per.Capita.) %>% 
  mutate("Firearms_per100" = Estimate.of.civilian.firearms.per.100.persons) %>% 
  mutate("Life_expe" = Health..Life.Expectancy.) %>% 
  mutate("Firearms_estimate" = as.numeric(gsub(",", "", Estimate.of.firearms.in.civilian.possession))) %>% 
  mutate(Population = as.numeric(gsub(",", "", Population.2017, fixed = FALSE))) %>% 
  select(Country, Firearms_per100, Firearms_estimate, Population, Region, Happiness_score, GDP_percap, Life_expe) 

df$Country[2] = 'USA'

rm(df2017)

world_map <- map_data("world") %>% 
  rename(Country = 'region')
df_map <- left_join(df, world_map, by = "Country")

ggplot((filter(df_map, Firearms_per100 < 1000)), aes(long, lat, group = group))+
  geom_polygon(aes(fill = Firearms_per100), color = "white")+
  scale_fill_continuous() +
  theme_bw() +
  labs(fill = 'Firearms per 
100 inhabitants') +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.key.width = unit(1.5,"cm"))

homi_map <- homicide_df %>% 
  filter(., Level == "Country") %>% 
  rename("Country" = Territory) %>% 
  left_join(., world_map, by = "Country")


ggplot(homi_map, aes(long, lat, group = group))+
  geom_polygon(aes(fill = Firearm_deathrate), color = "grey")+
  scale_fill_continuous() +
  theme_bw() +
  labs(caption = 'Blank areas are countries without data', fill = 'Homicide per 100.000 inhabitants ') +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.key.width = unit(1.5,"cm"))

# Create the homicide df
df_homi <- homicide_df %>% 
  # Filter out the countries, as this dataframe contains alot more
  filter(., Level == "Country") %>% 
  # 2012 seems to be the year with the most countries. Would like to get a country from the other years, but alas
  filter(., Year == 2012) %>% 
  select(Territory, Firearm_deathrate) %>% 
  mutate(Firearm_deathrate = as.numeric(Firearm_deathrate)) %>% 
  rename('Country' = Territory) %>% 
  left_join(., df, by = 'Country')

test <- combineDf(df_homi, df, crosswalk = crosswalk_df)







