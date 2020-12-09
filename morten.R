
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
  filter(Year == 2012, Level == "Country") %>% 
  select(Territory, Firearms_deathrate) %>% 
  rename(Country = Territory) %>% 
  merge(table)


ggplot(regional_differences, aes(Firearms_per100, Firearms_deathrate, color = Region, size = Population)) +
  geom_jitter(aes(), show.legend = TRUE) +
  xlab("Firearms per 100 inhabitants") +
  ylab("Homicide rate by firearms") +
  theme(axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y.left = element_blank(),
        axis.text.x = element_blank())

ggplot(regional_differences, aes(GDP_percap, Happiness_score, color = Region, size = Population)) +
  geom_jitter(aes(), show.legend = TRUE) +
  xlab("Firearms per 100 inhabitants") +
  ylab("Homicide rate by firearms") +
  theme(axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y.left = element_blank(),
        axis.text.x = element_blank())


##### everything below is untouched #####

# Plot the countries sorted in a barplot
ggplot(table, aes(reorder(Country, Firearms_per100), Firearms_per100, fill = Country)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  theme(strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        axis.title.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank())

# Plot the firearms100 vs happines
ggplot(table, aes(Happiness_score, Firearms_per100, color = Country)) +
  geom_jitter(aes(), show.legend = FALSE)

ggplot(homicide_df, aes(Year, Value, group = Subregion, fill = Subregion)) +
  geom_bar(stat = 'identity', show.legend = FALSE) +
  facet_wrap(~Subregion) +
  theme_light() +
  theme(axis.text.y.left = element_blank())
