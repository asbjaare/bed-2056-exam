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
  mutate("Firearm_deathrate" = as.numeric(Value))

# Find the table from wikipedia and return a dataframe
table <- "https://en.wikipedia.org/wiki/Estimated_number_of_civilian_guns_per_capita_by_country" %>%
  read_html() %>%
  html_nodes(xpath='//table[1]') %>% 
  html_table(fill = TRUE) %>% 
  data.frame()


# Rename the variables in the wikipedia table df and merge it with the happines data
df <- table %>% 
  rename("Country" = Country..or.dependent.territory..subnational.area..etc..) %>% 
  rename("Firearms100" = Estimate.of.civilian.firearms.per.100.persons) %>% 
  rename("Firearms_estimate" = Estimate.of.firearms.in.civilian.possession) %>% 
  rename("Reg_firearm" = Registered.firearms) %>% 
  rename("Unreg_firearm" = Unregistered.firearms) %>% 
  rename("Population" = Population.2017) %>% 
  mutate(Population = sub(",", "", Population, fixed = FALSE)) %>% 
  mutate(Population = sub(",", "", Population, fixed = FALSE)) %>%
  mutate(Population = sub(",", "", Population, fixed = FALSE)) %>% 
  mutate(Population = as.numeric(Population)) %>% 
  select(Country, Firearms100, Firearms_estimate, Reg_firearm, Unreg_firearm, Population, Region) %>% 
  merge(., df2017)

# Plot the countries sorted in a barplot
ggplot(df, aes(reorder(Country, Firearms100), Firearms100, fill = Country)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  theme(strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        axis.title.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank())

# Plot the firearms100 vs happines
ggplot(df, aes(Happiness.Score, Firearms100, color = Country)) +
  geom_jitter(aes(), show.legend = FALSE)

# Plots the homicide rate 
ggplot(homicide_df, aes(Year, Value, group = Subregion, fill = Subregion)) +
  geom_bar(stat = 'identity', show.legend = FALSE) +
  facet_wrap(~Subregion) +
  theme_light() +
  theme(axis.text.y.left = element_blank())

# Dont think i need this but w/e
df_2 <- table %>% 
  rename("Country" = Country..or.dependent.territory..subnational.area..etc..) %>% 
  rename("Firearms100" = Estimate.of.civilian.firearms.per.100.persons) %>% 
  rename("Firearms_estimate" = Estimate.of.firearms.in.civilian.possession) %>% 
  rename("Reg_firearm" = Registered.firearms) %>% 
  rename("Unreg_firearm" = Unregistered.firearms) %>% 
  rename("Population" = Population.2017) %>% 
  mutate(Population = sub(",", "", Population, fixed = FALSE)) %>%
  mutate(Population = sub(",", "", Population, fixed = FALSE)) %>% 
  mutate(Population = as.numeric(Population)) %>% 
  select(Country, Firearms100, Population, Region)

# Create the 
df_fire_vs_homi <- homicide_df %>% 
  filter(., Level == "Country") %>% 
  filter(., Year == 2012) %>% 
  select(Territory, Value) %>% 
  mutate(Value = as.numeric(Value)) %>% 
  rename('Country' = Territory) %>% 
  merge(., df_2)


# Plot the firearms100 vs happines
ggplot(df_fire_vs_homi, aes(Firearms100, Value, color = Region, size = Population)) +
  geom_jitter(aes(), show.legend = TRUE) +
  xlab("Homicide rate by firearms") +
  ylab("Firearms per 100 inhabitants") +
  theme(axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y.left = element_blank(),
        axis.text.x = element_blank())





 