rm(list=ls())

# load packages
library(tidyverse)
library(rvest)

# Url for the schedule
#url <- "https://en.wikipedia.org/wiki/List_of_countries_by_firearm-related_death_rate"
url <- "https://en.wikipedia.org/wiki/Estimated_number_of_civilian_guns_per_capita_by_country"

# Find the table from wikipedia and return a dataframe
table <- url %>%
  read_html() %>%
  html_nodes(xpath='//table[1]') %>% 
  html_table(fill = TRUE) %>% 
  data.frame()

# Read the happines data from the UN from the CSV and put it in a dataframe
df2017 <- read_csv('2017.csv')


# Rename the variables in the wikipedia table df and merge it with the happines data
table <- table %>% 
  rename("Country" = Country..or.dependent.territory..subnational.area..etc..) %>% 
  rename("Firearms100" = Estimate.of.civilian.firearms.per.100.persons) %>% 
  rename("Firearms_estimate" = Estimate.of.firearms.in.civilian.possession) %>% 
  rename("Reg_firearm" = Registered.firearms) %>% 
  rename("Unreg_firearm" = Unregistered.firearms) %>% 
  select(Country, Firearms100, Firearms_estimate, Reg_firearm, Unreg_firearm) %>% 
  merge(., df2017)


# Plot the countries sorted in a barplot
ggplot(table, aes(reorder(Country, Firearms100), Firearms100, fill = Country)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  theme(strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        axis.title.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank())


 