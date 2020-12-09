rm(list=ls())

# load packages
library(tidyverse)
library(rvest)


# Url for the schedule
url <- "https://en.wikipedia.org/wiki/List_of_countries_by_firearm-related_death_rate"


# Find the table from wikipedia and return a dataframe
df <- url %>%
  read_html() %>%
  html_nodes('table') %>% 
  html_table(header = NA, fill = TRUE) %>% 
  .[[4]] %>% 
  filter(row_number() <= n()-1) %>% 
  select(Country, Year, Total, Homicide, Unintentional, Undetermined, "Guns per 100 inhabitants")

df %>% 
  html_table()

