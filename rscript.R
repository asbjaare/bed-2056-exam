rm(list=ls())

# load packages
library(tidyverse)
library(rvest)

# Url for the schedule
url <- "https://en.wikipedia.org/wiki/List_of_countries_by_firearm-related_death_rate"

# Gets the date and removes the "Mandag" from each of the chars
table <- url %>%
  read_html() %>%
  html_nodes(xpath='//table[4]') %>% 
  html_table(fill = TRUE) %>% 
  data.frame()

table <- table %>% 
  mutate(Year = as.numeric(Year), Total = as.numeric(Total), Homicide = as.numeric(Homicide))

ggplot(table, aes(Total, Homicide, group = Country, color = Country, label = Country)) +
  geom_jitter(aes(), show.legend = FALSE) +
  scale_y_continuous(trans = 'log10') +
  scale_x_continuous(trans = 'log10') +
  geom_text(aes(label=ifelse(Homicide>1,as.character(Country),'')), hjust=0,vjust=0, show.legend = FALSE)
