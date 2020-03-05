#CS356
#Unit 2
#Assignment Submission 2

#Use BigQuery Table
#Use MapReduce to determine the top 10
#most popular names and the top 10 least
#popular names in the United States.

#Installing Packages
library(highr)
library(tidyverse)
library(lubridate)
library(tibble)
library(tidyr)
library(readr)
library(purrr)
library(dplyr)
library(stringr)
library(forcats)
library(openair)
library(tidyverse)
library(tidytext)
library(gridExtra)

#Read  usa_names_agg.csv file
names <- read_csv("C:/Users/rafsan bhuiyan/Downloads/results/usa_names_agg.csv", col_types = c("iccci")) %>%
  select(-1)

#use the mutate funtion to identify gender labels
#use the factor function to encode a vector
#read the date and mutate
#change to decade
names <- names %>% 
  mutate(gender = factor(gender, labels = c("Female", "Male")))
names <- names %>% 
  mutate(decade = year %>% as.character %>% str_replace("\\d$", "0s")) %>% 
  mutate(decade = factor(decade))

# Summarize the count by Decade
names_sum <- names %>% 
  group_by(decade, gender, name) %>% 
  summarise(count = sum(number)) %>% 
  ungroup()

#Determine the top 10 most popular names
#and store it into top10MostPopularNames
top10MostPopularNames <- names_sum %>%
  arrange(desc(count)) %>%
  top_n(10) %>%
  ungroup()

# Save the results to a CSV file
write.csv(
  x = top10MostPopularNames,
  file = "top10MostPopularNames.csv",
  row.names = FALSE)

#Determine the 10 least popular names
#and store it into top10LeastPopularNames
top10LeastPopularNames <- names_sum %>%
  arrange(count) %>%
  head(10)

# Save the results to a CSV file
write.csv(
  x = top10LeastPopularNames,
  file = "top10LeastPopularNames.csv",
  row.names = FALSE)

