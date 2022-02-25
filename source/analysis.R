library("dplyr")
library("tidyr")
library("ggplot2")


county_level_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")
jurisdiction_level_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends_jail_jurisdiction.csv")

# Summary Statistic 1: What is the change of the proportion of jailed population of latinx folks to the total population of folks between 1990 and 2018
latinx_Folks_1990 <- county_level_data %>%
  group_by(year) %>%
  summarise(latinx_population = sum(latinx_jail_pop, na.rm = TRUE)) %>%
  filter(year == 1990) %>%
  pull(latinx_population)
total_Population_1990 <- county_level_data %>%
  group_by(year) %>%
  summarise(total_population = sum(total_pop, na.rm = TRUE)) %>%
  filter(year == 1990) %>%
  pull(total_population)
ltx_Jailed_Pop_1990 <- latinx_Folks_1990 / total_Population_1990

latinx_Folks_2018 <- county_level_data %>%
  group_by(year) %>%
  summarise(latinx_population = sum(latinx_jail_pop, na.rm = TRUE)) %>%
  filter(year == 2018) %>%
  pull(latinx_population)
total_Population_2018 <- county_level_data %>%
  group_by(year) %>%
  summarise(total_population = sum(total_pop, na.rm = TRUE)) %>%
  filter(year == 2018) %>%
  pull(total_population)
ltx_Jailed_Pop_2018 <- latinx_Folks_2018 / total_Population_2018

latinx_prop_change <- (ltx_Jailed_Pop_2018 - ltx_Jailed_Pop_1990) / ltx_Jailed_Pop_1990


# Summary Statistic 2: What is the change of the proportion of jailed population of aapi folks to the total population of folks between 1990 and 2018
aapi_Folks_1990 <- county_level_data %>%
  group_by(year) %>%
  summarise(aapi_population = sum(aapi_jail_pop, na.rm = TRUE)) %>%
  filter(year == 1990) %>%
  pull(aapi_population)
total_Population_1990 <- county_level_data %>%
  group_by(year) %>%
  summarise(total_population = sum(total_pop, na.rm = TRUE)) %>%
  filter(year == 1990) %>%
  pull(total_population)
aapi_Jailed_Pop_1990 <- aapi_Folks_1990 / total_Population_1990

aapi_Folks_2018 <- county_level_data %>%
  group_by(year) %>%
  summarise(aapi_population = sum(aapi_jail_pop, na.rm = TRUE)) %>%
  filter(year == 2018) %>%
  pull(aapi_population)
total_Population_2018 <- county_level_data %>%
  group_by(year) %>%
  summarise(total_population = sum(total_pop, na.rm = TRUE)) %>%
  filter(year == 2018) %>%
  pull(total_population)
aapi_Jailed_Pop_2018 <- aapi_Folks_2018 / total_Population_2018

aapi_prop_change <- (aapi_Jailed_Pop_2018 - aapi_Jailed_Pop_1990) / aapi_Jailed_Pop_1990


# Summary Statistic 3: What is the change of the proportion of jailed population of white folks to the total population of folks between 1990 and 2018
white_Folks_1990 <- county_level_data %>%
  group_by(year) %>%
  summarise(white_population = sum(white_jail_pop, na.rm = TRUE)) %>%
  filter(year == 1990) %>%
  pull(white_population)
total_Population_1990 <- county_level_data %>%
  group_by(year) %>%
  summarise(total_population = sum(total_pop, na.rm = TRUE)) %>%
  filter(year == 1990) %>%
  pull(total_population)
white_Jailed_Pop_1990 <- white_Folks_1990 / total_Population_1990

white_Folks_2018 <- county_level_data %>%
  group_by(year) %>%
  summarise(white_population = sum(white_jail_pop, na.rm = TRUE)) %>%
  filter(year == 2018) %>%
  pull(white_population)
total_Population_2018 <- county_level_data %>%
  group_by(year) %>%
  summarise(total_population = sum(total_pop, na.rm = TRUE)) %>%
  filter(year == 2018) %>%
  pull(total_population)
white_Jailed_Pop_2018 <- white_Folks_2018 / total_Population_2018

white_prop_change <- (white_Jailed_Pop_2018 - white_Jailed_Pop_1990) / white_Jailed_Pop_1990


# Summary Statistic 4: Which county in New York State had the most total population of jailed females
ny_data <- county_level_data %>% 
  filter(state == "NY") 

county_highest_female_adult_jail_pop <- function(county_level_data) {
  county_level_data %>% 
    filter(female_adult_jail_pop == max(female_adult_jail_pop, na.rm = TRUE)) %>% 
    pull(county_name)
}

by_highest_county_female_adult_jail_pop <- county_highest_female_adult_jail_pop(ny_data)


# Summary Statistic 5: Which county in Washington state had the most total population between 15-64 year old Females
wa_data <- county_level_data %>% 
  filter(state == "WA") 

county_highest_female_pop_15to64 <- function(county_level_data) {
  county_level_data %>% 
    filter(female_pop_15to64 == max(female_pop_15to64, na.rm = TRUE)) %>% 
    pull(county_name)
}

wa_highest_county_female_pop_15to64 <- county_highest_female_pop_15to64(wa_data)


# Time Trend Chart would be a total population over an amount of time


# Variable Comparison compare two races


# Map


