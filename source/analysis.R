library("dplyr")
library("tidyr")
library("ggplot2")
library("maps")
library("mapproj")

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

ny_highest_county_female_adult_jail_pop <- county_highest_female_adult_jail_pop(ny_data)


# Summary Statistic 5: Which county in Washington state had the most total population between 15-64 year old Females
wa_data <- county_level_data %>% 
  filter(state == "WA") 

county_highest_female_pop_15to64 <- function(county_level_data) {
  county_level_data %>% 
    filter(female_pop_15to64 == max(female_pop_15to64, na.rm = TRUE)) %>% 
    pull(county_name)
}

wa_highest_county_female_pop_15to64 <- county_highest_female_pop_15to64(wa_data)


# Time Trend Chart: Chart the trend of jailed population of latinx folk vs. the total population of jailed folks since 2000 to 2018
trend_comparison <- county_level_data %>%
  filter(year >= 2000 & year <= 2018) %>%
  group_by(year) %>%
  summarise(
    ltx_jp = sum(latinx_jail_pop, na.rm = TRUE),
    total_jp = sum(total_jail_pop, na.rm = TRUE)
  ) %>%
  select(year, ltx_jp, total_jp)
trend_chart <- ggplot(data = trend_comparison) +
  geom_line(aes(y = ltx_jp, x = year, colour = "Latinx")) +
  geom_line(aes(y = total_jp, x = year, colour = "Total")) +
  labs(x = "Year", y = "Jailed Population", title = "Latinx Jailed Population vs. Total Jailed Population, Years 2000 vs 2018") +
  scale_color_manual(
    name = "Line Representation",
    values = c("Latinx" = "darkred", "Total" = "steelblue")
  )

# Variable Comparison Chart: Compare the total of the total aapi jailed populations at each county vs. white jailed populations
aapi_vs_white_comparison_data <- county_level_data %>%
  filter(!is.na(aapi_jail_pop)) %>%
  filter(!is.na(white_jail_pop)) %>%
  select(white_jail_pop, aapi_jail_pop)
aapi_vs_white_jp_comparison <- ggplot(data = aapi_vs_white_comparison_data) +
  aes(y = white_jail_pop, x = aapi_jail_pop) +
  geom_point() +
  geom_smooth(method = lm) +
  labs(
    x = "AAPI Jailed Population", y = "White Jailed Population",
    title = "AAPI Jailed Population vs. Total Population, White"
  )

# Mapping: Map the Black total Jail Population within the year 2017 and Map the Total Male Jail Population within the year 2017

updated_county <- county_level_data %>%
  group_by(county_name) %>%
  filter(year == 2017)

county_shapes <- map_data("county") %>%
  unite(polyname, region, subregion, sep = ",") %>%
  left_join(county.fips, by = "polyname")

map_data <- county_shapes %>%
  left_join(updated_county, by = "fips")

blank_theme <- theme_bw() +
  theme ( 
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank (),
    panel.border = element_blank()
  )

black_jp_map <- ggplot(map_data) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = black_jail_pop),
    color = "blue", size = 0.5
  ) +
  coord_map() +
  scale_fill_continuous(limits = c(0, max(map_data$black_jail_pop)), 
                        na.value = "white", low = "pink", high = "purple") +
  blank_theme +
  labs(fill = "2017 Black Jail Population") +
  ggtitle("2017 US Black Jailed Population")

total_male_jp_map <- ggplot(map_data) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = male_jail_pop),
    color = "blue", size = 0.5
  ) +
  coord_map() +
  scale_fill_continuous(limits = c(0, max(map_data$male_jail_pop)), 
                        na.value = "white", low = "pink", high = "purple") +
  blank_theme +
  labs(fill = "2017 Total Male Jailed Population") +
  ggtitle("2017 US Total Male Jailed Population")

