install.packages("tidyverse")
install.packages("plotly")
install.packages("gapminder")

library(tidyverse)
library(plotly)
library(dplyr)
library(gapminder)

unicef_metadata <- read_csv("unicef_metadata.csv", 
                            col_types = cols(alpha_2_code = col_skip(), 
                                             alpha_3_code = col_skip(), numeric_code = col_skip(), 
                                             iso3c = col_skip()))
unicef_indicator_1 <- read_csv("unicef_indicator_1.csv", 
                               col_types = cols(alpha_2_code = col_skip(), 
                                                alpha_3_code = col_skip(), numeric_code = col_skip(), 
                                                indicator = col_skip(), sex = col_skip(), 
                                                unit_multiplier = col_skip(), unit_of_measure = col_skip(), 
                                                observation_status = col_skip(), 
                                                observation_confidentaility = col_skip(), 
                                                time_period_activity_related_to_when_the_data_are_collected = col_skip(), 
                                                current_age = col_skip()))
data_right_3 <- read_csv("data_right_3.csv")


# Exercise 1

data_join <- full_join(unicef_metadata, unicef_indicator_1)
data_join <- full_join(unicef_metadata,unicef_indicator_1, by = c("country", "year" = "time_period"))

# Exercise 2

data_join <- full_join(unicef_indicator_1, data_right_3)
data_join <- full_join(unicef_indicator_1, data_right_3, by = c("country"))

# Exercise 3

data_join <- full_join(unicef_metadata, data_right_3)
data_join <- full_join(unicef_metadata, data_right_3, by = c("country"))

# Final Data Object
data_join <- unicef_metadata %>%
  full_join(unicef_indicator_1, by = c("country", "year" = "time_period")) %>%
  full_join(data_right_3, by = c("country"))

map_world <- map_data("world")

# Maps
map_data_join <- full_join(data_join, map_world, by = c("country" = "region"))
  
map_world <- map_data("world")

# Map 1
data_join %>%
  filter(year == 2022) %>%
  full_join(map_world, by = c("country" = "region")) %>%
  ggplot(aes(x = long, y = lat, group = group, fill = obs_value)) +
  geom_polygon() +
  scale_fill_gradient(low = "#FFB3B3", high = "#FFB3FF", na.value = "lightgrey") +
  labs(
    title = "Differences between countries regarding the amount of Overweight Children in 2000",
    subtitle = "Countries in grey have no data due to a mismatch with their names",
    caption = "Source : R package {gapminder}",
    x = "Longitude",
    y = "Latitude",
    fill = "obs_value"
  ) +
  theme_bw()


# Map 2 
map_world <- map_data("world")

data_join %>%
  filter(year == 2000) %>%
  full_join(map_world, by = c("country" = "region")) %>%
  ggplot(aes(x = long, y = lat, group = group, fill = obs_value)) +
  geom_polygon() +
scale_fill_gradient(low = "red", high = "purple", na.value = "lightgrey") +
  labs(
    title = "Differences between countries regarding the amount of Overweight Children in 2000",
    subtitle = "Countries in grey have no data due to a mismatch with their names",
    caption = "Source : R package {gapminder}",
    x = "Longitude",
    y = "Latitude",
    fill = "obs_value"
  ) +
  theme_bw()

# Time Series 1
options(scipen = 999)
data_join <- gapminder %>%
filter(!is.na(continent))

timeseries_plot_1 <- data_join %>%
  ggplot() +
  aes(year, pop, group = country, color = continent) +
  geom_line() +
  labs(
    title = "Differences between countries regarding population since 1960 by continent",
    caption = "Source : R package {gapminder}",
    x = "Year",
    y = "Population",
    color = "Continent"
  ) +
  theme_bw()

ggplotly(timeseries_plot_1)
    
# Scatter plot 4***

filtered_data <- unicef_metadata %>%
  filter(year %in% c(1965,1980, 2000, 2021))

ggplot(filtered_data) +
  aes(x = `GDP per capita (constant 2015 US$)`, 
      y = `Life expectancy at birth, total (years)`, 
      color = country, 
      size = `Population, total`) +
  geom_point(alpha = 0.4) + 
  facet_wrap(~ year, nrow = 1) +
  scale_x_continuous(
    limits = c(0,50000),
    breaks = c(20000, 40000),
    labels = scales :: dollar_format(scale = 0.01)
  ) +
  scale_size_continuous(
    range = c(1, 10),  # Adjusted range
    breaks = c(1000000, 10000000),  # Adjusted breaks
    labels = scales::comma
  ) +
  labs(
    x = "GDP per Capita in USD",
    y = "Life Expectancy in years",
    title = "Evolution of the Relationship between Life Expectancy and GDP from 2010 to 2021 per country"
  ) +
  guides(color = "none", size = "none") +
  theme_classic() +
  theme(text = element_text(family = "serif"))

# Bar Chart X

# Filter and summarize data with a different object name

filtered_data <- unicef_metadata %>%
  filter(year %in% c(1965, 1980, 2000, 2021)) 

# Left join with data_join to add additional columns
filtered_data <- left_join(filtered_data, data_join, by = "country")

# Filter out rows with NA continent
filtered_data <- filtered_data %>%
  filter(!is.na(continent))
  
# Group by continent and calculate average life expectancy

avg_life_expectancy <- filtered_data %>%
  group_by(year.x, continent) %>%
  summarise(m_lifeexp = mean(`Life expectancy at birth, total (years)`, na.rm = TRUE))

# Plot
ggplot(avg_life_expectancy, aes(x = reorder(continent, m_lifeexp, FUN = median), y = m_lifeexp, fill = continent)) +
  geom_col() +
  facet_wrap(~ year.x, nrow = 1) +
  labs(
    x = "Time Period",
    y = "Average Life Expectancy",
    fill = "Continent",
    title = "Evolution of the Average Life Expectancy per continent from 1965 to 2021 per continent"
  ) +
  theme_classic() + 
  theme(
    text = element_text(family = "serif"),
    axis.text.x = element_blank()
  ) +
  scale_fill_manual(values = c("#FF9999", "#FFFF66", "#99FF99", "#9999FF", "#FF99FF"))

