library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)

shlaSpatial22 <- read_csv(file = "hornedLarks/tbl_all_potential_survey_sites_with_2022_results_04_06_2023.csv",
                          col_select = c(1:3, 61:77, 80:81))

shlaSpatial22 <-
  shlaSpatial22 %>%
  filter(surveyed_2022 == 1)

## Habitat suitability (10-class) in the 2022 sample
shlaSpatial22 %>%
  ggplot(., aes(x = four_year_avg)) + geom_histogram(boundary = 0) +
  labs(
    x = "Four-year average habitat suitability",
    y = "No. of points, 2022 sample",
    title = "Distribution of four-year avearage habitat\nsuitability values in the 2022 sample points"
  )

## Scaled habitat suitability (0.0 - 1.0) in the 2022 sample
shlaSpatial22 %>%
  ggplot(., aes(x = four_year_avg_scaled)) + geom_histogram(boundary = 0) +
  labs(
    x = "Scaled average habitat suitability",
    y = "No. of points (2022 sample)",
    title = "Distribution of scaled avearage habitat\nsuitability values in the 2022 sample points"
  )

# NEED TO SET VARIABLES TO PROPER TYPE IN THE INITIAL READ.CSV!
  shlaSpatial22 %>%
    select(unique_ID, max_2017,max_2018, max_2020, max_2021) %>%
    pivot_longer(cols = !unique_ID, names_to = "Year", values_to = "Suitability") %>%
    ggplot(., aes(x = Year, y = Suitability, group = unique_ID)) + geom_line()

