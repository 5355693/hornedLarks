library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)

shlaSpatial22 <- read_csv(file = "hornedLarks/tbl_all_potential_survey_sites_with_2022_results_04_06_2023.csv",
                          col_select = c(61:77, 80:81),
                          col_types = cols(.default = "n", 
                                           recovery_zone = "f",
                                           unique_ID = "f",
                                           surveyed_2022 = "l"))

## Habitat suitability (10-class) in the 2022 sample
shlaSpatial22 %>%
  filter(surveyed_2022 == T) %>%
  ggplot(., aes(x = four_year_avg)) + geom_histogram(boundary = 0) +
  labs(
    x = "Four-year average habitat suitability",
    y = "No. of points, 2022 sample",
    title = "Distribution of four-year avearage habitat\nsuitability values in the 2022 sample points"
  )

## Scaled habitat suitability (0.0 - 1.0) in the 2022 sample
shlaSpatial22 %>%
  filter(surveyed_2022 == T) %>%
  ggplot(., aes(x = four_year_avg_scaled)) + geom_histogram(boundary = 0) +
  labs(
    x = "Scaled average habitat suitability",
    y = "No. of points (2022 sample)",
    title = "Distribution of scaled avearage habitat\nsuitability values in the 2022 sample points"
  )

shlaSpatial22 %>%
  filter(surveyed_2022 == T) %>%
  ggplot(., aes(x = four_year_avg_scaled, fill = as.factor(num_birds_2022))) + geom_dotplot(binwidth = 0.025) +
  scale_y_continuous(labels=function(x)x*40) + 
  labs(fill = "No. of larks detected",
       x = "Scaled average habitat suitability",
       y = "No. of points")

# Rough look at changes year-to-year in predicted habitat suitability
mean(abs(shlaSpatial22$max_2018 - shlaSpatial22$max_2017))
mean(abs(shlaSpatial22$max_2020 - shlaSpatial22$max_2018))  
mean(abs(shlaSpatial22$max_2021 - shlaSpatial22$max_2020))
mean(0.762,0.745, 0.904) # = 0.762

min(abs(shlaSpatial22$max_2018 - shlaSpatial22$max_2017)) # 0
min(abs(shlaSpatial22$max_2020 - shlaSpatial22$max_2018)) # 0
min(abs(shlaSpatial22$max_2021 - shlaSpatial22$max_2020)) # 0

max(abs(shlaSpatial22$max_2018 - shlaSpatial22$max_2017)) # 6
max(abs(shlaSpatial22$max_2020 - shlaSpatial22$max_2018)) # 6
max(abs(shlaSpatial22$max_2021 - shlaSpatial22$max_2020)) # 6 

a<-(abs(shlaSpatial22$max_2018 - shlaSpatial22$max_2017))
b<-(abs(shlaSpatial22$max_2020 - shlaSpatial22$max_2018)) 
c<-(abs(shlaSpatial22$max_2021 - shlaSpatial22$max_2020))
d <- rbind(a,b,c)
hist(d, main = "Year-to-year change\nin habitat suitability at all points",
     xlab = "Year-to-year change in habitat suitabiliy")

# Compare presence/absence and habitat suitability:
shlaSpatial22 %>%
  filter(surveyed_2022 == T) %>%
  mutate(presence = if_else(num_birds_2022>0,1,0)) %>% # create a binary
 # select(presence, num_birds_2022) %>% # make sure it works
 # arrange(desc(num_birds_2022)) # make sure it works
  ggplot(., aes(x = four_year_avg, y = presence)) + geom_point() + 
  stat_smooth(method="glm", se=TRUE, method.args = list(family=binomial))

shlaSpatial22 %>%
  ggplot(., aes(x = four_year_avg, fill = surveyed_2022)) + geom_histogram() + 
  labs(fill = "Surveyed in 2022?",
       x = "Four-year average habitat suitability (0-10)",
       y = "No. of points")
