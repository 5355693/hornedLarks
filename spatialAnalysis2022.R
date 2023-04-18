library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)

shlaSpatial22 <- read_csv(file = "hornedLarks/tbl_all_potential_survey_sites_with_2022_results_04_06_2023.csv",
                          col_select = c(1:3, 61:77, 80:81))
shlaSpatial22$OBJECTID <- factor(shlaSpatial22$OBJECTID)
shlaSpatial22$ORIG_FID <- factor(shlaSpatial22$ORIG_FID)
shlaSpatial22$ID <- factor(shlaSpatial22$ID)
shlaSpatial22$recovery_zone <- factor(shlaSpatial22$recovery_zone)
shlaSpatial22$unique_ID <- factor(shlaSpatial22$unique_ID)

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

shlaSpatial22 %>%
  filter(surveyed_2022 == 1) %>%
  ggplot(., aes(x = four_year_avg_scaled, fill = as.factor(num_birds_2022))) + geom_dotplot(binwidth = 0.025) +
  scale_y_continuous(labels=function(x)x*40) + 
  labs(fill = "No. of larks detected",
       x = "Scaled average habitat suitability",
       y = "No. of points")

# NEED TO SET VARIABLES TO PROPER TYPE IN THE INITIAL READ.CSV!
  shlaSpatial22 %>%
    select(unique_ID, max_2017,max_2018, max_2020, max_2021) %>%
    pivot_longer(cols = !unique_ID, names_to = "Year", values_to = "Suitability") %>%
    ggplot(., aes(x = Year, y = Suitability, group = unique_ID)) + geom_line()

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
