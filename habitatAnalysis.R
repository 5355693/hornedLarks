library(tidyverse)
library(readxl)
library(lubridate)
library(suncalc)
library(ggpmisc)
library(unmarked)
library(AICcmodavg)
library(ubms)
library(knitr)
library(kableExtra)
library(ggpmisc)
library(ubms)
library(gt)

# Review and organize data, changing formats and variable names as needed.
habitatData <- read_excel("WV_SHLA_survey_data_from_SA_2025_11_25_25_200m_buf_SuitableHabAnalysisResults.xlsx", 
sheet = "tbl_pts_WV_SHLA_survey_data_fro", 
col_types = c("numeric", "text", "text", 
"skip", "skip", "skip", "skip", "skip", 
"numeric", "numeric", "numeric", 
"numeric", "numeric", "numeric", 
"numeric", "numeric", "numeric", 
"numeric", "numeric", "numeric", 
"numeric", "numeric", "numeric", 
"numeric", "numeric", "numeric", 
"numeric", "numeric", "numeric", 
"numeric", "numeric", "numeric", 
"numeric", "numeric", "skip", "numeric", 
"numeric", "numeric", "numeric", 
"numeric", "skip", "numeric", "numeric", 
"numeric", "numeric", "skip", "text", 
"numeric", "skip", "skip", "skip", 
"skip", "numeric", "numeric", "numeric", 
"numeric", "numeric", "numeric"))

# Remove a blank row
habitatData <- habitatData[-141, ]

habitatData <- habitatData %>% 
  mutate(firstColor = ifelse(diff_pct_suit_30Apr25_24May25 > 0, "black", "red"))

habitatData <- habitatData %>% 
  mutate(secondColor = ifelse(diff_pct_suit_30Apr25_08June25 > 0, "black", "red"))

habitatData <- habitatData %>%
  mutate(thirdColor = ifelse(diff_pct_suit_30Apr25_15June25 > 0, "black", "red"))

# Plot change to first period.
ggplot(habitatData, aes(x = unique_ID, y = diff_pct_suit_30Apr25_24May25)) +
  geom_segment(aes(x = unique_ID, xend = unique_ID, y = 0, yend = diff_pct_suit_30Apr25_24May25), color="grey") +
  geom_point(color = habitatData$firstColor, size = 2) +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_text(size = 18)
  ) +
  xlab("") +
  ylab("Change in proportion suitable habitat,\n30 April to 25 May")

# Plot change to second period.
ggplot(habitatData, aes(x = unique_ID, y = diff_pct_suit_30Apr25_08June25)) +
  geom_segment(aes(x = unique_ID, xend = unique_ID, y = 0, yend = diff_pct_suit_30Apr25_08June25), color="grey") +
  geom_point(color = habitatData$secondColor, size = 2) +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_text(size = 18)
  ) +
  xlab("") +
  ylab("Change in proportion suitable habitat,\n30 April to 08 June")

# Plot change to third period.
ggplot(habitatData, aes(x = unique_ID, y = diff_pct_suit_30Apr25_15June25)) +
  geom_segment(aes(x = unique_ID, xend = unique_ID, y = 0, yend = diff_pct_suit_30Apr25_15June25), color="grey") +
  geom_point(color = habitatData$thirdColor, size = 2) +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_text(size = 18)
  ) +
  xlab("") +
  ylab("Change in proportion suitable habitat,\n30 April to 15 June")

# Boxplots of change in habitat suitability by period
habitatData %>%
  select(diff_pct_suit_30Apr25_24May25, diff_pct_suit_30Apr25_08June25, diff_pct_suit_30Apr25_15June25) %>%
  pivot_longer(., cols = 1:3, names_to = "Date", values_to = "Habitat") %>%
  ggplot(., aes(x = factor(Date, levels = c("diff_pct_suit_30Apr25_24May25",
                                            "diff_pct_suit_30Apr25_08June25",
                                            "diff_pct_suit_30Apr25_15June25")), y = Habitat)) + 
  geom_boxplot() + 
  scale_x_discrete(
    breaks = c("diff_pct_suit_30Apr25_24May25",
    "diff_pct_suit_30Apr25_08June25",
    "diff_pct_suit_30Apr25_15June25"),          
    labels = c("May 24", "Jun 08", "Jun 15")) + 
  theme(legend.position = "none",
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 12)) +
  labs(y = "Change proportion suitable habitat\nsince 30 April",
       x = "Habitat sampling date")

# Plot individual point changes over time
habitatData %>%
  select(unique_ID,diff_pct_suit_30Apr25_24May25,pct_suitable_may_24_2025, pct_suitable_june_08_2025, pct_suitable_june_15_2025) %>%
  mutate(pct_suitable_apr_30_2025 = pct_suitable_may_24_2025 - diff_pct_suit_30Apr25_24May25) %>%
  select(unique_ID,pct_suitable_apr_30_2025, pct_suitable_may_24_2025, pct_suitable_june_08_2025, pct_suitable_june_15_2025) %>%
  rename(y0 = pct_suitable_apr_30_2025,
         y1 = pct_suitable_may_24_2025,
         y2 = pct_suitable_june_08_2025,
         y3 = pct_suitable_june_15_2025) %>%
  pivot_longer(., cols = starts_with("y"), names_to = "segmentNumber", values_to = "suitHab") %>%
  group_split(unique_ID) %>%
  map(~ {
    # Apply embed to the time_val column
    embedded_matrix <- embed(.x$suitHab, dimension = 2)
    
    # Convert matrix to a tibble and preserve the group identifier
    tibble(
      unique_ID = unique(.x$unique_ID),
      yStart = embedded_matrix[, 2],
      yEnd = embedded_matrix[, 1],
    )
  }) %>%
  bind_rows() %>%
  mutate(x_end = rep(seq(1,3,1),140)) %>%
  mutate(x_start = rep(seq(0,2,1), 140)) %>%
  mutate(dirChange = ifelse(yEnd - yStart >0, "Increase", "Decrease"),
         segColor = ifelse(dirChange == "Increase", "red", "black")) %>%
  ggplot(., aes(group = unique_ID)) + 
  geom_segment(aes(x = x_start,y = yStart,xend = x_end,yend = yEnd,colour = dirChange)) + 
  geom_point(aes(x = x_start, y = yStart, color = dirChange)) +
  scale_color_manual(values = c("Increase" = "black", "Decrease" = "red")) +
  scale_x_continuous(
    breaks = c(0,1,2,3),          
    labels = c("Apr 30", "May 24", "Jun 08", "Jun 15")) + 
  theme(legend.position = "none",
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 12)) +
  labs(y = "Proportion suitable habtat",
       x = "Habitat sampling date")
  
# Plot no. larks detected v. suitable habitat
habitatData %>%
  select(Number_Detected,diff_pct_suit_30Apr25_24May25,pct_suitable_may_24_2025, pct_suitable_june_08_2025, pct_suitable_june_15_2025) %>%
  mutate(pct_suitable_apr_30_2025 = pct_suitable_may_24_2025 - diff_pct_suit_30Apr25_24May25) %>%
  ggplot(., aes(x = pct_suitable_apr_30_2025, y = Number_Detected)) + 
  geom_point() + geom_smooth(method = "glm",
                             method.args = list(family = poisson)) + 
  theme(legend.position = "none",
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 12)) +
  labs(y = "No. larks detected",
       x = "Proportion suitable habitat on 30 April")


## Date 2
habitatData %>%
  select(Number_Detected,diff_pct_suit_30Apr25_24May25,pct_suitable_may_24_2025, pct_suitable_june_08_2025, pct_suitable_june_15_2025) %>%
  mutate(pct_suitable_apr_30_2025 = pct_suitable_may_24_2025 - diff_pct_suit_30Apr25_24May25) %>%
  ggplot(., aes(x = pct_suitable_may_24_2025, y = Number_Detected)) + 
  geom_point() + geom_smooth(method = "glm",
                             method.args = list(family = poisson)) +
  theme(legend.position = "none",
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 12)) +
  labs(y = "No. larks detected",
       x = "Proportion suitable habitat on 24 May")

## Date 3
habitatData %>%
  select(Number_Detected,diff_pct_suit_30Apr25_24May25,pct_suitable_may_24_2025, pct_suitable_june_08_2025, pct_suitable_june_15_2025) %>%
  mutate(pct_suitable_apr_30_2025 = pct_suitable_may_24_2025 - diff_pct_suit_30Apr25_24May25) %>%
  ggplot(., aes(x = pct_suitable_june_08_2025, y = Number_Detected)) + 
  geom_point() + geom_smooth(method = "glm",
                             method.args = list(family = poisson)) + 
  theme(legend.position = "none",
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 12)) +
  labs(y = "No. larks detected",
       x = "Proportion suitable habitat on 08 June")


## Date 4
habitatData %>%
  select(Number_Detected,diff_pct_suit_30Apr25_24May25,pct_suitable_may_24_2025, pct_suitable_june_08_2025, pct_suitable_june_15_2025) %>%
  mutate(pct_suitable_apr_30_2025 = pct_suitable_may_24_2025 - diff_pct_suit_30Apr25_24May25) %>%
  ggplot(., aes(x = pct_suitable_june_15_2025, y = Number_Detected)) + 
  geom_point() + geom_smooth(method = "glm",
                             method.args = list(family = poisson))  + 
  theme(legend.position = "none",
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 12)) +
  labs(y = "No. larks detected",
       x = "Proportion suitable habitat on 15 June")
