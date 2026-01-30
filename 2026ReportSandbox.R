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

larkData <- read.csv(file = "/Users/johnlloyd/Documents/GitHub/hornedLarks/surveyData23_26.csv",header = TRUE,sep = ",")
larkData$surveyYear <- factor(larkData$surveyYear)

summary_table <- 
  larkData %>%
  group_by(surveyYear) %>%
  summarize(sum = sum(Interval_1, Interval_2, Interval_3, Interval_4, Interval_5,
                      Interval_6,Interval_7,Interval_8,Interval_9,Interval_10,Interval_11,
                      Interval_12,Interval_13,Interval_14,Interval_15,Interval_16,Interval_17,
                      Interval_18,Interval_19,Interval_20,Interval_21,Interval_22,Interval_23,
                      Interval_24, na.rm = TRUE))
colnames(summary_table)[1] <- "Year"
colnames(summary_table)[2] <- "Count"
summary_table %>%
  gt() %>%
  tab_header(
    title = "Horned Lark detections per year"
  ) %>%
  fmt_number(
    columns = Count,
    decimals = 0
  )


larkData %>%
  group_by(surveyYear) %>%
  summarize(sum = sum(Interval_1, Interval_2, Interval_3, Interval_4, Interval_5,
            Interval_6,Interval_7,Interval_8,Interval_9,Interval_10,Interval_11,
            Interval_12,Interval_13,Interval_14,Interval_15,Interval_16,Interval_17,
            Interval_18,Interval_19,Interval_20,Interval_21,Interval_22,Interval_23,
            Interval_24, na.rm = TRUE)) %>%
  ggplot(., aes(x = surveyYear, y = sum)) + geom_col() + 
  ylab("Total no. larks detected") + xlab("Year") +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 14))

effort_summary_table1 <- 
  larkData %>%
  group_by(surveyYear) %>%
  summarize(count = n_distinct(site))
            
effort_summary_table2 <-
  larkData %>%
  mutate(siteDay = paste(site,"_",dayOfYear)) %>%
  group_by(surveyYear) %>%
  summarize(count = n_distinct(siteDay))
 
effort_summary_table <- merge(effort_summary_table1, effort_summary_table2, by = "surveyYear")
effort_summary_table$Survey_Minutes <- c(214*8,359*8,109*24,140*12)

colnames(effort_summary_table)[1] <- "Year"
colnames(effort_summary_table)[2] <- "No. sites"
colnames(effort_summary_table)[3] <- "No. surveys"
colnames(effort_summary_table)[4] <- "Survey minutes"
effort_summary_table %>%
  gt() %>%
  tab_header(
    title = "Survey effort per year"
  ) %>%
  tab_footnote(
    footnote = "Not including an additional 6-minute playback period at the end of each survey, data from which were not used in this analysis.",
    locations = cells_body(columns = `Survey minutes`, rows = 3),
    placement = "right"
  )

#702 unique sites surveyed over 4 years
larkData %>%
  summarize(sites = n_distinct(site))

count_summary_table <-
larkData %>%
  mutate(siteDay = paste(site,"_",dayOfYear)) %>%
  group_by(surveyYear,siteDay) %>%
  summarize(sum = sum(Interval_1, Interval_2, Interval_3, Interval_4, Interval_5,
                      Interval_6,Interval_7,Interval_8,Interval_9,Interval_10,Interval_11,
                      Interval_12,Interval_13,Interval_14,Interval_15,Interval_16,Interval_17,
                      Interval_18,Interval_19,Interval_20,Interval_21,Interval_22,Interval_23,
                      Interval_24, na.rm = TRUE)) %>%
  group_by(surveyYear) %>%
  count(sum, name = "Freq") %>%
  arrange(desc(surveyYear),sum)

colnames(count_summary_table)[1] <- "Year"
colnames(count_summary_table)[2] <- "No. of larks"
colnames(count_summary_table)[3] <- "No. surveys"
count_summary_table %>%
  gt() %>%
  tab_header(
    title = "Lark encounters per year"
  ) 


testy<-
larkData %>%
  filter(surveyYear == 2024) %>%
  mutate(siteDay = paste(site,"_",dayOfYear)) %>%
  group_by(siteDay,surveyYear) %>%
  summarize(sum = sum(Interval_1, Interval_2, Interval_3, Interval_4, Interval_5,
                      Interval_6,Interval_7,Interval_8,Interval_9,Interval_10,Interval_11,
                      Interval_12,Interval_13,Interval_14,Interval_15,Interval_16,Interval_17,
                      Interval_18,Interval_19,Interval_20,Interval_21,Interval_22,Interval_23,
                      Interval_24, na.rm = TRUE)) %>%
  group_by(surveyYear) %>%
  count(sum, name = "Freq") %>%
  arrange(desc(surveyYear),sum)

# 2024
surveyData24 <- read_csv("~/Documents/GitHub/hornedLarks/WV_SHLA_2024_11.20.24.csv")
surveyData25 <- read_csv("~/Documents/GitHub/hornedLarks/WV_SHLA_data_22_Sep_2025.csv",n_max = 159)

distance_table_2024 <-
surveyData24 %>%
  select(Distance_1, Distance_2, Distance_3) %>%
  pivot_longer(., 1:3, names_to = "Distance", values_to = "Meters") %>%
  filter(!is.na(Meters))
distance_table_2024 <- distance_table_2024[-1]

distance_table_2025 <-
surveyData25 %>%
  select(Distance_1) %>%
  filter(!is.na(Distance_1)) %>%
  rename(Meters = Distance_1)

distance_table_all <- rbind(distance_table_2024, distance_table_2025)

distance_table_all %>%
  ggplot(., aes(x = Meters)) + geom_histogram() + 
  geom_vline(aes(xintercept = 110), color = "red", linetype = "dashed", size = 1) + 
  ylab("No. of detections") + xlab("Distance to observer (m)") + 
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 14))

all_data_23 <- read.csv("~/Documents/GitHub/hornedLarks/all_data_2023.csv")
all_data_24 <- read.csv("~/Documents/GitHub/hornedLarks/all_data_2024.csv")
all_data_25 <- read.csv("~/Documents/GitHub/hornedLarks/all_data_2025.csv")

intervals <- data.frame(c(all_data_23$firstInterval, all_data_24$firstInterval, all_data_25$firstInterval))
colnames(intervals) <- "Interval"
ggplot(intervals, aes(x = Interval)) + geom_histogram(binwidth = 1) +
  ylab("No. of detections") + xlab("Interval") + 
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 14))
