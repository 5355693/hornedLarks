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

# Review and organize data, changing formats and variable names as needed.
#surveyData <- read_xlsx("~/Documents/GitHub/hornedLarks/WV_SurveyOutput.xlsx")
#surveyData <- read_csv("~/Documents/GitHub/hornedLarks/WV_SHLA_22_23.csv")
surveyData <- read_csv("~/Documents/GitHub/hornedLarks/WV_SHLA_2024_11.20.24.csv")
names(surveyData)[5] <- 'surveyEvent'
surveyData$surveyEvent <- factor(surveyData$surveyEvent)
#surveyData$Count_Date <- mdy(surveyData$Count_Date)
surveyData$Count_Date <- mdy(surveyData$Survey_Date)
#surveyData$Start_Time <- hms(surveyData$Start_Time)
surveyData$Start_Time <- hms(surveyData$Survey_Time)
surveyData$Site_ID <- factor(surveyData$unique_ID)
surveyData$Observer <- factor(surveyData$Observer)
surveyData$Sky_Code <- factor(surveyData$Sky_Code, levels = c("0","1","2","3","4"), 
                              labels = c("Clear","Partly cloudy","Mostly cloudy","Fog or smoke","Drizzle"))
surveyData$Sex <- factor(surveyData$Sex, levels = c("M","F","U"), labels = c("Male","Female","Unknown"))
surveyData$Age <- factor(surveyData$Age, levels = c("A", "J"), labels = c("Adult", "Juvenile"))
#surveyData$`Distance Band` <- factor(surveyData$`Distance Band`)
#names(surveyData)[20] <- 'distanceBand'
surveyData$Interval_1 <- ifelse(surveyData$Min_1 == "X", NA, surveyData$Min_1)
surveyData$Interval_2 <- ifelse(surveyData$Min_2 == "X", NA, surveyData$Min_2)
surveyData$Interval_3 <- ifelse(surveyData$Min_3 == "X", NA, surveyData$Min_3)
surveyData$Interval_4 <- ifelse(surveyData$Min_4 == "X", NA, surveyData$Min_4)
surveyData$Interval_5 <- ifelse(surveyData$Min_5 == "X", NA, surveyData$Min_5)
surveyData$Interval_6 <- ifelse(surveyData$Min_6 == "X", NA, surveyData$Min_6)
surveyData$Interval_7 <- ifelse(surveyData$Min_7 == "X", NA, surveyData$Min_7)
surveyData$Interval_8 <- ifelse(surveyData$Min_8 == "X", NA, surveyData$Min_8)
surveyData$Interval_9 <- ifelse(surveyData$Min_9 == "X", NA, surveyData$Min_9)
surveyData$Interval_10 <- ifelse(surveyData$Min_10 == "X", NA, surveyData$Min_10)
surveyData$Interval_11 <- ifelse(surveyData$Min_11 == "X", NA, surveyData$Min_11)
surveyData$Interval_12 <- ifelse(surveyData$Min_12 == "X", NA, surveyData$Min_12)
surveyData$Interval_13 <- ifelse(surveyData$Min_13 == "X", NA, surveyData$Min_13)
surveyData$Interval_14 <- ifelse(surveyData$Min_14 == "X", NA, surveyData$Min_14)
surveyData$Interval_15 <- ifelse(surveyData$Min_15 == "X", NA, surveyData$Min_15)
surveyData$Interval_16 <- ifelse(surveyData$Min_16 == "X", NA, surveyData$Min_16)
surveyData$Interval_17 <- ifelse(surveyData$Min_17 == "X", NA, surveyData$Min_17)
surveyData$Interval_18 <- ifelse(surveyData$Min_18 == "X", NA, surveyData$Min_18)
surveyData$Interval_19 <- ifelse(surveyData$Min_19 == "X", NA, surveyData$Min_19)
surveyData$Interval_20 <- ifelse(surveyData$Min_20 == "X", NA, surveyData$Min_20)
surveyData$Interval_21 <- ifelse(surveyData$Min_21 == "X", NA, surveyData$Min_21)
surveyData$Interval_22 <- ifelse(surveyData$Min_22 == "X", NA, surveyData$Min_22)
surveyData$Interval_23 <- ifelse(surveyData$Min_23 == "X", NA, surveyData$Min_23)
surveyData$Interval_24 <- ifelse(surveyData$Min_24 == "X", NA, surveyData$Min_24)
surveyData$Interval_25 <- ifelse(surveyData$Min_25 == "X", NA, surveyData$Min_25)
surveyData$Interval_26 <- ifelse(surveyData$Min_26 == "X", NA, surveyData$Min_26)
surveyData$Interval_27 <- ifelse(surveyData$Min_27 == "X", NA, surveyData$Min_27)
surveyData$Interval_28 <- ifelse(surveyData$Min_28 == "X", NA, surveyData$Min_28)
surveyData$Interval_29 <- ifelse(surveyData$Min_29 == "X", NA, surveyData$Min_29)
surveyData$Interval_30 <- ifelse(surveyData$Min_30 == "X", NA, surveyData$Min_30)

surveyData$Interval_1 <- factor(surveyData$Interval_1, levels = c("C","S","V"), labels = 
                                  c("Calling", "Singing", "Visual"))
surveyData$Interval_2 <- factor(surveyData$Interval_2, levels = c("C","S","V"), labels = 
                                  c("Calling", "Singing", "Visual"))
surveyData$Interval_3 <- factor(surveyData$Interval_3, levels = c("C","S","V"), labels = 
                                  c("Calling", "Singing", "Visual"))
surveyData$Interval_4 <- factor(surveyData$Interval_4, levels = c("C","S","V"), labels = 
                                  c("Calling", "Singing", "Visual"))
surveyData$Interval_5 <- factor(surveyData$Interval_5, levels = c("C","S","V"), labels = 
                                  c("Calling", "Singing", "Visual"))
surveyData$Interval_6 <- factor(surveyData$Interval_6, levels = c("C","S","V"), labels = 
                                  c("Calling", "Singing", "Visual"))
surveyData$Interval_7 <- factor(surveyData$Interval_7, levels = c("C","S","V"), labels = 
                                  c("Calling", "Singing", "Visual"))
surveyData$Interval_8 <- factor(surveyData$Interval_8, levels = c("C","S","V"), labels = 
                                  c("Calling", "Singing", "Visual"))
surveyData$Interval_9 <- factor(surveyData$Interval_9, levels = c("C","S","V"), labels = 
                                  c("Calling", "Singing", "Visual"))
surveyData$Interval_10 <- factor(surveyData$Interval_10, levels = c("C","S","V"), labels = 
                                  c("Calling", "Singing", "Visual"))
surveyData$Interval_11 <- factor(surveyData$Interval_11, levels = c("C","S","V"), labels = 
                                  c("Calling", "Singing", "Visual"))
surveyData$Interval_12 <- factor(surveyData$Interval_12, levels = c("C","S","V"), labels = 
                                  c("Calling", "Singing", "Visual"))
surveyData$Interval_13 <- factor(surveyData$Interval_13, levels = c("C","S","V"), labels = 
                                  c("Calling", "Singing", "Visual"))
surveyData$Interval_14 <- factor(surveyData$Interval_14, levels = c("C","S","V"), labels = 
                                  c("Calling", "Singing", "Visual"))
surveyData$Interval_15 <- factor(surveyData$Interval_15, levels = c("C","S","V"), labels = 
                                  c("Calling", "Singing", "Visual"))
surveyData$Interval_16 <- factor(surveyData$Interval_16, levels = c("C","S","V"), labels = 
                                  c("Calling", "Singing", "Visual"))
surveyData$Interval_17 <- factor(surveyData$Interval_17, levels = c("C","S","V"), labels = 
                                  c("Calling", "Singing", "Visual"))
surveyData$Interval_18 <- factor(surveyData$Interval_18, levels = c("C","S","V"), labels = 
                                  c("Calling", "Singing", "Visual"))
surveyData$Interval_19 <- factor(surveyData$Interval_19, levels = c("C","S","V"), labels = 
                                  c("Calling", "Singing", "Visual"))
surveyData$Interval_20 <- factor(surveyData$Interval_20, levels = c("C","S","V"), labels = 
                                  c("Calling", "Singing", "Visual"))
surveyData$Interval_21 <- factor(surveyData$Interval_21, levels = c("C","S","V"), labels = 
                                  c("Calling", "Singing", "Visual"))
surveyData$Interval_22 <- factor(surveyData$Interval_22, levels = c("C","S","V"), labels = 
                                  c("Calling", "Singing", "Visual"))
surveyData$Interval_23 <- factor(surveyData$Interval_23, levels = c("C","S","V"), labels = 
                                  c("Calling", "Singing", "Visual"))
surveyData$Interval_24 <- factor(surveyData$Interval_24, levels = c("C","S","V"), labels = 
                                  c("Calling", "Singing", "Visual"))
surveyData$Interval_25 <- factor(surveyData$Interval_25, levels = c("C","S","V"), labels = 
                                  c("Calling", "Singing", "Visual"))
surveyData$Interval_26 <- factor(surveyData$Interval_26, levels = c("C","S","V"), labels = 
                                  c("Calling", "Singing", "Visual"))
surveyData$Interval_27 <- factor(surveyData$Interval_27, levels = c("C","S","V"), labels = 
                                  c("Calling", "Singing", "Visual"))
surveyData$Interval_28 <- factor(surveyData$Interval_28, levels = c("C","S","V"), labels = 
                                  c("Calling", "Singing", "Visual"))
surveyData$Interval_29 <- factor(surveyData$Interval_29, levels = c("C","S","V"), labels = 
                                  c("Calling", "Singing", "Visual"))
surveyData$Interval_30 <- factor(surveyData$Interval_30, levels = c("C","S","V"), labels = 
                                  c("Calling", "Singing", "Visual"))


surveyData$dayOfYear <- yday(surveyData$Count_Date) # create a day-of-year variable for analysis
surveyData$surveyYear <- year(surveyData$Count_Date)

## Add a "first detected by..." column to survey data:
surveyData <-
  surveyData %>%
  filter(!is.na(Sex)) %>%
  select(Sex, Lark_ID, Interval_1, Interval_2, Interval_3, Interval_4,
         Interval_5, Interval_6, Interval_7, Interval_8, Interval_9, 
         Interval_10, Interval_11, Interval_12, Interval_13, Interval_14,
         Interval_15, Interval_16, Interval_17, Interval_18, Interval_19,
         Interval_20, Interval_21, Interval_22, Interval_23, Interval_24,
         Interval_25, Interval_26, Interval_27, Interval_28, Interval_29,
         Interval_30) %>%
  pivot_longer(., cols = 3:32, names_to = "interval", values_to = "detection") %>%
  group_by(Lark_ID, Sex) %>%
  filter(!is.na(detection)) %>%
  summarise(firstDet = first(detection)) %>%
  right_join(., surveyData, by = 'Lark_ID', keep = F) %>%
  select(!(Sex.x)) %>%
  rename(Sex = Sex.y)

## Add a "first detected interval" column
surveyData <-
  surveyData %>%
  filter(!is.na(Sex)) %>%
  select(Sex, Lark_ID, Interval_1, Interval_2, Interval_3, Interval_4,
         Interval_5, Interval_6, Interval_7, Interval_8, Interval_9, 
         Interval_10, Interval_11, Interval_12, Interval_13, Interval_14,
         Interval_15, Interval_16, Interval_17, Interval_18, Interval_19,
         Interval_20, Interval_21, Interval_22, Interval_23, Interval_24,
         Interval_25, Interval_26, Interval_27, Interval_28, Interval_29,
         Interval_30) %>%
  pivot_longer(., cols = 3:32, names_to = "interval", values_to = "detection") %>%
  group_by(Lark_ID, Sex) %>%
  filter(!is.na(detection)) %>%
  summarise(firstDet = first(interval)) %>%
  mutate(firstInterval = ifelse(firstDet == "Interval_1",1,
                                ifelse(firstDet == "Interval_2", 2,
                                       ifelse(firstDet == "Interval_3", 3,
                                              ifelse(firstDet == "Interval_4", 4,
                                                     ifelse(firstDet == "Interval_5", 5,
                                                            ifelse(firstDet == "Interval_6", 6,
                                                                   ifelse(firstDet == "Interval_7", 7,
                                                                          ifelse(firstDet == "Interval_8", 8,
                                                                                 ifelse(firstDet == "Interval_9", 9,
                                                                                        ifelse(firstDet == "Interval_10", 10,
                                                                                               ifelse(firstDet == "Interval_11", 11,
                                                                                                      ifelse(firstDet == "Interval_12", 12,
                                                                                                             ifelse(firstDet == "Interval_13", 13,
                                                                                                                    ifelse(firstDet == "Interval_14", 14,
                                                                                                                           ifelse(firstDet == "Interval_15", 15,
                                                                                                                                  ifelse(firstDet == "Interval_16", 16,
                                                                                                                                         ifelse(firstDet == "Interval_17", 17,
                                                                                                                                                ifelse(firstDet == "Interval_18", 18,
                                                                                                                                                       ifelse(firstDet == "Interval_19", 19,
                                                                                                                                                              ifelse(firstDet == "Interval_20", 20,
                                                                                                                                                                     ifelse(firstDet == "Interval_21", 21,
                                                                                                                                                                            ifelse(firstDet == "Interval_22", 22,
                                                                                                                                                                                   ifelse(firstDet == "Interval_23", 23,
                                                                                                                                                                                          ifelse(firstDet == "Interval_24", 24,
                                                                                                                                                                                                 ifelse(firstDet == "Interval_25", 25,
                                                                                                                                                                                                        ifelse(firstDet == "Interval_26", 26,
                                                                                                                                                                                                               ifelse(firstDet == "Interval_27", 27,
                                                                                                                                                                                                                      ifelse(firstDet == "Interval_28", 28,
                                                                                                                                                                                                                             ifelse(firstDet == "Interval_29", 29,
                                                                                                                                                                                                                                    ifelse(firstDet == "Interval_30", 30, NA))))))))))))))))))))))))))))))) %>%
  right_join(., surveyData, by = 'Lark_ID', keep = F) %>%
  select(!(Sex.x)) %>%
  select(!firstDet.x) %>%
  rename(firstDet = firstDet.y, Sex = Sex.y)

# Get sunrise times to look at effect of survey timing in detections,
# here using the Corvallis airport as the location. We could calculate
# sunrise based on the lat/long of each survey point, but they are 
# all going to have roughly the same sunrise so for ease I'm simply estimating
# sunrise at a single, central location.

# calculate sunrise times at Corvallis Airport on each survey date.
sunriseTimes <- getSunlightTimes(date = surveyData$Count_Date, lat = 44.50, lon = -123.28,
                                 keep = c("sunrise"), tz="America/Los_Angeles")

#create a new variable in surveyData that has sunrise matched to the survey point.
#careful, here, because there is no matching function (i.e., this only works if the
#two data frames are sorted in the same order. This should be the case unless you
#sort one after calling this function).
surveyData$sunrise <- sunriseTimes$sunrise
#subtract the two times to get decimal hours after sunrise.
surveyData$mas <- 60*((surveyData$Start_Time@hour + surveyData$Start_Time@minute/60) -
                        (hour(surveyData$sunrise) + minute(surveyData$sunrise)/60))
#clean up
rm(sunriseTimes)

surveyData <- surveyData[-195,] # This row causes problems because it has removal data, but not distance data. This causes the two component matrices for unmarkedFrameGDS to conflict.
         
##Write file to CSV for easier import to reporting markdown:
write_csv(surveyData, file = "/Users/johnlloyd/Documents/GitHub/hornedLarks/surveyData2024.csv")
surveyData <- read.csv(file = "/Users/johnlloyd/Documents/GitHub/hornedLarks/surveyData2024.csv",
                       header = TRUE,
                       sep = ",")
# Calculate the incidence of encounters:
surveyData %>%
  group_by(Field_ID) %>% #109 unique survey locations in 2024
  summarise(larksDetected = first(Number_Detected)) %>%
  group_by(larksDetected) %>%
  summarise(count = n()) %>%
  mutate(freq = count/sum(count)) %>%
  ggplot(., aes(x = as.factor(larksDetected), y = freq)) + 
  geom_col() + labs(x = "No. of larks detected", y = "Proportion of survey points")

# Singing males only:
surveyData %>%
  mutate(singMale = ifelse(firstDet == "Singing", 1, 0)) %>%
  mutate(singMale = ifelse(is.na(singMale), 0, singMale)) %>%
  group_by(surveyYear, Site_ID) %>%
  summarise(larksDetected = first(Number_Detected)) %>%
  group_by(surveyYear,larksDetected) %>%
  summarise(count = n()) %>%
  mutate(freq = count/sum(count)) %>%
  ggplot(., aes(x = as.factor(larksDetected), y = freq)) + 
  geom_col() + labs(x = "No. of singing larks detected", y = "Proportion of survey points")

# Summarize and visualize distance of detections
# These values will be used to label the panels in the subsequent plot.
panel_names <- c(
  `Distance_1` = "First interval",
  `Distance_2` = "Second interval",
  `Distance_3` = "Third interval",
  `PB_Distance_4` = "Fourth interval\n(first playback)",
  `PB_Distance_5` = "Fifth interval\n(second playback)"
)

# Calculate the median detection distance (first only) for use as labels in the subsequent plot.
medians <- surveyData %>%
  select(Distance_1, Distance_2, Distance_3, PB_Distance_4, PB_Distance_5, Lark_ID) %>%
  pivot_longer(cols = c(Distance_1, Distance_2, Distance_3, PB_Distance_4, PB_Distance_5), names_to = "interval", values_to = "distance") %>%
  group_by(Lark_ID) %>%
  drop_na() %>%
  filter(row_number()==1) %>%
  group_by(interval) %>%
  summarize(median = round(median(distance),0))

# Make and label the plot.
surveyData %>%
  select(Distance_1, Distance_2, Distance_3, PB_Distance_4, PB_Distance_5, Lark_ID) %>%
  pivot_longer(cols = c(Distance_1, Distance_2, Distance_3, PB_Distance_4, PB_Distance_5), names_to = "interval", values_to = "distance") %>%
  group_by(Lark_ID) %>%
  drop_na() %>%
  filter(row_number()==1) %>%
  group_by(interval) %>%
  ggplot(., aes(x = distance)) + geom_histogram() + facet_wrap(vars(interval), labeller = as_labeller(panel_names)) + 
  geom_vline(data = medians, aes(xintercept=median), color = "red") + 
  geom_text(data = medians, aes(x = 290, y = 10, label = paste0("Median detection\ndistance: ", median," m")),
            size = 3) + 
  labs(y = "No. of new detections", x = "Distance (m)")
ggsave("/Users/johnlloyd/Documents/GitHub/hornedLarks/medianDetDistances.png", width = 6, height = 4, units = "in")

# Break out by on- and off-road
mediansOn <- surveyData %>%
  filter(OffRoad == 0) %>%
  select(Distance_1, Distance_2, Distance_3, PB_Distance_4, PB_Distance_5, Lark_ID) %>%
  pivot_longer(cols = c(Distance_1, Distance_2, Distance_3, PB_Distance_4, PB_Distance_5), names_to = "interval", values_to = "distance") %>%
  group_by(Lark_ID) %>%
  drop_na() %>%
  filter(row_number()==1) %>%
  group_by(interval) %>%
  summarize(median = round(median(distance),0))

surveyData %>%
  filter(OffRoad == 0) %>%
  select(Distance_1, Distance_2, Distance_3, PB_Distance_4, PB_Distance_5, Lark_ID) %>%
  pivot_longer(cols = c(Distance_1, Distance_2, Distance_3, PB_Distance_4, PB_Distance_5), names_to = "interval", values_to = "distance") %>%
  group_by(Lark_ID) %>%
  drop_na() %>%
  filter(row_number()==1) %>%
  group_by(interval) %>%
  ggplot(., aes(x = distance)) + geom_histogram() + facet_wrap(vars(interval), labeller = as_labeller(panel_names)) + 
  geom_vline(data = mediansOn, aes(xintercept=median), color = "red") + 
  geom_text(data = mediansOn, aes(x = 290, y = 10, label = paste0("Median detection\ndistance: ", median," m")),
            size = 3) + 
  labs(y = "No. of new detections", x = "Distance (m)")
ggsave("/Users/johnlloyd/Documents/GitHub/hornedLarks/medianDetDistancesOnRoad.png", width = 6, height = 4, units = "in")

mediansOff <- surveyData %>%
  filter(OffRoad == 1) %>%
  select(Distance_1, Distance_2, Distance_3, PB_Distance_4, PB_Distance_5, Lark_ID) %>%
  pivot_longer(cols = c(Distance_1, Distance_2, Distance_3, PB_Distance_4, PB_Distance_5), names_to = "interval", values_to = "distance") %>%
  group_by(Lark_ID) %>%
  drop_na() %>%
  filter(row_number()==1) %>%
  group_by(interval) %>%
  summarize(median = round(median(distance),0))

surveyData %>%
  filter(OffRoad == 1) %>%
  select(Distance_1, Distance_2, Distance_3, PB_Distance_4, PB_Distance_5, Lark_ID) %>%
  pivot_longer(cols = c(Distance_1, Distance_2, Distance_3, PB_Distance_4, PB_Distance_5), names_to = "interval", values_to = "distance") %>%
  group_by(Lark_ID) %>%
  drop_na() %>%
  filter(row_number()==1) %>%
  group_by(interval) %>%
  ggplot(., aes(x = distance)) + geom_histogram() + facet_wrap(vars(interval), labeller = as_labeller(panel_names)) + 
  geom_vline(data = mediansOff, aes(xintercept=median), color = "red") + 
  geom_text(data = mediansOff, aes(x = 290, y = 10, label = paste0("Median detection\ndistance: ", median," m")),
            size = 3) + 
  labs(y = "No. of new detections", x = "Distance (m)")
ggsave("/Users/johnlloyd/Documents/GitHub/hornedLarks/medianDetDistancesOffRoad.png", width = 6, height = 4, units = "in")


surveyData %>%
  select(firstInterval) %>%
  ggplot(., aes(x = firstInterval)) + geom_histogram() + 
  labs (y = "No. of new detections", x = "Count minute")
ggsave("/Users/johnlloyd/Documents/GitHub/hornedLarks/medianDetTime.png", width = 6, height = 4, units = "in")

## Incorporating removal models
encounters <-
  surveyData %>%
  group_by(Field_ID) %>%
  mutate(interval1 = ifelse(firstInterval == 1 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval2 = ifelse(firstInterval == 2 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval3 = ifelse(firstInterval == 3 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval4 = ifelse(firstInterval == 4 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval5 = ifelse(firstInterval == 5 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval6 = ifelse(firstInterval == 6 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval7 = ifelse(firstInterval == 7 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval8 = ifelse(firstInterval == 8 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval9 = ifelse(firstInterval == 9 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval10 = ifelse(firstInterval == 10 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval11 = ifelse(firstInterval == 11 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval12 = ifelse(firstInterval == 12 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval13 = ifelse(firstInterval == 13 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval14 = ifelse(firstInterval == 14 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval15 = ifelse(firstInterval == 15 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval16 = ifelse(firstInterval == 16 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval17 = ifelse(firstInterval == 17 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval18 = ifelse(firstInterval == 18 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval19 = ifelse(firstInterval == 19 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval20 = ifelse(firstInterval == 20 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval21 = ifelse(firstInterval == 21 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval22 = ifelse(firstInterval == 22 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval23 = ifelse(firstInterval == 23 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval24 = ifelse(firstInterval == 24 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval25 = ifelse(firstInterval == 25 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval26 = ifelse(firstInterval == 26 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval27 = ifelse(firstInterval == 27 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval28 = ifelse(firstInterval == 28 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval29 = ifelse(firstInterval == 29 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval30 = ifelse(firstInterval == 30 & Sex == "Male" & firstDet == "Singing", 1, 0)) %>%
  select(Sex, firstDet, Field_ID, interval1, interval2, interval3, interval4,
         interval5, interval6, interval7, interval8, interval9, interval10,
         interval11, interval12, interval13, interval14, interval15, interval16, 
         interval17, interval18, interval19, interval20, interval21, interval22, 
         interval23, interval24, interval25, interval26, interval27, interval28,
         interval29, interval30) %>%
  group_by(Field_ID) %>%
  summarise(interval1 = sum(interval1), # this model wants summed # of birds per interval
            interval2 = sum(interval2), # and has to match the distance data
            interval3 = sum(interval3),
            interval4 = sum(interval4),
            interval5 = sum(interval5),
            interval6 = sum(interval6),
            interval7 = sum(interval7),
            interval8 = sum(interval8),
            interval9 = sum(interval9),
            interval10 = sum(interval10),
            interval11 = sum(interval11),
            interval12 = sum(interval12),
            interval13 = sum(interval13),
            interval14 = sum(interval14),
            interval15 = sum(interval15),
            interval16 = sum(interval16),
            interval17 = sum(interval17),
            interval18 = sum(interval18),
            interval19 = sum(interval19),
            interval20 = sum(interval20),
            interval21 = sum(interval21),
            interval22 = sum(interval22),
            interval23 = sum(interval23),
            interval24 = sum(interval24),
            interval25 = sum(interval25),
            interval26 = sum(interval26),
            interval27 = sum(interval27),
            interval28 = sum(interval28),
            interval29 = sum(interval29),
            interval30 = sum(interval30)) 

yRemoval <- matrix(nrow = 109, ncol = 31)
rownames(yRemoval) <- encounters$Field_ID
yRemoval <- cbind(encounters[,2:31])
yRemoval[is.na(yRemoval)] <- 0
yRemoval <- as.matrix(yRemoval)
## Create a new variable called 'distance', which sums the number of birds by distance class at each point.
## Then remove all other variables.
## THIS INCLUDES ONLY SINGING MALES ##
distances <- surveyData %>%
  group_by(Field_ID) %>%
  mutate(firstDistance = ifelse(!is.na(Distance_1) & firstDet == "Singing", Distance_1, 
                                ifelse(is.na(Distance_1)&!is.na(Distance_2) & firstDet == "Singing", Distance_2,
                                       ifelse(is.na(Distance_1)&is.na(Distance_2)&!is.na(Distance_3) & firstDet == "Singing", Distance_3,
                                              ifelse(is.na(Distance_1)&is.na(Distance_2)&is.na(Distance_3)&!is.na(PB_Distance_4) & firstDet == "Singing", PB_Distance_4,
                                                     ifelse(is.na(Distance_1)&is.na(Distance_2)&is.na(Distance_3)&is.na(PB_Distance_4)&!is.na(PB_Distance_5) & firstDet == "Singing", PB_Distance_5, NA)))))) %>% 
  select(firstDet, firstDistance) %>% 
  mutate(distanceBand = case_when(
    between(firstDistance, 0, 50) ~ 1,
    between(firstDistance, 51,100) ~2,
    between(firstDistance, 101, 200) ~3,
    between(firstDistance, 201, 400) ~4)) %>%
  group_by(Field_ID, distanceBand) %>%
  summarize(count = n()) %>%
  select(distanceBand, count) %>%
  pivot_wider(id_cols = Field_ID, names_from = distanceBand, values_from = count, names_sort = TRUE, values_fill = 0,
              names_prefix = "yDist.") %>%
  select(Field_ID,yDist.1, yDist.2, yDist.3, yDist.4)

yDistances <- matrix(nrow = 109, ncol = 4)
rownames(yDistances) <- distances$Field_ID
yDistances <- cbind(distances[,2:5])
#yDistances[is.na(yDistances)] <- 0
yDistances <- as.matrix(yDistances)
## This can also be accomplished by:
unfDistData <- surveyData %>%
  group_by(Field_ID) %>%
  mutate(firstDistance = ifelse(!is.na(Distance_1) & firstDet == "Singing", Distance_1, 
                                ifelse(is.na(Distance_1)&!is.na(Distance_2) & firstDet == "Singing", Distance_2,
                                       ifelse(is.na(Distance_1)&is.na(Distance_2)&!is.na(Distance_3) & firstDet == "Singing", Distance_3,
                                              ifelse(is.na(Distance_1)&is.na(Distance_2)&is.na(Distance_3)&!is.na(PB_Distance_4) & firstDet == "Singing", PB_Distance_4,
                                                     ifelse(is.na(Distance_1)&is.na(Distance_2)&is.na(Distance_3)&is.na(PB_Distance_4)&!is.na(PB_Distance_5) & firstDet == "Singing", PB_Distance_5, NA)))))) %>%
  select(firstDistance,Field_ID)

fDistData <- formatDistData(distData = as.data.frame(unfDistData), distCol = "firstDistance", transectNameCol = "Field_ID",
                            dist.breaks = c(0,50,100,200,400))


## Create a data frame of site-level covariates.
covs <-
  surveyData %>%
  group_by(Field_ID) %>%
  summarise(site = first(Field_ID),
            observer = first(Observer),
            temp = first(Temp),
            dayOfYear = first (dayOfYear),
            mas = first(mas))

## Create the unmarked frame
umfDR <- unmarkedFrameGDR(yDistance = as.matrix(fDistData), yRemoval = yRemoval, numPrimary = 1,
                          siteCovs = covs, dist.breaks = c(0,50,100,200,400), unitsIn = "m")
summary(umfDR)

## initial distance-removal (DR) models.
drNull <- gdistremoval(lambdaformula = ~1, phiformula = ~1, removalformula = ~1,
                       distanceformula = ~1, data = umfDR, keyfun = "halfnorm",
                       output = "density", unitsOut = "kmsq", mixture = "ZIP")
summary(drNull)
backTransform(drNull)
getP(drNull) #interval- and distance-specific estimates of P

t<-getP(drNull) # assign to a matrix so we can play with them
sum(t$dist[1,,]) # This is overall P from the distance model, adding up the band-specific probabilities shown in getP
plot(seq(1,30,1), t$rem[1,,]) #Plot the change in detection rates across time intervals.
z<-1-t$rem[1,,] #probability of NOT detecting a bird during each interval
prod(z) # overall probability of NOT detecting a bird
1-prod(z) # overall probability of detecting a bird
sum(t$dist[1,,])*(1-prod(z)) # combined perceptibility (distance) and availability (removal), so the probability of detecting a bird within 400 m given it sings within a 30-minute period.

sum(t$rem[1,1:12,])
## Do this again but assuming only 1 visit.
encountersSingle <-
  surveyData %>%
  group_by(Field_ID) %>%
  filter(Visit == 1) %>%
  mutate(interval1 = ifelse(firstInterval == 1 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval2 = ifelse(firstInterval == 2 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval3 = ifelse(firstInterval == 3 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval4 = ifelse(firstInterval == 4 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval5 = ifelse(firstInterval == 5 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval6 = ifelse(firstInterval == 6 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval7 = ifelse(firstInterval == 7 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval8 = ifelse(firstInterval == 8 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval9 = ifelse(firstInterval == 9 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval10 = ifelse(firstInterval == 10 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval11 = ifelse(firstInterval == 11 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval12 = ifelse(firstInterval == 12 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval13 = ifelse(firstInterval == 13 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval14 = ifelse(firstInterval == 14 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval15 = ifelse(firstInterval == 15 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval16 = ifelse(firstInterval == 16 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval17 = ifelse(firstInterval == 17 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval18 = ifelse(firstInterval == 18 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval19 = ifelse(firstInterval == 19 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval20 = ifelse(firstInterval == 20 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval21 = ifelse(firstInterval == 21 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval22 = ifelse(firstInterval == 22 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval23 = ifelse(firstInterval == 23 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval24 = ifelse(firstInterval == 24 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval25 = ifelse(firstInterval == 25 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval26 = ifelse(firstInterval == 26 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval27 = ifelse(firstInterval == 27 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval28 = ifelse(firstInterval == 28 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval29 = ifelse(firstInterval == 29 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval30 = ifelse(firstInterval == 30 & Sex == "Male" & firstDet == "Singing", 1, 0)) %>%
  select(Sex, firstDet, Field_ID, interval1, interval2, interval3, interval4,
         interval5, interval6, interval7, interval8, interval9, interval10,
         interval11, interval12, interval13, interval14, interval15, interval16, 
         interval17, interval18, interval19, interval20, interval21, interval22, 
         interval23, interval24, interval25, interval26, interval27, interval28,
         interval29, interval30) %>%
  group_by(Field_ID) %>%
  summarise(interval1 = sum(interval1), # this model wants summed # of birds per interval
            interval2 = sum(interval2), # and has to match the distance data
            interval3 = sum(interval3),
            interval4 = sum(interval4),
            interval5 = sum(interval5),
            interval6 = sum(interval6),
            interval7 = sum(interval7),
            interval8 = sum(interval8),
            interval9 = sum(interval9),
            interval10 = sum(interval10),
            interval11 = sum(interval11),
            interval12 = sum(interval12),
            interval13 = sum(interval13),
            interval14 = sum(interval14),
            interval15 = sum(interval15),
            interval16 = sum(interval16),
            interval17 = sum(interval17),
            interval18 = sum(interval18),
            interval19 = sum(interval19),
            interval20 = sum(interval20),
            interval21 = sum(interval21),
            interval22 = sum(interval22),
            interval23 = sum(interval23),
            interval24 = sum(interval24),
            interval25 = sum(interval25),
            interval26 = sum(interval26),
            interval27 = sum(interval27),
            interval28 = sum(interval28),
            interval29 = sum(interval29),
            interval30 = sum(interval30)) 

yRemoval1 <- matrix(nrow = 60, ncol = 31)
rownames(yRemoval1) <- encountersSingle$Field_ID
yRemoval1 <- cbind(encountersSingle[,2:31])
yRemoval1[is.na(yRemoval1)] <- 0
yRemoval1 <- as.matrix(yRemoval1)

## Same for distances:
distances.1 <- surveyData %>%
  group_by(Field_ID) %>%
  filter(Visit == 1) %>%
  mutate(firstDistance = ifelse(!is.na(Distance_1) & firstDet == "Singing", Distance_1, 
                                ifelse(is.na(Distance_1)&!is.na(Distance_2) & firstDet == "Singing", Distance_2,
                                       ifelse(is.na(Distance_1)&is.na(Distance_2)&!is.na(Distance_3) & firstDet == "Singing", Distance_3,
                                              ifelse(is.na(Distance_1)&is.na(Distance_2)&is.na(Distance_3)&!is.na(PB_Distance_4) & firstDet == "Singing", PB_Distance_4,
                                                     ifelse(is.na(Distance_1)&is.na(Distance_2)&is.na(Distance_3)&is.na(PB_Distance_4)&!is.na(PB_Distance_5) & firstDet == "Singing", PB_Distance_5, NA)))))) %>% 
  select(firstDet, firstDistance) %>% 
  mutate(distanceBand = case_when(
    between(firstDistance, 0, 50) ~ 1,
    between(firstDistance, 51,100) ~2,
    between(firstDistance, 101, 200) ~3,
    between(firstDistance, 201, 400) ~4)) %>%
  group_by(Field_ID, distanceBand) %>%
  summarize(count = n()) %>%
  select(distanceBand, count) %>%
  pivot_wider(id_cols = Field_ID, names_from = distanceBand, values_from = count, names_sort = TRUE, values_fill = 0,
              names_prefix = "yDist.") %>%
  select(Field_ID,yDist.1, yDist.2, yDist.3, yDist.4)

yDistances.1 <- matrix(nrow = 60, ncol = 5)
rownames(yDistances.1) <- distances.1$Field_ID
yDistances.1 <- cbind(distances.1[,2:5])
#yDistances[is.na(yDistances)] <- 0
yDistances.1 <- as.matrix(yDistances.1)

## Create the site covs for the single visit analysis:
## Create a data frame of site-level covariates.
covs.1 <-
  surveyData %>%
  filter(Visit == 1) %>%
  group_by(Field_ID) %>%
  summarise(site = first(Field_ID),
            observer = first(Observer),
            temp = first(Temp),
            dayOfYear = first (dayOfYear),
            mas = first(mas))

## Create the unmarked frame for single visit analysis:
umfDR.1 <- unmarkedFrameGDR(yDistance = as.matrix(yDistances.1), yRemoval = yRemoval1, numPrimary = 1,
                          siteCovs = covs.1, dist.breaks = c(0,50,100,200,400), unitsIn = "m")
summary(umfDR.1)

## distance-removal (DR) models for single visit analysis
drNull.1 <- gdistremoval(lambdaformula = ~1, phiformula = ~1, removalformula = ~1,
                       distanceformula = ~1, data = umfDR.1, keyfun = "halfnorm",
                       output = "density", unitsOut = "kmsq", mixture = "ZIP")

getP(drNull.1) #interval- and distance-specific estimates of P

t.1<-getP(drNull.1) # assign to a matrix so we can play with them
sum(t.1$dist[1,,]) # This is overall P from the distance model, adding up the band-specific probabilities shown in getP
plot(seq(1,30,1), t.1$rem[1,,]) #Plot the change in detection rates across time intervals.
z.1<-1-t.1$rem[1,,] #probability of NOT detecting a bird during each interval
prod(z.1) # overall probability of NOT detecting a bird
1-prod(z.1) # overall probability of detcting a bird
sum(t.1$dist[1,,])*(1-prod(z.1)) # combined perceptibility (distance) and availability (removal), so the probability of detecting a bird within 400 m given it sings within a 30-minute period.
1-prod(z.1[1:2])

#The interval-by-interval change in overall detectability (availability):
dt8 <- sapply(2:30, function(n) 1-prod(z.1[1:n])) # This takes the product of the first 2 intervals, first 3, so on.
plot(seq(2,30,1),dt8,
     xlab = "Duration of count (minutes)",
     ylab = "Cumulative probability of detection") #plot

dt8_df <- data.frame(Detectability = round(dt8,3), Minute = seq(2,30,1))

dt8_df %>%
  ggplot(., aes(x = Minute, y = Detectability)) + geom_point() + 
  annotate(geom = 'table', x = 30, y = 0, label = list(dt8_df)) + 
  labs(y = "Cumulative probability of detection", x = "Minute")
ggsave("/Users/johnlloyd/Documents/GitHub/hornedLarks/detectabilityByMinute.pdf", width = 8, height = 8, units = "in")


increment_dt8 <- dt8 %>%
  as_tibble() %>%
  mutate(diff = lead(value) - value) %>%
  slice(1:(n()-1)) %>%
  pull(diff)
print(increment_dt8)


op <- par(mar = c(5,7,4,2) + 0.1)
plot(seq(2,29,1), increment_dt8,
     xlab = "Interval start minute",
     ylab = "Incremental increase in overall\nprobability of detection")

## Repeat for 2, 8-minute counts:
encounters.8 <-
  surveyData %>%
  group_by(Field_ID) %>%
  mutate(interval1 = ifelse(firstInterval == 1 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval2 = ifelse(firstInterval == 2 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval3 = ifelse(firstInterval == 3 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval4 = ifelse(firstInterval == 4 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval5 = ifelse(firstInterval == 5 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval6 = ifelse(firstInterval == 6 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval7 = ifelse(firstInterval == 7 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval8 = ifelse(firstInterval == 8 & Sex == "Male" & firstDet == "Singing", 1, 0)) %>%
  select(Sex, firstDet, Field_ID, interval1, interval2, interval3, interval4,
         interval5, interval6, interval7, interval8) %>%
  group_by(Field_ID) %>%
  summarise(interval1 = sum(interval1), # this model wants summed # of birds per interval
            interval2 = sum(interval2), # and has to match the distance data
            interval3 = sum(interval3),
            interval4 = sum(interval4),
            interval5 = sum(interval5),
            interval6 = sum(interval6),
            interval7 = sum(interval7),
            interval8 = sum(interval8)) 

yRemoval.8 <- matrix(nrow = 109, ncol = 9)
rownames(yRemoval.8) <- encounters.8$Field_ID
yRemoval.8 <- cbind(encounters.8[,2:9])
yRemoval.8[is.na(yRemoval.8)] <- 0
yRemoval.8 <- as.matrix(yRemoval.8)

## Same for distances:
distances.8 <- surveyData %>%
  group_by(Field_ID) %>%
  mutate(firstDistance = ifelse(!is.na(Distance_1) & firstDet == "Singing", Distance_1, NA)) %>% 
  select(firstDet, firstDistance) %>% 
  mutate(distanceBand = case_when(
    between(firstDistance, 0, 50) ~ 1,
    between(firstDistance, 51,100) ~2,
    between(firstDistance, 101, 200) ~3,
    between(firstDistance, 201, 400) ~4)) %>%
  group_by(Field_ID, distanceBand) %>%
  summarize(count = n()) %>%
  select(distanceBand, count) %>%
  pivot_wider(id_cols = Field_ID, names_from = distanceBand, values_from = count, names_sort = TRUE, values_fill = 0,
              names_prefix = "yDist.") %>%
  select(Field_ID,yDist.1, yDist.2, yDist.3, yDist.4)

yDistances.8 <- matrix(nrow = 109, ncol = 5)
rownames(yDistances.8) <- distances.8$Field_ID
yDistances.8 <- cbind(distances.8[,2:5])
#yDistances[is.na(yDistances)] <- 0
yDistances.8 <- as.matrix(yDistances.8)

## Create the unmarked frame for single visit analysis:
umfDR.8 <- unmarkedFrameGDR(yDistance = as.matrix(yDistances.8), yRemoval = yRemoval.8, numPrimary = 1,
                            siteCovs = covs, dist.breaks = c(0,50,100,200,400), unitsIn = "m")
summary(umfDR.8)

## distance-removal (DR) models for single visit analysis
drNull.8 <- gdistremoval(lambdaformula = ~1, phiformula = ~1, removalformula = ~1,
                         distanceformula = ~1, data = umfDR.8, keyfun = "halfnorm",
                         output = "density", unitsOut = "kmsq", mixture = "ZIP")

getP(drNull.8) #interval- and distance-specific estimates of P

t.8<-getP(drNull.8) # assign to a matrix so we can play with them
sum(t.8$dist[1,,]) # This is overall P from the distance model, adding up the band-specific probabilities shown in getP
plot(seq(1,8,1), t.8$rem[1,,]) #Plot the change in detection rates across time intervals.
z.8<-1-t.8$rem[1,,] #probability of NOT detecting a bird during each interval
prod(z.8) # overall probability of NOT detecting a bird
1-prod(z.8) # overall probability of detcting a bird
sum(t.8$dist[1,,])*(1-prod(z.8)) # combined perceptibility (distance) and availability (removal), so the probability of detecting a bird within 400 m given it sings within a 30-minute period.


## Are playback intervals similar to passive intervals?
pb <- 1-t$rem[1,25:30,]
1-prod(pb) #0.04

pp.1 <- 1-t$rem[1,1:8,]
1-prod(pp.1) #0.4285741

pp.2 <- 1-t$rem[1,9:16,]
1-prod(pp.2) #0.2232033

pp.3 <- 1-t$rem[1,17:24,]
1-prod(pp.3) #0.1088672


ppPB <- data.frame("detect" = c(0.0422962, 0.4285741, 0.2232033,0.1088672), "minute" = c("Playback", "Passive 1", "Passive 2", "Passive 3"))

ggplot(ppPB, aes(x = minute, y = detect)) + geom_col() + 
  labs(y = "Detectability (availability)", x = "Interval")
ggsave("/Users/johnlloyd/Documents/GitHub/hornedLarks/passivePlayback.png", width = 6, height = 4, units = "in")

# Compare detectability at on v. off-road counts for a 12-minute count. 
# We cannot use the distance-removal models for this analysis because the counts for the distance
# portion of the model will not match the counts for the removal portion of the model. This is
# because the removal data are collected at 1-minute intervals, whereas the distance data are
# collected at 8-minute intervals. Thus, with a 12-minute interval, any detections made during
# the intervals between minute 12 and minute 16 will have a record for distance but not a record
# for removal.
encounters.12 <-
  surveyData %>%
  group_by(Field_ID) %>%
  mutate(interval1 = ifelse(firstInterval == 1 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval2 = ifelse(firstInterval == 2 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval3 = ifelse(firstInterval == 3 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval4 = ifelse(firstInterval == 4 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval5 = ifelse(firstInterval == 5 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval6 = ifelse(firstInterval == 6 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval7 = ifelse(firstInterval == 7 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval8 = ifelse(firstInterval == 8 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval9 = ifelse(firstInterval == 9 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval10 = ifelse(firstInterval == 10 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval11 = ifelse(firstInterval == 11 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval12 = ifelse(firstInterval == 12 & Sex == "Male" & firstDet == "Singing", 1, 0)) %>%
  select(Sex, firstDet, Field_ID, interval1, interval2, interval3, interval4,
         interval5, interval6, interval7, interval8, interval9, interval10, interval11, interval12) %>%
  group_by(Field_ID) %>%
  summarise(interval1 = sum(interval1), # this model wants summed # of birds per interval
            interval2 = sum(interval2), # and has to match the distance data
            interval3 = sum(interval3),
            interval4 = sum(interval4),
            interval5 = sum(interval5),
            interval6 = sum(interval6),
            interval7 = sum(interval7),
            interval8 = sum(interval8),
            interval9 = sum(interval9),
            interval10 = sum(interval10),
            interval11 = sum(interval11),
            interval12 = sum(interval12)) 

yRemoval.12 <- matrix(nrow = 109, ncol = 13)
rownames(yRemoval.12) <- encounters.12$Field_ID
yRemoval.12 <- cbind(encounters.12[,2:13])
yRemoval.12[is.na(yRemoval.12)] <- 0
yRemoval.12 <- as.matrix(yRemoval.12)

## Create a data frame of site-level covariates.
covs12 <-
  surveyData %>%
  group_by(Field_ID) %>%
  summarise(site = first(Field_ID),
            observer = first(Observer),
            temp = first(Temp),
            dayOfYear = first (dayOfYear),
            mas = first(mas),
            offRoad = as.factor(first(OffRoad)))

## Removal moodels to contrast on- and off-road counts
removalFrame <- unmarkedFrameMPois(y = yRemoval.12, siteCovs = covs12, type = "removal")
removalNull <- multinomPois(~1 ~1, data = removalFrame)
removalRoad <- multinomPois(~offRoad ~1, data = removalFrame)
summary(removalRoad)
summary(removalNull)

# Detection probability after 1 removal interval
predict(removalNull, type = "det")[,1]

# Detection probability if counted for 12 intervals:
rowSums(getP(removalNull))[1]

newData <- data.frame(offRoad = factor(x= c(0,1), levels = c(0,1)))
predict(removalRoad,newdata = newData, type = "det")
getP(removalRoad, type = "det")[1,]
getP(removalRoad, type = "det")[58,]

1-prod(1-getP(removalRoad)[1,]) #on-road 
1-prod(1-getP(removalRoad)[58,]) #off-road

# What do detection times look like for on- and off-road points?
roadLabs <- c("On-road", "Off-road")
names(roadLabs) <- c(0,1)

surveyData %>%
  select(firstInterval, OffRoad) %>%
  ggplot(., aes(x = firstInterval)) + geom_histogram() +
  facet_wrap(facets = vars(OffRoad), labeller = labeller(OffRoad = roadLabs)) + 
  labs (y = "No. of new detections", x = "Count minute") 
ggsave("/Users/johnlloyd/Documents/GitHub/hornedLarks/onOffRoadDetectTimes.png", width = 6, height = 4, units = "in")


backTransform(linearComb(removalRoad['det'], c(1,1)))@estimate

# STAN model of the same:
removalNullCompSTAN <- stan_multinomPois(~1 ~1, removalFrameComp, chains=3, iter=300, cores = 3)
head(predict(removalNullCompSTAN, submodel="det"))

removalDaySTAN
removalDaySTANframe <- plot_effects(removalDaySTAN, "det")


removalDay <- multinomPois(~scale(dayOfYear) ~1, data = removalFrame)
removalTemp <- multinomPois(~temp ~1, data = removalFrame)
removalNoise <- multinomPois(~avgNoise ~1, data = removalFrame)
removalMAS <- multinomPois (~mas ~1, data = removalFrame)

fmRemovalList <- list("removalNull" = removalNull, "removalDay" = removalDay, "removalTemp" = removalTemp,
                      "removalNoise" = removalNoise, "removalMAS" = removalMAS)
aictab(cand.set = fmRemovalList, second.ord = T, sort = T)

summary(removalNull)
summary(removalNullComp)
removalNull['det']
backTransform(removalNull, type = "det")
backTransform(removalNullComp, type = "det")

backTransform(linearComb(removalDay['det'], c(1, min(covs$dayOfYear))))@estimate
backTransform(linearComb(removalDay['det'], c(1,dayOfYear = min(covs$dayOfYear,0))))@estimate
backTransform(removalDay, type = "state")

lc <- linearComb(removalDay, c(Int = 1, dayOfYear = median(scale(covs$dayOfYear))), type = "det")
backTransform(lc)

removalPredictPdata <- data.frame(dayOfYear = seq(min(covs$dayOfYear), max(covs$dayOfYear), by = 1))
removalPredictP <- predict(removalDay, type = "det", newdata = removalPredictPdata, appendData = TRUE) 
ggplot(data = removalPredictP, aes(x = dayOfYear, y = Predicted)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey80", alpha = 0.5) + 
  geom_line(aes(x = dayOfYear, y = Predicted), color = "black") + 
  xlab("Day of year") + ylab ("Availability for detection") + 
  theme_bw()

# STAN model of the same:
removalDaySTAN <- stan_multinomPois(~scale(dayOfYear) ~1, removalFrame, chains=3, iter=300, cores = 3)
removalDaySTAN
removalDaySTANframe <- plot_effects(removalDaySTAN, "det")

ggplot(data = removalDaySTANframe$data, aes(x = covariate, y = mn)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey80", alpha = 0.5) + 
  geom_line(aes(x = covariate, y = mn), color = "black") + 
  xlab("Day of year") + ylab ("Availability for detection") + 
  theme_bw()



