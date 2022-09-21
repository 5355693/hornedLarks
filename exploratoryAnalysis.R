library(tidyverse)
library(readxl)
library(lubridate)

# Review and organize data, changing formats and variable names as needed.
surveyData <- read_xlsx("~/Documents/GitHub/hornedLarks/WV_SurveyOutput.xlsx")
names(surveyData)[1] <- 'surveyEvent'
surveyData$Count_Date <- mdy(surveyData$Count_Date)
surveyData$Start_Time <- hms(surveyData$Start_Time)
surveyData$Sky_Code <- factor(surveyData$Sky_Code, levels = c("0","1","2","3","4"), 
                              labels = c("Clear","Partly cloudy","Mostly cloudy","Fog or smoke","Drizzle"))
surveyData$Sex <- factor(surveyData$Sex, levels = c("M","F","U"), labels = c("Male","Female","Unknown"))
surveyData$Age <- factor(surveyData$Age, levels = c("A", "J"), labels = c("Adult", "Juvenile"))
surveyData$`Distance Band` <- factor(surveyData$`Distance Band`)
names(surveyData)[19] <- 'distanceBand'
surveyData$Interval_1 <- factor(surveyData$Interval_1, levels = c("C","S","V","X"), labels = 
                                  c("Calling", "Singing", "Visual","None"))
surveyData$Interval_2 <- factor(surveyData$Interval_2, levels = c("C","S","V","X"), labels = 
                                  c("Calling", "Singing", "Visual","None"))
surveyData$Interval_3 <- factor(surveyData$Interval_3, levels = c("C","S","V","X"), labels = 
                                  c("Calling", "Singing", "Visual","None"))
surveyData$Interval_4 <- factor(surveyData$Interval_4, levels = c("C","S","V","X"), labels = 
                                  c("Calling", "Singing", "Visual","None"))

# Calculate the incidence of encounters:
surveyData %>%
  group_by(surveyEvent, Site_ID) %>%
  summarise(larksDetected = first(Number_Detected)) %>%
  group_by(larksDetected) %>%
  summarise(count = n()) %>%
  mutate(freq = count/sum(count))

# Test that we are summarizing correctly:
unique(surveyData$surveyEvent) # = 215
187+12+8+5+3 # = 215, from the summary table calculated lines 26-31

# Calculate the number of larks detected:

surveyData %>%
  group_by(surveyEvent) %>%
  summarise(first = first(Number_Detected)) %>%
  summarise(total = sum(first)) #55

# Make encounter histories for detected birds across intervals
surveyData$encounterHistory <- paste(if_else(surveyData$Interval_1 == "None", 0,1),
                                     if_else(surveyData$Interval_2 == "None", 0,1),
                                     if_else(surveyData$Interval_3 == "None", 0,1),
                                     if_else(surveyData$Interval_4 == "None", 0,1),
                                     sep = "")

# Summarize encounter histories
surveyData %>%
  group_by(encounterHistory) %>%
  summarise(count = n()) #23/55 had detection in only 1 interval.

# Summarize and visualize distance of detections
surveyData %>%
  filter(!is.na(distanceBand)) %>%
  group_by(distanceBand) %>%
  summarise(count = n()) %>%
ggplot(data = ., aes(x = distanceBand, y = count)) + geom_col() +
  geom_text(aes(label = count), vjust = 1.5, colour = "white")

# Summarize sex of individuals detected
surveyData %>%
  filter(!is.na(Sex)) %>%
  group_by(Sex) %>%
  summarise(count = n()) %>%
  ggplot(data = ., aes(x = Sex, y = count)) + geom_col() + 
  geom_text(aes(label = count), vjust= 1.5, colour = "white")
