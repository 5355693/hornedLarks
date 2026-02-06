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
library(msm)

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
        axis.text = element_text(size = 14))\

## Detection and abundance.
surveyData <- read.csv(file = "/Users/johnlloyd/Documents/GitHub/hornedLarks/surveyDataAll.csv",
                       header = TRUE,
                       sep = ",")

## piFun to handle variable interval lengths.
## in 2022, 2023: Interval 1, 2, 3, and 4 = 2 minutes each.
## in all other years, intevals = 1 minute.

# matrix of interval lengths:

times_mat <- matrix(NA,822,24)
times_mat[1:573,1:4] <- 2
times_mat[1:573,5:24] <- 0
times_mat[574:682,1:24] <- 1
times_mat[683:822,1:12] <- 1
times_mat[683:822,13:24] <- 0

## Factory that returns a piFun using a SITE-BY-INTERVAL times matrix
makeRemPiFun_bySite <- function(times_mat) {
  stopifnot(is.matrix(times_mat))
  function(p) {
    # p is an M x J matrix of per-unit-time detection probabilities (0..1)
    M <- nrow(p); J <- ncol(p)
    if (!all(dim(times_mat) == c(M, J)))
      stop("times_mat must have the same dimensions as p (sites x intervals).")
    
    # Convert per-unit p to per-interval detection prob q = 1 - (1 - p)^t
    q <- 1 - (1 - p)^times_mat
    
    # No time => no chance to detect in that interval
    q[is.na(times_mat) | times_mat <= 0] <- 0
    
    # Survival (not yet detected) up to the start of interval j
    surv <- matrix(1, M, J)
    if (J > 1) for (j in 2:J) surv[, j] <- surv[, j - 1] * (1 - q[, j - 1])
    
    # Multinomial cell probabilities: first detected in interval j
    pi <- surv * q
    
    # Return M x J matrix
    pi
  }
}

remPi <- makeRemPiFun_bySite(times_mat)

## need obsToY in this case because we can't use the default removal model features due to unequal intervals
make_obsToY_removal <- function(J) {
  stopifnot(J >= 1L)
  o2y <- diag(J)
  o2y[upper.tri(o2y)] <- 1L
  o2y
}

J <- 24
o2y <- make_obsToY_removal(J)
dim(o2y)

yRemoval <- matrix(nrow = 822, ncol = 24)
rownames(yRemoval) <- surveyData$site
yRemoval <- cbind(surveyData[,6:29])
yRemoval <- as.matrix(yRemoval)

## Create a data frame of site-level covariates.
covs <-
  surveyData %>%
  group_by(site, surveyYear, dayOfYear) %>%
  summarise(site = first(site),
            dayOfYear = first (dayOfYear),
            mas = first(mas))

## Create the unmarked frame
umfR <- unmarkedFrameMPois(y = yRemoval, siteCovs = covs, obsToY = o2y, piFun = "remPi")
summary(umfR)

## initial removal models.
### Detectability
dNull <- multinomPois(~1 ~1, data = umfR)
dYear <- multinomPois(~surveyYear ~1, data = umfR)
dDay <- multinomPois(~dayOfYear ~1, data = umfR)
dTime <- multinomPois(~mas ~1, data = umfR)

Model_List_Detect <- fitList(Null = dNull, Year = dYear, Day = dDay, Time = dTime)
Model_Selection_Detect <- modSel(Model_List_Detect, nullmod = "Null")
Model_Selection_Detect #The null is only 1.49 AIC above best model (Time), so prefer null.
Model_Selection_Detect.df <- as.data.frame(Model_Selection_Detect@Full$model)
Model_Selection_Detect.df$AIC <- Model_Selection_Detect@Full$AIC
Model_Selection_Detect.df$Weight <- Model_Selection_Detect@Full$AICwt

colnames(Model_Selection_Detect.df)[1] <- "Model"
colnames(Model_Selection_Detect.df)[2] <- "AIC"
colnames(Model_Selection_Detect.df)[3] <- "Model weight"

Model_Selection_Detect.df %>%
  gt() %>%
  tab_header(
    title = "Detectability models ranked."
  ) %>%
  fmt_number(
    columns = `Model weight`,
    decimals = 2
  ) %>%
  fmt_number(
    columns = AIC,
    decimals = 0
  )

ps <- data.frame(getP(dNull)[574,])
colnames(ps) <- "P"

ps <-
  ps %>%
  mutate(detection_rate = cumsum(P))

ps$Interval <- seq(1,24,1)

ps %>%
  ggplot(., aes(x = Interval, y = detection_rate)) + geom_line() + 
  labs(x = "Interval (minute)", y = "Cumulative probability\nof detection") + 
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 14))
  

predict(dNull, type = "det")[1,]



### Abundance
aNull <- multinomPois(~1 ~1, data = umfR)
aYear <- multinomPois(~1 ~surveyYear, data = umfR)
Model_List_Abund <- fitList(Null = aNull, Year = aYear)
Model_Selection_Abund <-modSel(Model_List_Abund, nullmod = "Null")
Model_Selection_Abund #Null preferred

predict(aNull, type = "state")
