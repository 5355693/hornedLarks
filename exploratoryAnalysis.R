library(tidyverse)
library(readxl)
library(lubridate)
library(suncalc)
library(ggpmisc)
library(unmarked)
library(AICcmodavg)
library(ubms)

# Review and organize data, changing formats and variable names as needed.
#surveyData <- read_xlsx("~/Documents/GitHub/hornedLarks/WV_SurveyOutput.xlsx")
surveyData <- read_csv("~/Documents/GitHub/hornedLarks/WV_SHLA_22.csv")
names(surveyData)[1] <- 'surveyEvent'
#surveyData$Count_Date <- mdy(surveyData$Count_Date)
surveyData$Count_Date <- mdy(surveyData$Survey_Date)
#surveyData$Start_Time <- hms(surveyData$Start_Time)
surveyData$Start_Time <- hms(surveyData$Survey_Time)
surveyData$Site_ID <- factor(surveyData$Site_ID)
surveyData$Observer <- factor(surveyData$Observer)
surveyData$Sky_Code <- factor(surveyData$Sky_Code, levels = c("0","1","2","3","4"), 
                              labels = c("Clear","Partly cloudy","Mostly cloudy","Fog or smoke","Drizzle"))
surveyData$Sex <- factor(surveyData$Sex, levels = c("M","F","U"), labels = c("Male","Female","Unknown"))
surveyData$Age <- factor(surveyData$Age, levels = c("A", "J"), labels = c("Adult", "Juvenile"))
surveyData$`Distance Band` <- factor(surveyData$`Distance Band`)
names(surveyData)[19] <- 'distanceBand'
surveyData$Interval_1 <- ifelse(surveyData$Interval_1 == "X", NA, surveyData$Interval_1)
surveyData$Interval_2 <- ifelse(surveyData$Interval_2 == "X", NA, surveyData$Interval_2)
surveyData$Interval_3 <- ifelse(surveyData$Interval_3 == "X", NA, surveyData$Interval_3)
surveyData$Interval_4 <- ifelse(surveyData$Interval_4 == "X", NA, surveyData$Interval_4)

surveyData$Interval_1 <- factor(surveyData$Interval_1, levels = c("C","S","V"), labels = 
                                  c("Calling", "Singing", "Visual"))
surveyData$Interval_2 <- factor(surveyData$Interval_2, levels = c("C","S","V"), labels = 
                                  c("Calling", "Singing", "Visual"))
surveyData$Interval_3 <- factor(surveyData$Interval_3, levels = c("C","S","V"), labels = 
                                  c("Calling", "Singing", "Visual"))
surveyData$Interval_4 <- factor(surveyData$Interval_4, levels = c("C","S","V"), labels = 
                                  c("Calling", "Singing", "Visual"))

surveyData$dayOfYear <- yday(surveyData$Count_Date) # create a day-of-year variable for analysis

## Add a "first detected by..." column to survey data:
surveyData <-
  surveyData %>%
  filter(!is.na(Sex)) %>%
  select(Sex, Lark_ID, Interval_1, Interval_2, Interval_3, Interval_4) %>%
  pivot_longer(., cols = 3:6, names_to = "interval", values_to = "detection") %>%
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
  select(Sex, Lark_ID, Interval_1, Interval_2, Interval_3, Interval_4) %>%
  pivot_longer(., cols = 3:6, names_to = "interval", values_to = "detection") %>%
  group_by(Lark_ID, Sex) %>%
  filter(!is.na(detection)) %>%
  summarise(firstDet = first(interval)) %>%
  mutate(firstInterval = ifelse(firstDet == "Interval_1",1,
                                ifelse(firstDet == "Interval_2", 2,
                                       ifelse(firstDet == "Interval_3", 3,
                                              ifelse(firstDet == "Interval_4", 4, NA))))) %>%
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

# Read in survey location and habitat data
habitatData <- read_csv("~/Documents/GitHub/hornedLarks/tbl_survey_locations_exported_09_26_2022_pct_suitable_2021.csv")
habitatData$Site_ID <- factor(habitatData$Site_ID)

habitatAndSurveyData <-
habitatData %>%
  select(Site_ID,ln_UTM1083,lt_UTM1083,ln_WGS84,lt_WGS84, pct_suitable_2021) %>%
  right_join(.,surveyData, by = 'Site_ID', keep = F)

habitatAndSurveyData$Site_ID <- factor(habitatAndSurveyData$Site_ID)
habitatAndSurveyData$Observer <- factor(habitatAndSurveyData$Observer)

# Make encounter histories for detected birds across intervals
surveyData$encounterHistory <- paste(if_else(surveyData$Interval_1 == "None", 0,1),
                                     if_else(surveyData$Interval_2 == "None", 0,1),
                                     if_else(surveyData$Interval_3 == "None", 0,1),
                                     if_else(surveyData$Interval_4 == "None", 0,1),
                                     sep = "")

# Which points are missing spatial data?
  habitatAndSurveyData %>%
  filter(is.na(ln_UTM1083)) %>%
  select(., Site_ID)

# Calculate the incidence of encounters:
surveyData %>%
  group_by(Site_ID) %>%
  summarise(larksDetected = first(Number_Detected)) %>%
  group_by(larksDetected) %>%
  summarise(count = n()) %>%
  mutate(freq = count/sum(count))

# Singing males only:
surveyData %>%
  mutate(singMale = ifelse(firstDet == "Singing", 1, 0)) %>%
  mutate(singMale = ifelse(is.na(singMale), 0, singMale)) %>%
group_by(Site_ID) %>%
  summarise(larksDetected = sum(singMale)) %>%
  group_by(larksDetected) %>%
  summarise(count = n()) %>%
  mutate(freq = count/sum(count))

# Test that we are summarizing correctly:
unique(surveyData$surveyEvent) # = 214
186+12+8+5+3 # = 215, from the summary table calculated lines 26-31

surveyData%>% 
  summarise(count = n_distinct(surveyData$Site_ID))
# plot lark detection frequencies
detectionTable <- surveyData %>%
  group_by(Site_ID) %>%
  summarise(`Larks detected` = first(Number_Detected)) %>%
  group_by(`Larks detected`) %>%
  summarise(`No. of points` = n()) %>%
  mutate(`Frequency` = sprintf("%0.2f",`No. of points`/sum(`No. of points`)))

ggplot(detectionTable, aes(x = `Larks detected`, y = `No. of points`)) + geom_col() + 
  geom_text(aes(label = `No. of points`, vjust = -0.5)) + 
  labs(x = "No. of larks detected at point", y = "No. of points") + 
  annotate("table", x = 4, y = 150, label = detectionTable)

# Calculate the number of larks detected:
surveyData %>%
  group_by(surveyEvent) %>%
  summarise(first = first(Number_Detected)) %>%
  summarise(total = sum(first)) #55


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
  geom_text(aes(label = count), vjust = 1.5, colour = "white") + 
  labs(x = "Distance band", y = "No. larks detected")

surveyData %>%
  filter(!is.na(distanceBand)) %>%
  group_by(distanceBand, firstDet) %>%
  summarise(count = n()) %>%
  ggplot(data = ., aes(x = distanceBand, y = count, fill = firstDet)) + geom_col() +
  geom_text(aes(label = count), vjust = 0.5, colour = "white") + 
  labs(x = "Distance band", y = "No. larks detected")

# Account for different areas searched
# Band 1 = 1963 m2, Band 2 = 29452 m2, Band 3 = 94248 m2, Band 4 = 376991 m2
surveyData %>%
  filter(!is.na(distanceBand)) %>%
  group_by(distanceBand) %>%
  summarise(count = n()) %>%
  mutate(adjCount = ifelse(distanceBand == 1,count/1963,
                           ifelse(distanceBand == 2, count/29452,
                                  ifelse(distanceBand == 3, count/94249,
                                         ifelse(distanceBand == 4, count/376991,0))))) %>%
ggplot(data = ., aes(x = distanceBand, y = adjCount)) + geom_col() +
  geom_text(aes(label = count), vjust = -0.6, colour = "black") + 
  labs(x = "Distance band",y = "Larks counted per sq. m. surveyed")

# Summarize sex of individuals detected
surveyData %>%
  filter(!is.na(Sex)) %>%
  group_by(Sex) %>%
  summarise(count = n()) %>%
  ggplot(data = ., aes(x = Sex, y = count)) + geom_col() + 
  geom_text(aes(label = count), vjust= 1.5, colour = "white") + 
  labs(x = "Sex", y = "No. larks counted")

# Check to see if noise is related to lark detections:
surveyData %>%
  mutate(presence = ifelse(Number_Detected>0,1,0)) %>%
  group_by(surveyEvent) %>%
  summarise(noise = first(Max_Noise), presence = first(presence)) %>%
  ggplot(.,aes(x = noise, y = presence, color = presence)) + geom_point() +
  scale_y_continuous(name = "Larks present?", breaks = c(0,1), labels = c("No","Yes")) + 
  theme(legend.position = "none", panel.grid.minor.y = element_blank()) + 
  xlab("Ambient noise (dBA)") + coord_fixed(ratio = 40) +
  stat_smooth(method="glm", se=TRUE, method.args = list(family=binomial))

# Check to see if temperature is related to lark detections:
surveyData %>%
  mutate(presence = ifelse(Number_Detected>0,1,0)) %>%
  group_by(surveyEvent) %>%
  summarise(temp = first(Temp), presence = first(presence)) %>%
  ggplot(.,aes(x = temp, y = presence, color = presence)) + geom_point() +
  scale_y_continuous(name = "Larks present?", breaks = c(0,1), labels = c("No","Yes")) + 
  theme(legend.position = "none", panel.grid.minor.y = element_blank()) + 
  coord_fixed(ratio = 20) + 
  stat_smooth(method="glm", se=TRUE, method.args = list(family=binomial)) + 
  labs(x=expression("Air temperature during survey " ( degree~F)))

mTemp <- glm(present ~ Temp, family = "binomial", data = surveyData)
summary(mTemp)

# check to see if time-of-day is related to detections
surveyData %>%
  mutate(presence = ifelse(Number_Detected>0,1,0)) %>%
  group_by(surveyEvent, mas) %>%
  summarise(temp = first(mas), presence = first(presence)) %>%
  ggplot(.,aes(x = mas, y = presence, color = presence)) + geom_point() +
  scale_y_continuous(name = "Larks present?", breaks = c(0,1), labels = c("No","Yes")) + 
  theme(legend.position = "none", panel.grid.minor.y = element_blank()) + 
  xlab("Minutes after sunrise") + coord_fixed(ratio = 100) + 
  stat_smooth(method="glm", se=TRUE, method.args = list(family=binomial))

mTimeOfDay <- glm(present ~ mas, family = "binomial", data = surveyData)
summary(mTimeOfDay)

# check to see if date is related to detections
surveyData %>%
  mutate(presence = ifelse(Number_Detected>0,1,0)) %>%
  group_by(surveyEvent, Count_Date) %>%
  summarise(dayOfYear = yday(Count_Date), presence = first(presence)) %>%
  ggplot(.,aes(x = dayOfYear, y = presence, color = presence)) + geom_point() +
  scale_y_continuous(name = "Larks present?", breaks = c(0,1), labels = c("No","Yes")) + 
  theme(legend.position = "none", panel.grid.minor.y = element_blank()) + 
  xlab("Day of the year") + coord_fixed(ratio = 20) + 
  stat_smooth(method="glm", se=TRUE, method.args = list(family=binomial))

mDayOfYear <- glm(present ~ dayOfYear, family = "binomial", data = surveyData)
summary(mDayOfYear)

# check to see if wind is related to detections
surveyData %>%
  mutate(presence = ifelse(Number_Detected>0,1,0)) %>%
  group_by(surveyEvent, Wind) %>%
  summarise(wind = first(Wind), presence = first(presence)) %>%
  ggplot(.,aes(x = wind, y = presence, color = presence)) + geom_point() +
  scale_y_continuous(name = "Larks present?", breaks = c(0,1), labels = c("No","Yes")) + 
  theme(legend.position = "none", panel.grid.minor.y = element_blank()) + 
  xlab("Wind speed") + coord_fixed(ratio = 3) + 
  stat_smooth(method="glm", se=TRUE, method.args = list(family=binomial))

# differences among observers
surveyData %>%
  mutate(presence = ifelse(Number_Detected>0,1,0)) %>%
  group_by(Observer,surveyEvent) %>%
  summarise(presence = first(presence), surveys = n_distinct(surveyEvent)) %>%
  group_by(Observer) %>%
  summarise(count = sum(presence), surveys = sum(surveys),
            incidence = count/surveys) %>%
  pivot_longer(cols = 2:3,
               names_to = "metric",
               values_to = "counts")  %>%
  ggplot(., aes(x = Observer, y = counts, fill = metric)) + 
  geom_bar(position = "dodge", stat = "identity") +
    labs(x = "Observer", y = "No. survey points",
         fill = "", caption = "(values above first bar in each group are the\npercentage of points surveyed that yielded\na detection of Streaked Horned Lark)") + scale_fill_manual(labels = c("Points with detections",
                                                   "Total points surveyed"),
                                        values = c("#F8766D","#00BFC4")) +
  geom_text(aes(label = paste(100*round(incidence,3),"%", sep = "")), 
            position = position_dodge(0.9),
            color=c("black","#00BFC4",
                    "black","#00BFC4",
                    "black","#00BFC4"),
            vjust = c(-0.2,1,-0.2,1,-0.2,1),hjust = 0.5,
            size = 3)
# Did observers tend to survey at different times of year,
# such that differences in detection rate reflect when they
# conducted most of their surveys?
ggplot(data = surveyData, aes(x = Observer, y = dayOfYear)) +
  geom_dotplot(binaxis = "y",
               stackdir = "center",
               dotsize = 0.5,
               binwidth = 1) + 
  labs(x = "Observer", y = "Day of year",
       caption = "(Points represent surveys conducted\nby the observer on a given day)")

# Did observers tend to survey at different temperatures,
# such that differences in detection rate reflect when they
# conducted most of their surveys?
surveyData %>%
  group_by(Observer, surveyEvent) %>%
  summarise(Observer = Observer, Temp = first(Temp)) %>%
ggplot(., aes(x = Observer, y = Temp)) + geom_boxplot() + 
  labs(y=expression("Air temperature during survey " ( degree~F)), x = "Observer")
 
# Is the percent of suitable habitat associated with detections?
habitatAndSurveyData %>%
  mutate(presence = ifelse(Number_Detected>0,1,0)) %>%
  group_by(surveyEvent) %>%
  summarise(habitat = first(pct_suitable_2021), presence = first(presence), number = max(Number_Detected)) %>%
  ggplot(.,aes(x = habitat, y = presence, color = number)) + geom_point(color = number, size = number) +
  scale_y_continuous(name = "Larks present?", breaks = c(0,1), labels = c("No","Yes")) + 
  theme(legend.position = "none", panel.grid.minor.y = element_blank()) + 
  xlab("Predicted proportion of suitable habitat") + coord_fixed(ratio = 1) +
  stat_smooth(method="glm", se=TRUE, method.args = list(family=binomial))

habitatAndSurveyData %>%
  mutate(presence = ifelse(Number_Detected>0,1,0)) %>%
  group_by(surveyEvent) %>%
  summarise(habitat = first(pct_suitable_2021), presence = first(presence), number = max(Number_Detected)) %>%
  ggplot(.,aes(x = habitat, y = number, color = number)) + geom_point() +
  labs(x = "Predicted proportion of suitable habitat", y = "Number of larks detected") +
  scale_color_viridis_b(option = "C", guide = NULL)

habitatAndSurveyData %>%
  ggplot(., aes(x = pct_suitable_2021, y = dayOfYear)) +
  geom_point()

# How were individuals first detected? Did it differ by sex?
surveyData %>%
  filter(!is.na(Sex)) %>%
  select(Sex, Lark_ID, Interval_1, Interval_2, Interval_3, Interval_4) %>%
  pivot_longer(., cols = 3:6, names_to = "interval", values_to = "detection") %>%
  group_by(Lark_ID, Sex) %>%
  filter(!is.na(detection)) %>%
  summarise(firstDet = first(detection)) %>%
  group_by(Sex, firstDet) %>%
  summarise(numDetected = n())


## Distance sampling.
## Exploring data for detection covariates to include

## Distances by observer
surveyData %>%
 filter(!is.na(distanceBand), firstDet == "Singing", Sex == "Male") %>%
  ggplot(., aes(x = distanceBand)) + geom_bar() + 
  facet_wrap(vars(Observer))

## By temp
surveyData %>%
  filter(!is.na(distanceBand), firstDet == "Singing", Sex == "Male") %>%
  ggplot(., aes(x = distanceBand, y = Temp)) + geom_point()

## By Day
surveyData %>%
  filter(!is.na(distanceBand), firstDet == "Singing", Sex == "Male") %>%
  ggplot(., aes(x = distanceBand, y = dayOfYear)) + geom_point()

## By Mas
surveyData %>%
  filter(!is.na(distanceBand), firstDet == "Singing", Sex == "Male") %>%
  ggplot(., aes(x = distanceBand, y = mas)) + geom_point()

## By noise
surveyData %>%
  filter(!is.na(distanceBand), firstDet == "Singing", Sex == "Male") %>%
  ggplot(., aes(x = distanceBand, y = Avg_Noise)) + geom_point()

## Create a new variable called 'distance', which translates the distance_band information into the midpoint of the distance.
## Then remove all other variables.
  ## THIS EXCLUDES ALL NON-SINGING MALES ##
dists <-
  surveyData %>%
  group_by(Site_ID) %>%
  mutate(distance = ifelse(distanceBand == 1, 12.5,
                           ifelse(distanceBand == 2, 61,
                                  ifelse(distanceBand == 3, 150,
                                         ifelse(distanceBand == 4, 300, NA))))) %>%
  select(Site_ID, distance, Sex, firstDet) %>%
  filter(!is.na(distance), firstDet == "Singing", Sex == "Male")

## Note here that we need the "as.data.frame" argument because 'dists' is a tidyverse tibble, 
## and unmarked doesn't seem to like tibbles. This forces it into a standard R data frame.
yDat <- formatDistData(distData = as.data.frame(dists), distCol = "distance", transectNameCol = "Site_ID", 
                       dist.breaks = c(0,25,100,200,400))

## Create a data frame of site-level covariates.
covs <-
  surveyData %>%
  group_by(Site_ID) %>%
  summarise(site = first(Site_ID),
            observer = first(Observer),
            temp = first(Temp),
            avgNoise = first(Avg_Noise),
            dayOfYear = first (dayOfYear),
            mas = first(mas))


umf <- unmarkedFrameDS(y = as.matrix(yDat), siteCovs = as.data.frame(covs),
                       survey = "point", dist.breaks = c(0,25,100,200,400), unitsIn = "m")
summary(umf)
hist(umf, freq = TRUE, xlab = "Distance (m)", main = "Streaked Horned Lark detections 2022", cex.lab = 0.8, cex.axis = 0.8)

# Fitting models.
# Half-normal, null
hnNull <- distsamp(~1~1, umf, keyfun = "halfnorm", output = "density", unitsOut = "kmsq")
hnNull
backTransform(hnNull, type = "state")
backTransform(hnNull, type = "det")

# calculating detection probability
sig <- exp(coef(hnNull, type="det"))
ea <- 2*pi * integrate(grhn, 0, 400, sigma=sig)$value # effective area
sqrt(ea / pi) # effective radius
# detection probability
ea / (pi*400^2)

#Half-normal, MAS
hnMAS <- distsamp(~mas ~1, umf, keyfun = "halfnorm", output = "density", unitsOut = "kmsq")
hnMAS

#Back-transformed detection probability for the average MAS
sig <- backTransform(linearComb(hnMAS['det'], c(1, mean(covs$mas))))@estimate
ea <- 2*pi * integrate(grhn, 0, 400, sigma=sig)$value # effective area
sqrt(ea / pi) # effective radius
# detection probability
ea / (pi*400^2)

# Half-normal, Day of year
hnDay <- distsamp(~dayOfYear ~1, data = umf, keyfun = "halfnorm", output = "density", unitsOut = "kmsq")
hnDay

#Back-transformed detection probability for the average day of year
sig <- backTransform(linearComb(hnDay['det'], c(1, mean(covs$dayOfYear))))@estimate
ea <- 2*pi * integrate(grhn, 0, 400, sigma=sig)$value # effective area
sqrt(ea / pi) # effective radius
# detection probability
ea / (pi*400^2)

# Half-normal, noise
hnNoise <- distsamp(~avgNoise ~1, data = umf, keyfun = "halfnorm", output = "density", unitsOut = "kmsq")
hnNoise

#Back-transformed detection probability for the average day of year
sig <- backTransform(linearComb(hnNoise['det'], c(1, mean(covs$avgNoise))))@estimate
ea <- 2*pi * integrate(grhn, 0, 400, sigma=sig)$value # effective area
sqrt(ea / pi) # effective radius
# detection probability
ea / (pi*400^2)

# Half-normal, Temp
hnTemp <- distsamp(~temp ~1, data = umf, keyfun = "halfnorm", output = "density", unitsOut = "kmsq")
hnTemp

#Back-transformed detection probability for the average day of year
sig <- backTransform(linearComb(hnTemp['det'], c(1, mean(covs$temp))))@estimate
ea <- 2*pi * integrate(grhn, 0, 400, sigma=sig)$value # effective area
sqrt(ea / pi) # effective radius
# detection probability
ea / (pi*400^2)

# Hazard-rate models
haNull <- distsamp(~1 ~1, data = umf, keyfun = "hazard", output = "density", unitsOut = "kmsq")
haNoise <- distsamp(~avgNoise ~1, data = umf, keyfun = "hazard", output = "density", unitsOut = "kmsq")
haTemp <- distsamp(~temp ~1, data = umf, keyfun = "hazard", output = "density", unitsOut = "kmsq")
haDay <- distsamp(~dayOfYear ~1, data = umf, keyfun = "hazard", output = "density", unitsOut = "kmsq")
haMAS <- distsamp(~mas ~1, data = umf, keyfun = "hazard", output = "density", unitsOut = "kmsq")

fmList <- list("haNull" = haNull, "haDay" = haDay, "haNoise" = haNoise, "haMAS" = haMAS, "haTemp" = haTemp,
               "hnNull" = hnNull, "hnDay" = hnDay, "hnNoise" = hnNoise, "hnMAS" = hnMAS, "hnTemp" = hnTemp)
tableDistanceAIC <- aictab(cand.set = fmList, second.ord = T, sort = T)

# Goodness of fit
fitstats <- function(hnDay) {
  observed <- getY(hnDay@data)
  expected <- fitted(hnDay)
  resids <- residuals(hnDay)
  sse <- sum(resids^2)
  chisq <- sum((observed - expected)^2 / expected)
  freeTuke <- sum((sqrt(observed) - sqrt(expected))^2)
  out <- c(SSE=sse, Chisq=chisq, freemanTukey=freeTuke)
  return(out)
}
(pb <- parboot(hnDay, fitstats, nsim=25, report=1))

## Prediction and plotting
## Sigma for first day of year:
backTransform(linearComb(hnDay['det'], c(1, min(covs$dayOfYear))))@estimate
## = 110.326
## Sigma for last day of year:
backTransform(linearComb(hnDay['det'], c(1, max(covs$dayOfYear))))@estimate
## = 46.94998
## Sigma for the median day:
backTransform(linearComb(hnDay['det'], c(1, median(covs$dayOfYear))))@estimate
## = 68.06919

par(mfrow=c(1, 1))
plot(function(x) gxhn(x, sigma=110.3216), 0, 400, xlab="Distance (m)",
     ylab="Detection probability", cex.lab=1,
     cex.axis=0.7, las=1, col = "green")
plot(function(x) gxhn(x, sigma=46.94988), 0, 400, add = TRUE, col = "blue")
plot(function(x) gxhn(x, sigma=68.06919), 0, 400, add = TRUE, col = "black")
legend('topright', c("First day of surveys", "Last day of surveys", "Median day of surveys"),
       col=c("green", "blue", "black"), lty=1, cex=0.8)

## Estimating p-hat with a parametric bootstrap
getP <- function(hnNull) {
  sig <- exp(coef(hnNull, type="det"))
  ea <- 2*pi * integrate(grhn, 0, 400, sigma=sig)$value # effective area
  er <- sqrt(ea / pi) # effective radius
  p <- ea / (pi*400^2) # detection probability
  out <- c(p = p, er = er)
  return(out)
}

parboot(hnNull, getP, nsim = 25, report = 1)

getPcovs <- function(hnDay) {
  sig <- backTransform(linearComb(hnDay['det'], c(1, median(covs$dayOfYear))))@estimate
  ea <- 2*pi * integrate(grhn, 0, 400, sigma=sig)$value # effective area
  er <- sqrt(ea / pi) # effective radius
  p <- ea / (pi*400^2) #detection probability
  out <- c(p = p , er = er)
  return(out)
}
getPcovs(hnDay)
parboot(hnDay, getPcovs, nsim = 25, report = 25)

## Predict seasonal changes in P-hat
## Loop function for calculating P-hat across days of the season
distanceCovPred <- matrix(nrow = 24, ncol = 7) #create a matrix for output
colnames(distanceCovPred) <- c("sigma", "ea", "er", "p","lowerCI", "upperCI", "Day") #give the columns names
for (i in 1:24) { #this loops through 24 days of the year to generate estimates of p-hat
  distanceCovPred[[i,1]] <- backTransform(linearComb(hnDay['det'], c(1, i+157)))@estimate # this is a kludge to index days (i.e., lowest value = day 158)
  distanceCovPred[[i,2]] <- 2*pi * integrate(grhn, 0, 400, sigma=distanceCovPred[[i,1]])$value # effective area
  distanceCovPred[[i,3]] <- sqrt(distanceCovPred[[i,2]] / pi) # effective radius
  distanceCovPred[[i,4]] <- distanceCovPred[[i,2]] / (pi*400^2) #detection probability
  getPcovsL <- function(hnDay) { #this defines the function that 'parboot' will use
    sig <- backTransform(linearComb(hnDay['det'], c(1, i+157)))@estimate
    ea <- 2*pi * integrate(grhn, 0, 400, sigma=sig)$value # effective area
    er <- sqrt(ea / pi) # effective radius
    p <- ea / (pi*400^2) #detection probability
    out <- c(p = p , er = er)
  } #following lines are for pulling out upper and lower 95% Ci from bootstrap; parallel process didn't work, so had to set cores = 1
  distanceCovPred[[i,5]] <- quantile(parboot(hnDay,getPcovsL, nsim = 100, report = 100, ncores = 1)@t.star[,1],probs = 0.025)
  distanceCovPred[[i,6]] <- quantile(parboot(hnDay,getPcovsL, nsim = 100, report = 100, ncores = 1)@t.star[,1], probs = 0.975)
  }

distanceCovPred[,7]<- seq(158, 181, 1)

ggplot(data = as.data.frame(distanceCovPred), aes(x = Day, y = p)) +
  geom_ribbon(aes(ymin = lowerCI, ymax = upperCI), fill = "grey80", alpha = 0.5) + 
  geom_line(aes(x = Day, y = p), color = "black") + 
  xlab("Day of year") + ylab ("Probability of detection") + 
  theme_bw()



## Estimating density with a parametric bootstrap
getD <- function(hnDay) {
  d <- backTransform(hnDay, type = "state")@estimate
  return(d)
}

parboot(hnDay, getD, nsim = 25, report = 1)

backTransform(hnDay, type = "state")

## Compare distance models if we use a different right truncation distance. In the above 
## models, we assume that nothing is detected beyond 400m. During meetings, Beth Gardner
## suggested that we look at a shorter truncation distance, as that might explain
## why estimated p-hat is so low. Below, we repeat the distance analysis assuming a maximum detection
## distance of 300 m

dists300 <-
  surveyData %>%
  group_by(Site_ID) %>%
  mutate(distance = ifelse(distanceBand == 1, 12.5,
                           ifelse(distanceBand == 2, 61,
                                  ifelse(distanceBand == 3, 150,
                                         ifelse(distanceBand == 4, 250, NA))))) %>%
  select(Site_ID, distance, Sex, firstDet) %>%
  filter(!is.na(distance), firstDet == "Singing", Sex == "Male")

## Note here that we need the "as.data.frame" argument because 'dists' is a tidyverse tibble, 
## and unmarked doesn't seem to like tibbles. This forces it into a standard R data frame.
yDat300 <- formatDistData(distData = as.data.frame(dists), distCol = "distance", transectNameCol = "Site_ID", 
                       dist.breaks = c(0,25,100,200,300))


umf300 <- unmarkedFrameDS(y = as.matrix(yDat300), siteCovs = as.data.frame(covs),
                       survey = "point", dist.breaks = c(0,25,100,200,300), unitsIn = "m")
summary(umf300)

# Fitting models.
# Half-normal
hnNull300 <- distsamp(~1~1, umf300, keyfun = "halfnorm", output = "density", unitsOut = "kmsq")
hnMAS300 <- distsamp(~mas ~1, umf300, keyfun = "halfnorm", output = "density", unitsOut = "kmsq")
hnDay300 <- distsamp(~dayOfYear ~1, data = umf300, keyfun = "halfnorm", output = "density", unitsOut = "kmsq")
hnNoise300 <- distsamp(~avgNoise ~1, data = umf300, keyfun = "halfnorm", output = "density", unitsOut = "kmsq")
hnTemp300 <- distsamp(~temp ~1, data = umf300, keyfun = "halfnorm", output = "density", unitsOut = "kmsq")

fmList300 <- list("hnNull" = hnNull300, "hnDay" = hnDay300, "hnNoise" = hnNoise300, "hnMAS" = hnMAS300, "hnTemp" = hnTemp300)
tableDistanceAIC300 <- aictab(cand.set = fmList300, second.ord = T, sort = T)

## Looking at detectability, we see a slight increase by truncating at 300 m v. 400 m.
# calculating detection probability
sig <- exp(coef(hnNull300, type="det"))
ea <- 2*pi * integrate(grhn, 0, 300, sigma=sig)$value # effective area
sqrt(ea / pi) # effective radius
# detection probability == 0.14 (v. 0.08 w/400 m)
ea / (pi*300^2)

#Back-transformed detection probability for the average day of year
sig <- backTransform(linearComb(hnDay300['det'], c(1, mean(covs$dayOfYear))))@estimate
ea <- 2*pi * integrate(grhn, 0, 300, sigma=sig)$value # effective area
sqrt(ea / pi) # effective radius
# detection probability == 0.11 (v. 0.06 w/400 m)
ea / (pi*300^2)

# Graph the two hnDay models and compare.
## First repeat the calculation for the hn400 model
## Predict seasonal changes in P-hat
## Loop function for calculating P-hat across days of the season
distanceCovPred <- matrix(nrow = 24, ncol = 8) #create a matrix for output
colnames(distanceCovPred) <- c("sigma", "ea", "er", "p","lowerCI", "upperCI", "Day", "truncationDist") #give the columns names
for (i in 1:24) { #this loops through 24 days of the year to generate estimates of p-hat
  distanceCovPred[[i,1]] <- backTransform(linearComb(hnDay['det'], c(1, i+157)))@estimate # this is a kludge to index days (i.e., lowest value = day 158)
  distanceCovPred[[i,2]] <- 2*pi * integrate(grhn, 0, 400, sigma=distanceCovPred[[i,1]])$value # effective area
  distanceCovPred[[i,3]] <- sqrt(distanceCovPred[[i,2]] / pi) # effective radius
  distanceCovPred[[i,4]] <- distanceCovPred[[i,2]] / (pi*400^2) #detection probability
  getPcovsL <- function(hnDay) { #this defines the function that 'parboot' will use
    sig <- backTransform(linearComb(hnDay['det'], c(1, i+157)))@estimate
    ea <- 2*pi * integrate(grhn, 0, 400, sigma=sig)$value # effective area
    er <- sqrt(ea / pi) # effective radius
    p <- ea / (pi*400^2) #detection probability
    out <- c(p = p , er = er)
  } #following lines are for pulling out upper and lower 95% Ci from bootstrap; parallel process didn't work, so had to set cores = 1
  distanceCovPred[[i,5]] <- quantile(parboot(hnDay,getPcovsL, nsim = 100, report = 100, ncores = 1)@t.star[,1],probs = 0.025)
  distanceCovPred[[i,6]] <- quantile(parboot(hnDay,getPcovsL, nsim = 100, report = 100, ncores = 1)@t.star[,1], probs = 0.975)
}

distanceCovPred[,7]<- seq(158, 181, 1)
distanceCovPred[,8]<- 400

#Repeat for model with a 300 m truncation distance
distanceCovPred300 <- matrix(nrow = 24, ncol = 8) #create a matrix for output
colnames(distanceCovPred300) <- c("sigma", "ea", "er", "p","lowerCI", "upperCI", "Day","truncationDist") #give the columns names
for (i in 1:24) { #this loops through 24 days of the year to generate estimates of p-hat
  distanceCovPred300[[i,1]] <- backTransform(linearComb(hnDay300['det'], c(1, i+157)))@estimate # this is a kludge to index days (i.e., lowest value = day 158)
  distanceCovPred300[[i,2]] <- 2*pi * integrate(grhn, 0, 300, sigma=distanceCovPred300[[i,1]])$value # effective area
  distanceCovPred300[[i,3]] <- sqrt(distanceCovPred300[[i,2]] / pi) # effective radius
  distanceCovPred300[[i,4]] <- distanceCovPred300[[i,2]] / (pi*300^2) #detection probability
  getPcovsL <- function(hnDay300) { #this defines the function that 'parboot' will use
    sig <- backTransform(linearComb(hnDay300['det'], c(1, i+157)))@estimate
    ea <- 2*pi * integrate(grhn, 0, 300, sigma=sig)$value # effective area
    er <- sqrt(ea / pi) # effective radius
    p <- ea / (pi*300^2) #detection probability
    out <- c(p = p , er = er)
  } #following lines are for pulling out upper and lower 95% Ci from bootstrap; parallel process didn't work, so had to set cores = 1
  distanceCovPred300[[i,5]] <- quantile(parboot(hnDay300,getPcovsL, nsim = 100, report = 100, ncores = 1)@t.star[,1],probs = 0.025)
  distanceCovPred300[[i,6]] <- quantile(parboot(hnDay300,getPcovsL, nsim = 100, report = 100, ncores = 1)@t.star[,1], probs = 0.975)
}

distanceCovPred300[,7]<- seq(158, 181, 1)
distanceCovPred300[,8]<- 300

truncationComparison <- rbind(distanceCovPred,distanceCovPred300)

ggplot(data = as.data.frame(truncationComparison, aes(x = Day, y = p, group = truncationDist))) +
  geom_ribbon(aes(y = p, x = Day, ymin = lowerCI, ymax = upperCI, fill = as.factor(truncationDist)), alpha = 0.5) + 
  geom_line(aes(x = Day, y = p, group = truncationDist, color = as.factor(truncationDist))) + 
  xlab("Day of year") + ylab ("Probability of detection") + 
  scale_color_discrete(name = "Truncation distance (m)") + scale_fill_discrete(guide = "none")

ggsave(filename = "~/Documents/GitHub/hornedLarks/truncationDistances.png")

## Distance model with STAN (won't run)
umfDSSTAND <- unmarkedFrameDS(y = as.matrix(yDat), siteCovs = as.data.frame(covs),
                       survey = "point", dist.breaks = c(0,0.025,0.1,0.2,0.4), unitsIn = "km")
distanceNullSTAN <- stan_distsamp(~1 ~1, data = umf, keyfun = "halfnorm",
                                   output = "density", unitsOut = "kmsq", chains = 3, iter = 300, cores = 3)
## Incorporating removal models
encounters <-
  surveyData %>%
  group_by(Site_ID) %>%
  mutate(interval1 = ifelse(firstInterval == 1 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval2 = ifelse(firstInterval == 2 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval3 = ifelse(firstInterval == 3 & Sex == "Male" & firstDet == "Singing", 1, 0),
         interval4 = ifelse(firstInterval == 4 & Sex == "Male" & firstDet == "Singing", 1, 0)) %>%
  select(Site_ID, interval1, interval2, interval3, interval4, Sex, firstDet) %>%
  group_by(Site_ID) %>%
  summarise(interval1 = sum(interval1), # this model wants summed # of birds per interval
            interval2 = sum(interval2), # and has to match the distance data
            interval3 = sum(interval3),
            interval4 = sum(interval4)) 

yRemoval <- matrix(nrow = 214, ncol = 4)
rownames(yRemoval) <- encounters$Site_ID
yRemoval <- cbind(encounters[,2:5])
yRemoval[is.na(yRemoval)] <- 0
## Create a new variable called 'distance', which translates the distance_band information into the midpoint of the distance.
## Then remove all other variables.
## THIS INCLUDES ONLY SINGING MALES ##
distsRemoval <-
  surveyData %>%
  group_by(Site_ID) %>%
  mutate(distance = ifelse(distanceBand == 1, 12.5,
                           ifelse(distanceBand == 2, 61,
                                  ifelse(distanceBand == 3, 150,
                                         ifelse(distanceBand == 4, 300, NA))))) %>%
  select(Site_ID, distance, Sex, firstDet) %>%
  filter(!is.na(distance), firstDet == "Singing", Sex == "Male")

## Note here that we need the "as.data.frame" argument because 'dists' is a tidyverse tibble, 
## and unmarked doesn't seem to like tibbles. This forces it into a standard R data frame.
yDatDistance <- formatDistData(distData = as.data.frame(distsRemoval), distCol = "distance", transectNameCol = "Site_ID", 
                       dist.breaks = c(0,25,100,200,400))



umfDR <- unmarkedFrameGDR(yDistance = as.matrix(yDatDistance), yRemoval = as.matrix(yRemoval), numPrimary = 1,
                          siteCovs = covs, dist.breaks = c(0,25,100,200,400), unitsIn = "m")
summary(umfDR)

drNull <- gdistremoval(lambdaformula = ~1, phiformula = ~1, removalformula = ~1,
                       distanceformula = ~1, data = umfDR, keyfun = "halfnorm",
                       output = "density", unitsOut = "kmsq", mixture = "ZIP")
summary(drNull)

## Distance removal with day-of-year as covariate on distance:
drDay <- gdistremoval(lambdaformula = ~1, phiformula = ~1, removalformula = ~1,
                      distanceformula = ~dayOfYear, data = umfDR, keyfun = "halfnorm",
                      output = "density", unitsOut = "kmsq", mixture = "ZIP")
summary(drDay)

drTemp <- gdistremoval(lambdaformula = ~1, phiformula = ~1, removalformula = ~temp,
                      distanceformula = ~dayOfYear, data = umfDR, keyfun = "halfnorm",
                      output = "density", unitsOut = "kmsq", mixture = "ZIP")
drMAS <- gdistremoval(lambdaformula = ~1, phiformula = ~1, removalformula = ~mas,
                       distanceformula = ~dayOfYear, data = umfDR, keyfun = "halfnorm",
                       output = "density", unitsOut = "kmsq", mixture = "ZIP")
drNoise <- gdistremoval(lambdaformula = ~1, phiformula = ~1, removalformula = ~avgNoise,
                       distanceformula = ~dayOfYear, data = umfDR, keyfun = "halfnorm",
                       output = "density", unitsOut = "kmsq", mixture = "ZIP")

drBest <- gdistremoval(lambdaformula = ~1, phiformula = ~1, removalformula = ~dayOfYear,
                       distanceformula = ~dayOfYear, data = umfDR, keyfun = "halfnorm",
                       output = "density", unitsOut = "kmsq", mixture = "ZIP")

drDayRemOnly <- gdistremoval(lambdaformula = ~1, phiformula = ~1, removalformula = ~dayOfYear,
                      distanceformula = ~1, data = umfDR, keyfun = "halfnorm",
                      output = "density", unitsOut = "kmsq", mixture = "ZIP")

drMASRemOnly <- gdistremoval(lambdaformula = ~1, phiformula = ~1, removalformula = ~mas,
                             distanceformula = ~1, data = umfDR, keyfun = "halfnorm",
                             output = "density", unitsOut = "kmsq", mixture = "ZIP")
backTransform(drDay, type = "lambda")

# Get p
getPdistrem <- function(x) {
  sig <- backTransform(linearComb(drDay,c(1,171), type = "dist"))
  ea <- 2*pi * integrate(grhn, 0, 400, sigma=sig@estimate)$value # effective area
  er <- sqrt(ea / pi) # effective radius
  p <- ea / (pi*400^2) #detection probability
  return(p)
}
getPdistrem()


## Function to calculate the prob. of detection,
## where p = detection prob. from distance sampling, 
## and d = detection prob. from removal sampling,
## and P*D = overall detectability.
## I think due to the uncertainty and very small estimates for each part of the equation,
## the bootstrap function is failing. Not sure how to get to a CI around P.
getPdistrem <- function(x) {
  d <- backTransform(x@estimates@estimates$rem)@estimate
  sig <- backTransform(x@estimates@estimates$dist)@estimate
  ea <- 2*pi * integrate(grhn, 0, 400, sigma=sig)$value # effective area
  er <- sqrt(ea / pi) # effective radius
  p <- ea / (pi*400^2) #detection probability
  Pd <- p*d
  return(Pd)
}
getPdistrem(drNull)

parboot(drNull, getPdistrem, nsim = 25, report = 1)

getPD <- function(x) {
  d <- backTransform(drDay, type = "rem")
  sig <- backTransform(linearComb(drDay,c(1,171), type = "dist"))
  ea <- 2*pi * integrate(grhn, 0, 400, sigma=sig@estimate)$value # effective area
  er <- sqrt(ea / pi) # effective radius
  p <- ea / (pi*400^2) #detection probability
  Pd <- p*(d@estimate)
  return(Pd)
}
getPD(drDay)
parboot(drDay, getPD, nsim = 25, report = 25)



## To show that the basic removal model produces the same estimate as the gdistremoval function
removalFrame <- unmarkedFrameMPois(y = yRemoval, siteCovs = covs, type = "removal")
removalNull <- multinomPois(~1 ~1, data = removalFrame)
removalDay <- multinomPois(~scale(dayOfYear) ~1, data = removalFrame)
removalTemp <- multinomPois(~temp ~1, data = removalFrame)
removalNoise <- multinomPois(~avgNoise ~1, data = removalFrame)
removalMAS <- multinomPois (~mas ~1, data = removalFrame)

fmRemovalList <- list("removalNull" = removalNull, "removalDay" = removalDay, "removalTemp" = removalTemp,
               "removalNoise" = removalNoise, "removalMAS" = removalMAS)
aictab(cand.set = fmRemovalList, second.ord = T, sort = T)

summary(removalNull)
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


## How do removal models compare to full encounter histories? Following code will treat the 
## surveys as capture/recapture efforts, thus retaining the full encounter history. This
## differs from the removal models, which truncate the history after the first detection.
captHistory <-
  encounters %>%
  replace(is.na(.), 0) %>%
  mutate(captureHistory = paste(interval1, interval2, interval3, interval4, sep = "")) 
  
captHistory$captureHistory <- factor(captHistory$captureHistory, 
                                     levels = c("0001","0010","0100","1000",
                                                "0011","0101","0110","1010","1001","1100",
                                                "0111","1011","1101","1110","1111"))
capSummary <- table(captHistory$Site_ID, captHistory$captureHistory)

crPiFun <- function(p) {
  p1 <- p[,1]
  p2 <- p[,2]
  p3 <- p[,3]
  p4 <- p[,4]
  cbind("0001" = (1-p1) * (1-p2) * (1-p3) * p4,
        "0010" = (1-p1) * (1-p2) * p3 * (1-p4),
        "0100" = (1-p1) * p2 * (1-p3) * (1-p4),
        "1000" = p1 * (1-p2) * (1-p3) * (1-p4),
        "0011" = (1-p1) * (1-p2) * p3 * p4,
        "0101" = (1-p1) * p2 * (1-p3) * p4,
        "0110" = (1-p1) * p2 * p3 * (1-p4),
        "1010" = p1 * (1-p2) * p3 * (1-p4),
        "1001" = p1 * (1-p2) * (1-p3) * p4,
        "1100" = p1 * p2 * (1-p3) * (1-p4),
        "0111" = (1-p1) * p2 * p3 * p4,
        "1011" = p1 * (1-p2) * p3 * p4,
        "1101" = p1 * p2 * (1-p3) * p4,
        "1110" = p1 * p2 * p3 * (1-p4),
        "1111" = p1 * p2 * p3 * p4)
}

o2y <- matrix(1, 4, 15)

class(capSummary) <- "matrix"
umf.cr1 <- unmarkedFrameMPois(y = capSummary, siteCovs = covs, piFun = "crPiFun", obsToY = o2y)
crNull <- multinomPois(~1 ~1, umf.cr1, engine = "R")
crDay <- multinomPois(~dayOfYear ~1, umf.cr1, engine = "R")
crTemp <- multinomPois(~temp ~1, data = umf.cr1, engine = "R")
crNoise <- multinomPois(~avgNoise ~1, data = umf.cr1, engine = "R")
crMAS <- multinomPois (~mas ~1, data = umf.cr1, engine = "R")

fmCRList <- list("crNull" = crNull, "crDay" = crDay, "crTemp" = crTemp,
                      "crNoise" = crNoise, "crMAS" = crMAS)
aictab(cand.set = fmRemovalList, second.ord = T, sort = T)

crNull
crDay

backTransform(crNull, type = "det")
lc <- linearComb(crDay, c(Int = 1, dayOfYear = median(covs$dayOfYear)), type = "det")
backTransform(lc)


crPredictPdata <- data.frame(dayOfYear = seq(min(covs$dayOfYear), max(covs$dayOfYear), by = 1))
crPredictP <- predict(crDay, type = "det", newdata = crPredictPdata, appendData = TRUE) 
ggplot(data = crPredictP, aes(x = dayOfYear, y = Predicted)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey80", alpha = 0.5) + 
  geom_line(aes(x = dayOfYear, y = Predicted), color = "black") + 
  xlab("Day of year") + ylab ("Availability for detection") + 
  theme_bw()



