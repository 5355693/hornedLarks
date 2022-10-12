library(tidyverse)
library(readxl)
library(lubridate)
library(suncalc)
library(ggpmisc)
library(unmarked)

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
surveyData$Interval_1 <- factor(surveyData$Interval_1, levels = c("C","S","V","X"), labels = 
                                  c("Calling", "Singing", "Visual","None"))
surveyData$Interval_2 <- factor(surveyData$Interval_2, levels = c("C","S","V","X"), labels = 
                                  c("Calling", "Singing", "Visual","None"))
surveyData$Interval_3 <- factor(surveyData$Interval_3, levels = c("C","S","V","X"), labels = 
                                  c("Calling", "Singing", "Visual","None"))
surveyData$Interval_4 <- factor(surveyData$Interval_4, levels = c("C","S","V","X"), labels = 
                                  c("Calling", "Singing", "Visual","None"))

surveyData$dayOfYear <- yday(surveyData$Count_Date) # create a day-of-year variable for analysis

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
habitatData <- read_csv("tbl_survey_locations_exported_09_26_2022_pct_suitable_2021.csv")

habitatAndSurveyData <-
habitatData %>%
  select(Site_ID,ln_UTM1083,lt_UTM1083,ln_WGS84,lt_WGS84, pct_suitable_2021) %>%
  right_join(.,surveyData, by = 'Site_ID', keep = F)

habitatAndSurveyData$Site_ID <- factor(habitatAndSurveyData$Site_ID)
habitatAndSurveyData$Observer <- factor(habitatAndSurveyData$Observer)
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
  geom_text(aes(label = count), vjust = 1.5, colour = "white") + 
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

## Distance sampling.
## Create a new variable called 'distance', which translates the distance_band information into the midpoint of the distance.
## Then remove all other variables.
dists <-
surveyData %>%
  group_by(Site_ID) %>%
  mutate(distance = ifelse(distanceBand == 1, 12.5,
                           ifelse(distanceBand == 2, 61,
                                  ifelse(distanceBand == 3, 150,
                                         ifelse(distanceBand == 4, 300, NA))))) %>%
  select(Site_ID, distance) %>%
  filter(!is.na(distance))

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

hnMAS <- distsamp(~mas ~1, umf, keyfun = "halfnorm", output = "density", unitsOut = "kmsq")
hnMAS
backTransform(hnMAS, type = "state")

hnDay <- distsamp(~dayOfYear ~1, data = umf, keyfun = "halfnorm", output = "density", unitsOut = "kmsq")
hnDay
backTransform(hnDay, type = "state")
backTransform(linearComb(hnDay['det'], c(1, mean(covs$dayOfYear))))

#Back-transformed detection probability for the average day of year
sig <- backTransform(linearComb(hnDay['det'], c(1, mean(covs$dayOfYear))))@estimate
ea <- 2*pi * integrate(grhn, 0, 400, sigma=sig)$value # effective area
sqrt(ea / pi) # effective radius
# detection probability
ea / (pi*400^2)




