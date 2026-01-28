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


