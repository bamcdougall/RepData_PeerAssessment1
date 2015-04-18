################################################################################
##
##  Author:  Brendan McDougall
##  Proj Purpose: Project 1 of Reproducible Research / Johns Hopkins Univ
##  File Purpose: Acquire "experimental" data, transform into analytic data
##                  execute high-level summary, and report work as Literate
##                  Statistical program
##  MOOC:  Coursera
##  Course ID:  repdata-013
##  Date:  4/16/15
##
##
################################################################################
##
##  System Info:  Windows 7, 64 bit, i7 processor, RStudio Version 0.98.1102
##                  R x64 3.1.2, git scm 1.9.5
##
################################################################################
##
## Revision History
##
##      4/9/15: Downloaded raw data; generated local clone of GitHub Fork;
##              reviewed initial files
##
##                  
##                  
##                  
##
##      
##
################################################################################
##
##  Methdology:
##  (1a) Load dyplyr library, so table manipulation is easier;
##  (1b) Load Lubridate library, so date handing is "R-managed";
##  (1c) Load the lattice library, so plotting is similar to template
##  (2a) Read data -> (2b) Transform exptl data into analytic data;
##  (3)  Analyze data
##  (4)Plot data per assignment instruction generating
##      plots and a png file with each plot.
##
################################################################################
##
## Part (1a, 1b, 1c):
##
library(dplyr)
library(lubridate)
library(lattice)
##
################################################################################
##
## Part (2a & 2b):
##
curDir <- getwd()
fileList <- dir()
##
##  raw data imported into R using tbl_df and chaining (%>%) data transformations
##  per the workflow of the dplyr library, e.g. tbl_df vs data.frame
##
fitBitTblDF <- tbl_df(
    read.csv2("activity.csv", sep = ",", stringsAsFactors = FALSE, na.strings = "NA",
              header = TRUE, # colClasses = c("character", "numeric", "numeric")
    )
)  %>%
    mutate(
        min = formatC(interval, big.mark=":", big.interval=2, width=4, flag = "0"),
        UTCdate = ymd( paste(date)) + hm(min),
        dayNum = yday(UTCdate),
        wDay = wday(UTCdate),
        hourDec = hour(UTCdate) + minute(UTCdate)/60 + second(UTCdate)/3600
    ) %>%
    select(
        UTCdate, min, dayNum, wDay, hourDec, interval, steps
    )%>%
    filter(
        dayNum != 275 & dayNum != 335
        & dayNum != 282 & dayNum != 306 & dayNum != 309 & dayNum != 314 & dayNum != 315 & dayNum != 319
        #         steps != "NA"
    )
print(fitBitTblDF)
##
################################################################################
##
## (3) Analyze Data
##
## What is meant total number of steps taken per day?
fitBitSummaryDayNum <-
    fitBitTblDF %>%
    group_by(dayNum) %>%
    summarize(
        count = n(),
        meanIntervalSteps = mean(steps, na.rm = TRUE),
        cumDailySteps = sum(steps, na.rm = TRUE)
    ) %>%
    filter(cumDailySteps != 0) %>%
    arrange(dayNum)

# Print result to console
print(fitBitSummaryDayNum)

## Six number summary by day

summaryNumbersByDay <- summary(select(fitBitSummaryDayNum,cumDailySteps))
medianDailySteps <- summaryNumbersByDay[3]
meanDailySteps <- summaryNumbersByDay[4]
print(medianDailySteps)
print(meanDailySteps)

## What is the average daily activity pattern?
fitBitSummaryMin <-
    fitBitTblDF %>%
    group_by(hourDec) %>%
    summarize(
        count = n(),
        meanIntervalStepsbyInterval = mean(steps, na.rm = TRUE),
        sdIntervalStepsbyInterval =  sd(steps, na.rm = TRUE)
    ) %>%
    filter(meanIntervalStepsbyInterval != 0) %>%
    arrange(hourDec)

# Print result to console
print(fitBitSummaryMin)

## Six number summary by interval

summaryNumbersByMin <- summary(select(fitBitSummaryMin,meanIntervalStepsbyInterval))
medianDailyStepsByMin <- summaryNumbersByMin[3]
meanDailyStepsByMin <- summaryNumbersByMin[4]
print(medianDailyStepsByMin)
print(meanDailyStepsByMin)

summarySDnumbersByMin <- summary(select(fitBitSummaryMin,sdIntervalStepsbyInterval))
medianDailyStepsSDByMin <- summarySDnumbersByMin[3]
meanDailyStepsSDByMin <- summarySDnumbersByMin[4]
print(medianDailyStepsSDByMin)
print(meanDailyStepsSDByMin)



## Imputing missing values
## Generate the fitBit table data.frame again, but not filter out NA rows

fitBitTblDFimput <- tbl_df(
    read.csv2("activity.csv", sep = ",", stringsAsFactors = FALSE, na.strings = "NA",
              header = TRUE, # colClasses = c("character", "numeric", "numeric")
    )
)  %>%
    mutate(
        min = formatC(interval, big.mark=":", big.interval=2, width=4, flag = "0"),
        UTCdate = ymd( paste(date)) + hm(min),
        dayNum = yday(UTCdate),
        wDay = wday(UTCdate),
        hourDec = hour(UTCdate) + minute(UTCdate)/60 + second(UTCdate)/3600
    ) %>%
    select(
        UTCdate, dayNum, wDay, hourDec, steps
    )
## Generate a radomized step-count data
randomized = vector("numeric", length = dim(fitBitTblDFimput)[1])
scaleFactor = 0.65 # scaling factor to reduce upward bias of imputed data
for (i in 1:dim(fitBitTblDFimput)[1]){
    randomized[i] = max(0, scaleFactor * as.numeric(strsplit(meanDailyStepsByMin, ":")[[1]][2]) + 
                            rnorm(1, mean = 0, sd=
                                      as.numeric(strsplit(meanDailyStepsSDByMin, ":")[[1]][2])))
}
fitBitTblDFimput <- tbl_df(cbind(fitBitTblDFimput, randomized)) %>%
    mutate(
        dayFactor = ifelse(wDay %in% c(2, 3, 4, 5, 6), c("weekday"), c("weekend")),
        stepsImput = ifelse(is.na(steps), randomized, steps),
        dayFactor = as.factor(dayFactor)
    ) %>%
    select(
        UTCdate, dayNum, wDay, hourDec, steps, randomized, stepsImput, dayFactor
    )%>%
    filter(
        #         dayNum != 275 & dayNum != 335
        #         & dayNum != 282 & dayNum != 306 & dayNum != 309 & dayNum != 314 & dayNum != 315 & dayNum != 319
        #steps != "NA" 
    )
print(fitBitTblDFimput)

## What is meant total number of steps taken per day for imputed data?
fitBitSummaryDayNumImput <-
    fitBitTblDFimput %>%
    group_by(dayNum) %>%
    summarize(
        count = n(),
        meanIntervalStepsImput = mean(stepsImput, na.rm = TRUE),
        cumDailyStepsImput = sum(stepsImput, na.rm = TRUE)
    ) %>%
    arrange(dayNum)

# Print result to console
print(fitBitSummaryDayNum)

## Six number summary by day for imputed data

summaryNumbersByDayImput <- summary(select(fitBitSummaryDayNumImput,cumDailyStepsImput))
medianDailyStepsImput <- summaryNumbersByDayImput[3]
meanDailyStepsImput <- summaryNumbersByDayImput[4]
print(medianDailyStepsImput)
print(meanDailyStepsImput)

## What is the average daily activity pattern with Imputed Data?
fitBitSummaryMinImput <-
    fitBitTblDFimput %>%
    group_by(hourDec) %>%
    summarize(
        count = n(),
        meanIntervalStepsbyIntervalImput = mean(stepsImput, na.rm = TRUE),
        sdIntervalStepsbyIntervalImput =  sd(stepsImput, na.rm = TRUE)
    ) %>%
    arrange(hourDec)

# Print result to console
print(fitBitSummaryMinImput)

## Six number summary by interval with Imputed Data

summaryNumbersByMinImput <- summary(select(fitBitSummaryMinImput,meanIntervalStepsbyIntervalImput))
medianDailyStepsByMinImput <- summaryNumbersByMinImput[3]
meanDailyStepsByMinImput <- summaryNumbersByMinImput[4]
print(medianDailyStepsByMinImput)
print(meanDailyStepsByMinImput)

summarySDnumbersByMinImput <- summary(select(fitBitSummaryMinImput,sdIntervalStepsbyIntervalImput))
medianDailyStepsSDByMinImput <- summarySDnumbersByMinImput[3]
meanDailyStepsSDByMinImput <- summarySDnumbersByMinImput[4]
print(medianDailyStepsSDByMinImput)
print(meanDailyStepsSDByMinImput)






## Are there differences in activity patterns between weekdays and weekends?



##
################################################################################
##
## (4) Plot Data
##
##  Generate a scatterplot of Global Active Power vs Time
##  (1) Title = "Activity:  Tracking Steps vs Time with Fit Bit"
##  (2) ylabel = "Steps [#]"
##  (3) xlabel = "Day" , "Steps [#]", or NONE
##  (4) Save plot to a PNG file with width = 480 px and height = 480 px
windows(width=6.6667,height=6.6667)   # in windows, sets screen display to 480pX480p
par(
    mfrow = c(1, 1),        # explicitly set plot device to draw 1 graph
    mar = c(4, 4, 2, 1),    # explicitly set margins around plot to default class values
    oma = c(1, 1, 1, 1)     # explicitly set outer margin such that a line of text could be added
)

print(
    histogram(
        ~ cumDailySteps, data = fitBitSummaryDayNum, type = "count",
        main = "Activity:  Tracking Steps vs Time with Fit Bit",
        ylab="Days [#]", xlab = "Steps per Day [#]"
    )
)
# ##
# ################################################################################
# ##
# windows(width=6.6667,height=6.6667)   # in windows, sets screen display to 480pX480p
# par(    
#     mfrow = c(1, 1),        # explicitly set plot device to draw 1 graph
#     mar = c(4, 4, 2, 1),    # explicitly set margins around plot to default class values
#     oma = c(1, 1, 1, 1)     # explicitly set outer margin such that a line of text could be added
# )
# 
# print(
#     xyplot(steps ~ UTCdate, data = fitBitTblDF,
#            type = "l", lty=1, col = "black",
#            main = "Activity:  Tracking Steps vs Time with Fit Bit",
#            ylab="Steps during 5 min Intervals [#]", xlab = "Time [UTC Date]"
#     )
# )
# ##
# ################################################################################
# ##
# windows(width=6.6667,height=6.6667)   # in windows, sets screen display to 480pX480p
# par(
#     mfrow = c(1, 1),        # explicitly set plot device to draw 1 graph
#     mar = c(4, 4, 2, 1),    # explicitly set margins around plot to default class values
#     oma = c(1, 1, 1, 1)     # explicitly set outer margin such that a line of text could be added
# )
# 
# print(
#     xyplot(cumDailySteps ~ dayNum, data = fitBitSummaryDayNum,
#            type = "l", lty=1, col = "black",
#            main = "Activity:  Tracking Steps vs Time with Fit Bit",
#            ylab="Cumulative Daily Steps [#]", xlab = "Time [UTC Day]"
#     )
# )
# ##
################################################################################
##
windows(width=6.6667,height=6.6667)   # in windows, sets screen display to 480pX480p
par(
    mfrow = c(1, 1),        # explicitly set plot device to draw 1 graph
    mar = c(4, 4, 2, 1),    # explicitly set margins around plot to default class values
    oma = c(1, 1, 1, 1)     # explicitly set outer margin such that a line of text could be added
)

print(
    xyplot(meanIntervalStepsbyInterval ~ hourDec, data = fitBitSummaryMin,
           type = "l", lty=1, col = "black",
           main = "Fit Bit Activity:  Representative Step-Count for a Day",
           ylab="Mean Step Count in 5 Min Interval [#]", xlab = "Hour [#]"
    )
)
windows(width=6.6667,height=6.6667)
dev.off() 
##
################################################################################
##
## (4b) Plot Imputed Data
##
##  Generate a scatterplot of Global Active Power vs Time
##  (1) Title = "Activity:  Tracking Steps vs Time with Fit Bit"
##  (2) ylabel = "Steps [#]"
##  (3) xlabel = "Day" , "Steps [#]", or NONE
##  (4) Save plot to a PNG file with width = 480 px and height = 480 px
windows(width=6.6667,height=6.6667)   # in windows, sets screen display to 480pX480p
par(
    mfrow = c(1, 1),        # explicitly set plot device to draw 1 graph
    mar = c(4, 4, 2, 1),    # explicitly set margins around plot to default class values
    oma = c(1, 1, 1, 1)     # explicitly set outer margin such that a line of text could be added
)

print(
    histogram(
        ~ cumDailyStepsImput, data = fitBitSummaryDayNumImput, type = "count",
        main = "Activity:  Tracking Steps vs Time with Fit Bit (with Imputed Data)",
        ylab="Days [#]", xlab = "Steps per Day [#]"
    )
)
##
################################################################################
##
windows(width=6.6667,height=6.6667)   # in windows, sets screen display to 480pX480p
par(
    mfrow = c(1, 1),        # explicitly set plot device to draw 1 graph
    mar = c(4, 4, 2, 1),    # explicitly set margins around plot to default class values
    oma = c(1, 1, 1, 1)     # explicitly set outer margin such that a line of text could be added
)

print(
    xyplot(meanIntervalStepsbyIntervalImput ~ hourDec, data = fitBitSummaryMinImput,
           type = "l", lty=1, col = "black",
           main = "Fit Bit Activity:  Representative Step-Count for a Day with Imputed Data",
           ylab="Mean Step Count in 5 Min Interval [#]", xlab = "Hour [#]"
    )
)
windows(width=6.6667,height=6.6667)
dev.off() 
##
################################################################################
##
windows(width=6.6667,height=6.6667)   # in windows, sets screen display to 480pX480p
par(
    mfrow = c(1, 1),        # explicitly set plot device to draw 1 graph
    mar = c(4, 4, 2, 1),    # explicitly set margins around plot to default class values
    oma = c(1, 1, 1, 1)     # explicitly set outer margin such that a line of text could be added
)

fitBitTblDFimput2=aggregate(stepsImput ~ hourDec + dayFactor,fitBitTblDFimput,mean)
print(
    xyplot(stepsImput ~ hourDec | factor(dayFactor), data = fitBitTblDFimput2,
           type = "l", lty=1, col = "black",
           main = "Fit Bit Activity:  Representative Step-Count for a Day with Imputed Data",
           ylab="Mean Step Count in 5 Min Interval [#]", xlab = "Hour [#]", layout = c(1,2)
    )
)
windows(width=6.6667,height=6.6667)
dev.off() 
##
################################################################################
##
# # png(
# #     filename = "PA1_Histogram.png",
# #     width = 480, height = 480
# # )
# # par(
# #     mfrow = c(1, 1),        # explicitly set plot device to draw 1 graph
# #     mar = c(4, 4, 2, 1),    # explicitly set margins around plot to default class values
# #     oma = c(1, 1, 1, 1)     # explicitly set outer margin such that a line of text could be added
# # )
# # 
# # plot(
# #     subTblDF[["UTCdate"]], subTblDF[["Global_active_power"]], type = "l", lty=1, col = "black",
# #     main = "Global Active Power", ylab="Global Active Power (kilowatts)", xlab = ""
# # )
# # dev.off()