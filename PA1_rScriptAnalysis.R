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
##  (2a) Read data -> (2b) Transform exptl data into analytic data;
##  (3)  Analyze data
##  (4)Plot data per assignment instruction generating
##      plots and a png file with each plot.
##
################################################################################
##
## Part (1a & 1b):
##
library(dplyr)
library(lubridate)
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
        UTCdate = ymd( paste(date) )
    ) %>%
#     filter(
#         UTCdate >= ymd("yyyy-mm-dd") & UTCdate < ymd("yyyy-mm-dd")
#     )%>%
    select(
        UTCdate, interval, steps
#     mutate(
#         field1 = as.numeric(field1),
#         field2 = as.numeric(field2),
     )
print(fitBitTblDF)
##
################################################################################
##
## (3) Analyze Data
##
## What is mean total number of steps taken per day?
fitBitSummary <-
    fitBitTblDF %>%
    group_by(UTCdate) %>%
    summarize(count = n(),
              uniqueDate = n_distinct(UTCdate),
              uniqueIntervals = n_distinct(interval),
              meanDailySteps = mean(steps, na.rm = TRUE)
    ) %>%
#     filter(countries > 60) %>%
    arrange(UTCdate, uniqueIntervals)

# Print result to console
print(fitBitSummary)



## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?



##
################################################################################
##
## (4) Plot Data
##
##  Generate a scatterplot of Global Active Power vs Time
##  (1) Title = "Global Active Power"
##  (2) ylabel = "Global Active Power (kilowatts)"
##  (3) xlabel = None
##  (3) Top Left Margin Text = "Plot1"
##  (4) Save plot to a PNG file with width = 480 px and height = 480 px
# windows(width=6.6667,height=6.6667)   # in windows, sets screen display to 480pX480p
# par(
#     mfrow = c(1, 1),        # explicitly set plot device to draw 1 graph
#     mar = c(4, 4, 2, 1),    # explicitly set margins around plot to default class values
#     oma = c(1, 1, 1, 1)     # explicitly set outer margin such that a line of text could be added
# )
# 
# plot(
#     subTblDF[["UTCdate"]], subTblDF[["Global_active_power"]], type = "l", lty=1, col = "black",
#     main = "Global Active Power", ylab="Global Active Power (kilowatts)", xlab = ""
# )
# # ##
# # ## generate PNG file using device copy command and turn PNG graphic device off
# # ##
# # dev.copy(
# #     png,
# #     filename = "plot2.png",
# #     width = 480, height = 480
# # )
# # dev.off()
# ##
# ## generate PNG file using device copy command and turn PNG graphic device off
# ##
# png(
#     filename = "plot2.png",
#     width = 480, height = 480
# )
# par(
#     mfrow = c(1, 1),        # explicitly set plot device to draw 1 graph
#     mar = c(4, 4, 2, 1),    # explicitly set margins around plot to default class values
#     oma = c(1, 1, 1, 1)     # explicitly set outer margin such that a line of text could be added
# )
# 
# plot(
#     subTblDF[["UTCdate"]], subTblDF[["Global_active_power"]], type = "l", lty=1, col = "black",
#     main = "Global Active Power", ylab="Global Active Power (kilowatts)", xlab = ""
# )
# dev.off()