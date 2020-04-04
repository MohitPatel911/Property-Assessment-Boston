library(tidyr)
library(readr)
library(dplyr)
library(plotly)
library(ggplot2)
## Setting up the working directory for the project
getwd()
setwd("C:/Users/patel/Desktop/ALY-6070-Data Visualization/Rshiny/Aly-6070-RShiny-Project")

##Loading work space impage if you are already working on the same projest
## and want to continue working on it
load("C:/Users/patel/Desktop/ALY-6070-Data Visualization/Rshiny/Aly-6070-RShiny-Project/.RData")

## If you want to Import new Data into R###

Property_assessment_boston <- read_csv("C:/Users/patel/Desktop/ALY-6070-Data Visualization/Data/Property_assessment_boston.csv", 
                               col_types = cols(LU = col_character(), 
                                 ZIPCODE = col_character()))

## Subsetting data and getting only the nessary columns for data visualization
bostonProperty <- Property_assessment_boston[, c("YR_BUILT", "YR_REMOD", "ZIPCODE", "LU")]

## divides years built and remodeled data into "Years", "months", "days"
bostonProperty <- bostonProperty %>%
  separate(YR_BUILT, sep="-", into = c("YR_BUILT","month_B", "day_B"))

bostonProperty <- bostonProperty %>%
  separate(YR_REMOD, sep="-", into = c("YR_REMOD", "month_R", "day_R"))

bostonProperty[bostonProperty == "0"] <- NA

##Working with the null values and data cleaning
bostonProperty <- bostonProperty[-c(2:3,5:6)]

sum(is.na(bostonProperty$YR_BUILT)) ## gives the sum of missing values in year built column
sum(is.na(bostonProperty$YR_REMOD)) ## gives the sum of missing values in year remodeled cloumn
sum(is.na(bostonProperty$ZIPCODE)) ## gives the sum of missing values in zipcode column 
sum(is.na(bostonProperty$LU)) ## gives the sum of missing values in land use(LU) column

## subset data according to rows value of commercial and industrial

Commercial_Industry <- bostonProperty[ bostonProperty$LU == "C" | bostonProperty$LU == "I", ]

## subset data according to year built to display in which years
## maximum commercial and industrial properties were built

Commercial_Industry <- Commercial_Industry[ Commercial_Industry$YR_BUILT == "1899" | Commercial_Industry$YR_BUILT == "1900" | 
                                              Commercial_Industry$YR_BUILT == "1910" | Commercial_Industry$YR_BUILT == "1920" |
                                          Commercial_Industry$YR_BUILT == "1930" | Commercial_Industry$YR_BUILT == "1940"
                                       | Commercial_Industry$YR_BUILT == "1950" | Commercial_Industry$YR_BUILT == "1960"
                                       | Commercial_Industry$YR_BUILT == "1970",]


## Sacked bar plot in which year maximum
## Industrial and commercial properties were built
fig <- plot_ly(Commercial_Industry, x = ~YR_BUILT, y = ~LU, type = 'bar', name = "I")
fig<- fig%>% add_trace(y = ~LU, name = "C")
fig<- fig%>% layout(yaxis = list(title = 'count'), barmode = 'stack')
fig

## subset data according to year re-built to display in which years
## maximum commercial and industrial properties were re-built

Commercial_Industry$YR_BUILT <- as.numeric(as.character(Commercial_Industry$YR_BUILT))
Commercial_Industry$LU <- as.factor(as.character(Commercial_Industry$LU))


## subset data according maximum year re-built to display in which years
## maximum commercial and industrial properties were built
Commercial_Industry1 <- Commercial_Industry[ Commercial_Industry$YR_REMOD == 1975 |
                                          Commercial_Industry$YR_REMOD >= 1980,] 

## ommiting null values from the data set
Commercial_Industry <- na.omit(Commercial_Industry)
Newdf1 <- Commercial_Industry[-c(2:3)]

## ommiting null values from the data set
Commercial_Industry1 <- na.omit(Commercial_Industry1)
Newdf2 <- Commercial_Industry1[-c(1,3)]


## Sacked bar plot in which maximum
## Industrial and commercial properties were re-built
fig <- plot_ly(Commercial_Industry1, x = ~as.character(YR_REMOD), y = ~LU, type = 'bar', name = "I")
fig<- fig%>% add_trace(y = ~as.character(YR_REMOD), name = "C")
fig<- fig%>% layout(yaxis = list(title = 'count'), barmode = 'stack')
fig

## As we are working in the same directory it is a good practice to
## to save workshpace at the end of the day when we finish working for the day
## We don't have to specify the complete path of the file as we are in the 
## same directory. So, just writing the file name with .RData extention is 
## enough to save the work of the day.
save.image(".RData")



