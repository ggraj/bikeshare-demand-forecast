######################## Pre-Processing ##############################

## Load data from Week 1
bikeshare <- read.csv("train.csv", header = T, sep = ",")

## Factorize holiday, workingday, season, weather
bikeshare$season <- as.factor(bikeshare$season)
bikeshare$holiday <- as.factor(bikeshare$holiday)
bikeshare$workingday <- as.factor(bikeshare$workingday)
bikeshare$weather <- as.factor(bikeshare$weather)

## convert "datetime" column datatype to posxict
bikeshare$datetime <-as.POSIXct(bikeshare$datetime,tz="","%Y-%m-%d %H:%M:%S")

## attach the bikeshare to easily access its variables
attach(bikeshare)
library("plyr")

## Jan 2011 Data
start <- as.POSIXct('2011-01-01 00:00:00', tz="EST")
end <- as.POSIXct('2011-01-19 23:00:00', tz="EST")
time = data.frame(seq.POSIXt(from=start, to=end , by="1 hour"))
colnames(time)= c("datetime")
evenlyspacedData <- join(time, bikeshare, by = "datetime", type="left")
dataselected_jan2011 <- evenlyspacedData[,c(1,12)]


## Mar 2011 Data
start <- as.POSIXct('2011-03-01 00:00:00', tz="EST")
end <- as.POSIXct('2011-03-19 23:00:00', tz="EST")
time = data.frame(seq.POSIXt(from=start, to=end , by="1 hour"))
colnames(time)= c("datetime")
evenlyspacedData <- join(time, bikeshare, by = "datetime", type="left")
dataselected_mar2011 <- evenlyspacedData[,c(1,12)]


## May 2011 Data
start <- as.POSIXct('2011-05-01 00:00:00', tz="EST")
end <- as.POSIXct('2011-05-19 23:00:00', tz="EST")
time = data.frame(seq.POSIXt(from=start, to=end , by="1 hour"))
colnames(time)= c("datetime")
evenlyspacedData <- join(time, bikeshare, by = "datetime", type="left")
dataselected_may2011 <- evenlyspacedData[,c(1,12)]


## Aug 2011 Data
start <- as.POSIXct('2011-08-01 00:00:00', tz="EST")
end <- as.POSIXct('2011-08-19 23:00:00', tz="EST")
time = data.frame(seq.POSIXt(from=start, to=end , by="1 hour"))
colnames(time)= c("datetime")
evenlyspacedData <- join(time, bikeshare, by = "datetime", type="left")
dataselected_aug2011 <- evenlyspacedData[,c(1,12)]

## Oct 2011 Data
start <- as.POSIXct('2011-10-01 00:00:00', tz="EST")
end <- as.POSIXct('2011-10-19 23:00:00', tz="EST")
time = data.frame(seq.POSIXt(from=start, to=end , by="1 hour"))
colnames(time)= c("datetime")
evenlyspacedData <- join(time, bikeshare, by = "datetime", type="left")
dataselected_oct2011 <- evenlyspacedData[,c(1,12)]

## Dec 2011 Data
start <- as.POSIXct('2011-12-01 00:00:00', tz="EST")
end <- as.POSIXct('2011-12-19 23:00:00', tz="EST")
time = data.frame(seq.POSIXt(from=start, to=end , by="1 hour"))
colnames(time)= c("datetime")
evenlyspacedData <- join(time, bikeshare, by = "datetime", type="left")
dataselected_dec2011 <- evenlyspacedData[,c(1,12)]

## impute for all the sample months
library("zoo")
library("forecast")
dataselected_jan2011$count <- na.interp(dataselected_jan2011$count)
dataselected_mar2011$count <- na.interp(dataselected_mar2011$count)
dataselected_may2011$count <- na.interp(dataselected_may2011$count)
dataselected_aug2011$count <- na.interp(dataselected_aug2011$count)
dataselected_oct2011$count <- na.interp(dataselected_oct2011$count)
dataselected_dec2011$count <- na.interp(dataselected_dec2011$count)


######################## Sample Variance and Autocovariance ##############################


## Since Jan 2011 is a subset of the entire data. Sample mean and variance can be calculated from this data
## Sample Mean
mean(dataselected_jan2011$count) 
## Sample Variance
var(dataselected_jan2011$count)

## Autocovariance
## Jan 2011 data
acf(dataselected_jan2011$count, lag.max = 50, main="ACF for Jan 2011")
acf(dataselected_mar2011$count, lag.max = 50, main="ACF for March 2011")
acf(dataselected_may2011$count, lag.max = 50, main="ACF for May 2011")
acf(dataselected_aug2011$count, lag.max = 50, main="ACF for August 2011")
acf(dataselected_oct2011$count, lag.max = 50, main="ACF for October 2011")
acf(dataselected_dec2011$count, lag.max = 50, main="ACF for December 2011")

## The ACF plots are sinusoidal about 0. Thus evidence of stationary time series.
## The acf has a cyclic pattern at every 24 lags revealing a daily seasonality.
## But most of the acf values exceed the 5% significance limits.



################################### Variogram #################################
library("pastecs")
v1 <- vario(dataselected_jan2011$count, plotit=TRUE, max.dist = 100)
v2 <- vario(dataselected_mar2011$count, plotit=TRUE, max.dist = 100)
v3 <- vario(dataselected_may2011$count, plotit=TRUE, max.dist = 100)
v4 <- vario(dataselected_aug2011$count, plotit=TRUE, max.dist = 100)
v5 <- vario(dataselected_oct2011$count, plotit=TRUE, max.dist = 100)
v6 <- vario(dataselected_dec2011$count, plotit=TRUE, max.dist = 100)



## The above semi-variogram plots show low variances at every 24 lags revealing daily seasonality.


####################### Seasonality and Trend ###############################


## Take the first difference to remove trend
## Jan 2011 Data
## Using lag = 168 (24 hours * 7 days) since there is weekly seasonality due to weekdays and weekends
detrended_jan2011 <- diff(dataselected_jan2011$count)
deseasoned_jan2011 <- diff(dataselected_jan2011$count, lag=168)
detrend_deseasoned_jan2011 <- diff(detrended_jan2011, lag=168)
plot(detrended_jan2011, main="Jan 2011 Detrended", xlab="Lag", ylab="De-trended")
plot(deseasoned_jan2011, main="Jan 2011 De-seasoned", xlab="Lag", ylab="Deseasoned")
plot(detrend_deseasoned_jan2011, main="Jan 2011 De-Trended and Deseasoned", xlab="Lag", ylab="De-Trended and Deseasoned")


## March 2011 Data
detrended_mar2011 <- diff(dataselected_mar2011$count)
deseasoned_mar2011 <- diff(dataselected_mar2011$count, lag=168)
detrend_deseasoned_mar2011 <- diff(detrended_mar2011, lag=168)
plot(detrended_mar2011, main="March 2011 Detrended", xlab="Lag", ylab="De-trended")
plot(deseasoned_mar2011, main="March 2011 De-seasoned", xlab="Lag", ylab="Deseasoned")
plot(detrend_deseasoned_mar2011, main="March 2011 De-Trended and Deseasoned", xlab="Lag", ylab="De-Trended and Deseasoned")


## May 2011 Data
detrended_may2011 <- diff(dataselected_may2011$count)
deseasoned_may2011 <- diff(dataselected_may2011$count, lag=168)
detrend_deseasoned_may2011 <- diff(detrended_may2011, lag=168)
plot(detrended_may2011, main="May 2011 Detrended", xlab="Lag", ylab="De-trended")
plot(deseasoned_may2011, main="May 2011 De-seasoned", xlab="Lag", ylab="Deseasoned")
plot(detrend_deseasoned_may2011, main="May 2011 De-Trended and Deseasoned", xlab="Lag", ylab="De-Trended and Deseasoned")


## aug 2011 Data
detrended_aug2011 <- diff(dataselected_aug2011$count)
deseasoned_aug2011 <- diff(dataselected_aug2011$count, lag=168)
detrend_deseasoned_aug2011 <- diff(detrended_aug2011, lag=168)
plot(detrended_aug2011, main="August 2011 Detrended", xlab="Lag", ylab="De-trended")
plot(deseasoned_aug2011, main="August 2011 De-seasoned", xlab="Lag", ylab="Deseasoned")
plot(detrend_deseasoned_aug2011, main="August 2011 De-Trended and Deseasoned", xlab="Lag", ylab="De-Trended and Deseasoned")


## October 2011 Data
detrended_oct2011 <- diff(dataselected_oct2011$count)
deseasoned_oct2011 <- diff(dataselected_oct2011$count, lag=168)
detrend_deseasoned_oct2011 <- diff(detrended_oct2011, lag=168)
plot(detrended_oct2011, main="October 2011 Detrended", xlab="Lag", ylab="De-trended")
plot(deseasoned_oct2011, main="October 2011 De-seasoned", xlab="Lag", ylab="Deseasoned")
plot(detrend_deseasoned_oct2011, main="October 2011 De-Trended and Deseasoned", xlab="Lag", ylab="De-Trended and Deseasoned")



## December 2011 Data
detrended_dec2011 <- diff(dataselected_dec2011$count)
deseasoned_dec2011 <- diff(dataselected_dec2011$count, lag=168)
detrend_deseasoned_dec2011 <- diff(detrended_dec2011, lag=168)
plot(detrended_dec2011, main="December 2011 Detrended", xlab="Lag", ylab="De-trended")
plot(deseasoned_dec2011, main="December 2011 De-seasoned", xlab="Lag", ylab="Deseasoned")
plot(detrend_deseasoned_dec2011, main="December 2011 De-Trended and Deseasoned", xlab="Lag", ylab="De-Trended and Deseasoned")

## Observations:
## The above de-trended and deseasoned plots does not have constant variance or constant mean? (Not sure, ask Dr.Katie)

####################### Power Transformation ###########################

## High variances in the data prompts the use of power transformation
## Using Log Transformation

## Jan 2011 
dataselected_jan2011$logCount <- log(dataselected_jan2011$count)
plot(y= dataselected_jan2011$logCount, x=dataselected_jan2011$datetime, type="l", xlab="Date Time", ylab="Natural Log(Count)", main="Jan 2011 Natural Log Transformation of Total Count")

## March 2011
dataselected_mar2011$logCount <- log(dataselected_mar2011$count)
plot(y= dataselected_mar2011$logCount, x=dataselected_mar2011$datetime, type="l", xlab="Date Time", ylab="Natural Log(Count)", main="March 2011 Natural Log Transformation of Total Count")

## May 2011
dataselected_may2011$logCount <- log(dataselected_may2011$count)
plot(y= dataselected_may2011$logCount, x=dataselected_may2011$datetime, type="l", xlab="Date Time", ylab="Natural Log(Count)", main="May 2011 Natural Log Transformation of Total Count")

## August 2011
dataselected_aug2011$logCount <- log(dataselected_aug2011$count)
plot(y= dataselected_aug2011$logCount, x=dataselected_aug2011$datetime, type="l", xlab="Date Time", ylab="Natural Log(Count)", main="August 2011 Natural Log Transformation of Total Count")

## October 2011
dataselected_oct2011$logCount <- log(dataselected_oct2011$count)
plot(y= dataselected_oct2011$logCount, x=dataselected_oct2011$datetime, type="l", xlab="Date Time", ylab="Natural Log(Count)", main="October 2011 Natural Log Transformation of Total Count")

## December 2011
dataselected_dec2011$logCount <- log(dataselected_dec2011$count)
plot(y= dataselected_dec2011$logCount, x=dataselected_dec2011$datetime, type="l", xlab="Date Time", ylab="Natural Log(Count)", main="December 2011 Natural Log Transformation of Total Count")


## Observations
## The variance seems to have stabilized using natural log transformation of count.
## What other observations needed?




