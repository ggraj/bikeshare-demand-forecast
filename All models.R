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

## Jan 2012 Data
start <- as.POSIXct('2012-01-01 00:00:00', tz="EST")
end <- as.POSIXct('2012-01-19 23:00:00', tz="EST")
time = data.frame(seq.POSIXt(from=start, to=end , by="1 hour"))
colnames(time)= c("datetime")
evenlyspacedData <- join(time, bikeshare, by = "datetime", type="left")
dataselected_jan2012 <- evenlyspacedData[,c(1,12)]


## impute for all the sample months
# install.packages("zoo")
# install.packages("forecast")
library("zoo")
library("forecast")
dataselected_jan2012$count <- na.interp(dataselected_jan2012$count)


################### Holts Winter model  #####################

a = 0.3
b = 0
g = 0.2
## Jan 2012
jan2012TimeSeries<-ts(dataselected_jan2012$count,frequency=24)
HWModel_jan2012<-HoltWinters(jan2012TimeSeries, seasonal = "additive", alpha = a, beta = b, gamma = g)
plot(HWModel_jan2012, main="Jan 2012 Data with Holt Winters Model", xlab="Day of the month")
text(2,400,labels=bquote(alpha == .(HWModel_jan2012$alpha)), cex=1.5, pos=4) #write alpha on the graph
text(2,350,labels=bquote(beta == .(HWModel_jan2012$beta)), cex=1.5, pos=4) #write alpha on the graph
text(2,300,labels=bquote(gamma == .(HWModel_jan2012$gamma)), cex=1.5, pos=4) #write alpha on the graph
legend("topright", c("Actual","Fitted"), lty=c(1,1), lwd = c(2,2),col = c("black","red"))



trainingset_jan2012<- ts(dataselected_jan2012$count[1:336],frequency=24)
testingset_jan2012 <- ts(dataselected_jan2012$count[337:456],frequency=24, start=c(15,1))
HWModel_train_jan2012<-HoltWinters(trainingset_jan2012,alpha = a, beta = b, gamma = g)
jan2012.pred <- predict(HWModel_train_jan2012, n.ahead = 120, prediction.interval = TRUE)
MAPE1<- mean(abs(jan2012.pred[,1]-testingset_jan2012)/testingset_jan2012)*100
plot.ts(testingset_jan2012, xlim=c(15,20), ylim=c(-500,1000), xlab="Day of the month", col="green", main="HW Jan 2012 Forecast", ylab="Count")
lines(jan2012.pred[,1], col="blue")
lines(jan2012.pred[,2], col="red")
lines(jan2012.pred[,3], col="red")
text(15,350,labels=paste("MAPE=",round(MAPE1,2),"%"), pos=4)
text(15,900,labels=bquote(alpha == .(HWModel_train_jan2012$alpha)), cex=1.5, pos=4) #write alpha on the graph
text(15,800,labels=bquote(beta == .(HWModel_train_jan2012$beta)), cex=1.5, pos=4) #write alpha on the graph
text(15,700,labels=bquote(gamma == .(HWModel_train_jan2012$gamma)), cex=1.5, pos=4) #write alpha on the graph
legend("topright", c("Actual","Predicted","Intervals"), lty=c(1,1), lwd = c(2,2),col = c("green","blue","red"))




################### ARIMA ##############################
jan2012TS <- as.ts(dataselected_jan2012$count[1:336], frequency=24)
jan2012Test <- as.ts(dataselected_jan2012$count[337:456], frequency=24)

p = 2
d = 1
q = 3
P = 0
D = 1
Q = 1


model <- arima(jan2012TS,order=c(p,d,q),seasonal=list(order=c(P,D,Q),period=24))
model  #print out model info
residuals <-as.vector(residuals(model))

#get fitted values using the fitted() function
library(forecast)
fittedValues<-as.vector(fitted(model))

#run Box/jUng test on residuals to test for autocorrelation
#reject null hypothesis that data are independent if p-value is small (want large p-value)
b <- Box.test(residuals,lag=48,fitdf=3,type="Ljung")

#plot ACF/PACF of residuals
par(mfrow=c(1,2))
acf(residuals,lag.max=50,type="correlation",main="ACF of the Residuals")
acf(residuals,lag.max=50, type = "partial",main="PACF of the Residuals")

#four in one plot of the residuals
par(mfrow=c(2,2), oma=c(0,0,0,0)) #draw a set of four graphs, 2 on each row
qqnorm(residuals,datax=TRUE,pch=16,xlab="Residual",main="Normal Probability Plot")
qqline(residuals,datax=TRUE)
plot(fittedValues,residuals,pch=16,xlab="Fitted Value",ylab="Residual", main="Fitted Value vs Residuals")
abline(h=0)
hist(residuals,col="gray",xlab="Residual", main="Histogram of Residuals")
plot(residuals,type="l",xlab="Observation Order",ylab="Residual", main="Residuals vs Order")
points(residuals,pch=16,cex=0.5)
abline(h=0)

#Plot Fitted Versus Actuals
par(mfrow=c(1,1))
plot(dataselected_jan2012[1:336,],type="p",pch=16,cex=0.5,xlab="Day of the month",ylab="Total Count",main=paste("Fitted Versus Actuals ARIMA(",p,",",d,",",q,")x(",P,",",D,",",Q,") 24"))
fittedValuesDF<-data.frame(dataselected_jan2012$datetime[1:336],fittedValues)
lines(fittedValuesDF, col="red")
legend("topleft",c("y(t)","yhat(t)"),pch=c(16,NA),col=c("black","red"),lwd=c(NA,0.5),cex=0.55)
legend("topright",c(paste("AIC",model$aic), paste("Box Test",b$p.value)))

#******************FORECAST TIME*****************************


library(forecast)
bioForecasts<-forecast(model,h=120)

fittedData<-data.frame(dataselected_jan2012$datetime[1:336],bioForecasts$fitted[1:336])
forecastValues<-data.frame(dataselected_jan2012$datetime[337:456],bioForecasts$mean)
forecastUpper95<-data.frame(dataselected_jan2012$datetime[337:456],bioForecasts$upper[,2])
forecastLower95<-data.frame(dataselected_jan2012$datetime[337:456],bioForecasts$lower[,2])

#create the plot by first plotting the training data, but set the x axis limits to the entire training and test range
par(mfrow=c(1,1))  #plot just one graph in the graph panel
plot(dataselected_jan2012[1:336,],main=paste("Jan 2012 Data Forecast - ARIMA(",p,",",d,",",q,")x(",P,",",D,",",Q,") 24"), pch=16,xlim=c(min(dataselected_jan2012$datetime),max(dataselected_jan2012$datetime)),xlab="day",ylab="Total Count")
points(dataselected_jan2012[337:456,],col="blue",pch=16)
lines(forecastValues,col="red",lwd=3, lty=3)
lines(forecastUpper95,col="green",lwd=1)
lines(forecastLower95,col="green",lwd=1)
lines(fittedData,col="red",lwd=3)

#lets calculate our MAPE
MAPE1 <-mean(abs(forecastValues$bioForecasts.mean - dataselected_jan2012$count[337:456])/dataselected_jan2012$count[337:456])*100
text(min(dataselected_jan2012$datetime),max(dataselected_jan2012$count),pos=4,bquote("MAPE"== .(MAPE1)), cex=1)


plot(dataselected_jan2012, 
     xlim=c(min(dataselected_jan2012$datetime[337:456]),max(dataselected_jan2012$datetime[337:456])), 
     xlab="Day of the month", col="green", type = "l",main=paste("Jan 2012 Data Forecast - ARIMA(",p,",",d,",",q,")x(",P,",",D,",",Q,") 24"))
lines(forecastValues, col="blue")
lines(forecastUpper95, col="red")
lines(forecastLower95, col="red")
text(15,350,labels=paste("MAPE=",round(MAPE1,2),"%"), pos=4)
legend("topright", c("Actual","Predicted","Intervals"), lty=c(1,1,1), lwd = c(2,2,2),col = c("green","blue","red"))





################### Dynamic Regression - Regressors #################

## Load this file 
bikeshare <- read.csv(file = "bikeshare_clean.csv", header = TRUE, sep = ",")

## Factorize holiday, workingday, season, weather
bikeshare$season <- as.factor(bikeshare$season)
bikeshare$holiday <- as.factor(bikeshare$holiday)
bikeshare$workingday <- as.factor(bikeshare$workingday)
bikeshare$weather <- as.factor(bikeshare$weather)

## convert "datetime" column datatype to posxict
bikeshare$datetime <-as.POSIXct(bikeshare$datetime,"%Y-%m-%d %H:%M:%S")
bikeshare$date <- as.Date(bikeshare$date)


jan_2012index <- seq(from = 5473, to = 5928)

evenlyspacedData <- bikeshare[jan_2012index,]
rownames(evenlyspacedData) <- NULL
dataselected_jan2012 <- evenlyspacedData[,c(1,10)]
dataselected_jan2012$count <- ts(dataselected_jan2012$count,start = c(1,1),  frequency = 24)

## Weekday 
evenlyspacedData$weekday <- as.factor(weekdays(evenlyspacedData$date))
summary(evenlyspacedData)


## Drop season and temp
evenlyspacedData <- evenlyspacedData[,-c(2,6)]
summary(evenlyspacedData)


## Model matrix to create factors
initialReg <- evenlyspacedData[,-c(1,9)]
summary(initialReg)

library("forecast")
dummies <- seasonaldummy(ts(initialReg[,-7],f=24))
regressors_new <- model.matrix(count~.,initialReg)[,-1]

## holiday
holiday_reg <- model.matrix(~holiday -1, data = initialReg[, c(1,7)])
summary(holiday_reg)

## Workingday
workingday_reg <- model.matrix(~workingday -1, data = initialReg[, c(2,7)])
summary(workingday_reg)

## Weather
weather_reg <- model.matrix(~weather -1, data = initialReg[, c(3,7)])
summary(weather_reg)

## Weather
weekday_reg <- model.matrix(~weekday -1, data = initialReg[, c(8,7)])
summary(weekday_reg)

## combine all regressors together
regressors <- cbind.data.frame(holiday_reg, workingday_reg, weather_reg, weekday_reg, initialReg[,c(4,5,6)])


summary(regressors)

regressorsTS <- ts(regressors_new,start = c(1,1), frequency = 24)


## Training and Test set
trainindex <- seq(from = 1, to=336)
testindex <- seq(from = 337, to = 456)
regressors3 <- regressors[,-1]

bikeshareTS <- dataselected_jan2012$count
library(fpp)
regressorsTS_3 <- regressorsTS[,c(2,5,8,9,10,11,12,13)]
trainset.bike <- bikeshareTS[trainindex]
trainset.reg <- regressorsTS_3[trainindex,]
testset.bike <- bikeshareTS[testindex]
testset.reg <- regressorsTS_3[testindex,]



p = 3
d = 1
q = 4
P = 0
D = 1
Q = 1

modelreg3 <- arima(bikeshareTS, xreg=regressorsTS_3, order=c(p,d,q),seasonal=list(order=c(P,D,Q),period=24))
residuals <-as.vector(residuals(modelreg3))

#get fitted values using the fitted() function
library(forecast)
fittedValues<-as.vector(fitted(modelreg3))

#run Box/jUng test on residuals to test for autocorrelation
#reject null hypothesis that data are independent if p-value is small (want large p-value)
b <- Box.test(residuals,lag=48,fitdf=3,type="Ljung")

#plot ACF/PACF of residuals
par(mfrow=c(1,2))
acf(residuals,lag.max=50,type="correlation",main=paste("ACF - Residuals"))
acf(residuals,lag.max=50, type = "partial",main=paste("PACF - Residuals"))

#four in one plot of the residuals
par(mfrow=c(2,2), oma=c(0,0,0,0)) #draw a set of four graphs, 2 on each row
qqnorm(residuals,datax=TRUE,pch=16,xlab="Residual",main="Normal Probability Plot")
qqline(residuals,datax=TRUE)
plot(fittedValues,residuals,pch=16,xlab="Fitted Value",ylab="Residual", main="Fitted Value vs Residuals")
abline(h=0)
hist(residuals,col="gray",xlab="Residual", main="Histogram of Residuals")
plot(residuals,type="l",xlab="Observation Order",ylab="Residual", main="Residuals vs Order")
points(residuals,pch=16,cex=0.5)
abline(h=0)

#Plot Fitted Versus Actuals
par(mfrow=c(1,1))
plot(dataselected_jan2012,type="p",pch=16,cex=0.5,xlab="Day of the month",ylab="Total Count",
     main=paste("Fitted/Actuals - ARIMA(",p,",",d,",",q,"))X(",P,",",D,",",Q,")24"))
fittedValuesDF<-data.frame(dataselected_jan2012$datetime,fittedValues)
lines(fittedValuesDF, col="red")
legend("topleft",c("y(t)","yhat(t)"),pch=c(16,NA),col=c("black","red"),lwd=c(NA,0.5),cex=0.55)
#add AIC to plot
legend("topright",c(paste("Box Test",b$p.value)))


#lets fit an  ARIMA(p,d,q) X (P,D,Q) model
forecastmodel <- arima(trainset.bike, xreg=trainset.reg, order=c(p,d,q),seasonal=list(order=c(P,D,Q),period=24))
print(paste(p,d,q,P,D,Q))
summary(forecastmodel)

#Let's forecast for weeks 93 to 104 and compare them to our actuals
library(forecast)
clothingForecasts<-forecast(forecastmodel, xreg=testset.reg,h=120)

#let's plot our training data, our test data, our forecasts, and our upper and lower confidence intervals
#I find this easiest to do if I make a bunch of data frames first, then I can just add each data frame to the plot

trainingDF<-dataselected_jan2012[1:336,]
testDF<-dataselected_jan2012[337:456,]
fittedData<-data.frame(trainingDF$datetime,clothingForecasts$fitted)
forecastValues<-data.frame(testDF$datetime,clothingForecasts$mean)
forecastUpper95<-data.frame(testDF$datetime,clothingForecasts$upper[,2])
forecastLower95<-data.frame(testDF$datetime,clothingForecasts$lower[,2])

#create the plot by first plotting the training data, but set the x axis limits to the entire training and test range
par(mfrow=c(1,1))  #plot just one graph in the graph panel
plot(trainingDF,main=paste("Jan2012 Forecast - ARIMA(",p,",",d,",",q,"))X(",P,",",D,",",Q,")24"),
     pch=16,xlim=c(min(trainingDF$datetime),max(testDF$datetime)),
     xlab="Day of the month",ylab="Total Count", ylim=c(-10,250))
points(testDF,col="blue",pch=16)
lines(forecastValues,col="red",lwd=3, lty=3)
lines(forecastUpper95,col="green",lwd=1)
lines(forecastLower95,col="green",lwd=1)
lines(fittedData,col="red",lwd=3)

#lets calculate our MAPE
MAPE1 <-mean(abs(forecastValues$clothingForecasts.mean - testDF$count)/testDF$count)*100
legend("topleft",c(paste("MAPE",MAPE1)))

#zoom in on center of last year to see more detail
par(mfrow=c(1,1))  #plot just one graph in the graph panel
plot(trainingDF,main=paste("Jan2012 Forecast - ARIMA(",p,",",d,",",q,"))X(",P,",",D,",",Q,")24"), pch=16,xlim=c(min(testDF$datetime),max(testDF$datetime)),xlab="Day of the month",ylab="Total count")
points(testDF,col="blue",pch=16)
lines(forecastValues,col="red",lwd=3, lty=3)
lines(forecastUpper95,col="green",lwd=2)
lines(forecastLower95,col="green",lwd=2)
lines(fittedData,col="red",lwd=3)
legend("topright",c("actual","forecast","95% CI"),pch=c(16,NA,NA),col=c("blue","red","green"),lwd=c(NA,3,3),cex=1,lty=c(NA,3,1))
legend("topleft",c(paste("MAPE",MAPE1)))






################### Dynamic Regression - Lagged predictors #################

tempColumn<-regressorsTS_3[,2]
tempData<-cbind(tempColumn,
                c(NA,tempColumn[1:455]),
                c(NA,NA,tempColumn[1:454]),
                c(NA,NA,NA, tempColumn[1:453]))
colnames(tempData)<-paste("TempLag",0:3,sep="_")

trainset.reg <- tempData[trainindex,1:2]
testset.reg <- tempData[testindex,1:2]


p = 2
d = 1
q = 3
P = 0
D = 1
Q = 1



modelreg3 <- arima(bikeshareTS, xreg=tempData[,1:2], order=c(p,d,q),seasonal=list(order=c(P,D,Q),period=24))
residuals <-as.vector(residuals(modelreg3))

#get fitted values using the fitted() function
library(forecast)
fittedValues<-as.vector(fitted(modelreg3))

#four in one plot of the residuals
par(mfrow=c(2,2), oma=c(0,0,0,0)) #draw a set of four graphs, 2 on each row
qqnorm(residuals,datax=TRUE,pch=16,xlab="Residual",main="Normal Probability Plot")
qqline(residuals,datax=TRUE)
plot(fittedValues,residuals,pch=16,xlab="Fitted Value",ylab="Residual", main="Fitted Value vs Residuals")
abline(h=0)
hist(residuals,col="gray",xlab="Residual", main="Histogram of Residuals")
plot(residuals,type="l",xlab="Observation Order",ylab="Residual", main="Residuals vs Order")
points(residuals,pch=16,cex=0.5)
abline(h=0)

#Plot Fitted Versus Actuals
par(mfrow=c(1,1))
plot(dataselected_jan2012,type="p",pch=16,cex=0.5,xlab="Day of the month",ylab="Total Count",
     main=paste("Fitted/Actuals - ARIMA(",p,",",d,",",q,"))X(",P,",",D,",",Q,")24"))
fittedValuesDF<-data.frame(dataselected_jan2012$datetime,fittedValues)
lines(fittedValuesDF, col="red")
legend("topleft",c("y(t)","yhat(t)"),pch=c(16,NA),col=c("black","red"),lwd=c(NA,0.5),cex=0.55)
#add AIC to plot
legend("topright",c(paste("Box Test",b$p.value)))


#lets fit an  ARIMA(p,d,q) X (P,D,Q) model
forecastmodel <- arima(trainset.bike, xreg=trainset.reg, order=c(p,d,q),seasonal=list(order=c(P,D,Q),period=24))
print(paste(p,d,q,P,D,Q))
summary(forecastmodel)

#Let's forecast for weeks 93 to 104 and compare them to our actuals
library(forecast)
clothingForecasts<-forecast(forecastmodel, xreg=testset.reg,h=120)

trainingDF<-dataselected_jan2012[1:336,]
testDF<-dataselected_jan2012[337:456,]
fittedData<-data.frame(trainingDF$datetime,clothingForecasts$fitted)
forecastValues<-data.frame(testDF$datetime,clothingForecasts$mean)
forecastUpper95<-data.frame(testDF$datetime,clothingForecasts$upper[,2])
forecastLower95<-data.frame(testDF$datetime,clothingForecasts$lower[,2])

#create the plot by first plotting the training data, but set the x axis limits to the entire training and test range
par(mfrow=c(1,1))  #plot just one graph in the graph panel
plot(trainingDF,main=paste("Jan2012 Forecast - ARIMA(",p,",",d,",",q,"))X(",P,",",D,",",Q,")24"),
     pch=16,xlim=c(min(trainingDF$datetime),max(testDF$datetime)),
     xlab="Day of the month",ylab="Total Count", ylim=c(-10,250))
points(testDF,col="blue",pch=16)
lines(forecastValues,col="red",lwd=3, lty=3)
lines(forecastUpper95,col="green",lwd=1)
lines(forecastLower95,col="green",lwd=1)
lines(fittedData,col="red",lwd=3)

#lets calculate our MAPE
MAPE1 <-mean(abs(forecastValues$clothingForecasts.mean - testDF$count)/testDF$count)*100
legend("topleft",c(paste("MAPE",MAPE1)))

#zoom in on center of last year to see more detail
par(mfrow=c(1,1))  #plot just one graph in the graph panel
plot(trainingDF,main=paste("Jan2012 Forecast - ARIMA(",p,",",d,",",q,"))X(",P,",",D,",",Q,")24 with lag1 predictors"), pch=16,xlim=c(min(testDF$datetime),max(testDF$datetime)),xlab="Day of the month",ylab="Total count")
points(testDF,col="blue",pch=16)
lines(forecastValues,col="red",lwd=3, lty=3)
lines(forecastUpper95,col="green",lwd=2)
lines(forecastLower95,col="green",lwd=2)
lines(fittedData,col="red",lwd=3)
legend("topright",c("actual","forecast","95% CI"),pch=c(16,NA,NA),col=c("blue","red","green"),lwd=c(NA,3,3),cex=1,lty=c(NA,3,1))
legend("topleft",c(paste("MAPE",MAPE1)))




################### Neural Networks ###############################
## Load this file 
bikeshare <- read.csv(file = "bikeshare_clean.csv", header = TRUE, sep = ",")

## Factorize holiday, workingday, season, weather
bikeshare$season <- as.factor(bikeshare$season)
bikeshare$holiday <- as.factor(bikeshare$holiday)
bikeshare$workingday <- as.factor(bikeshare$workingday)
bikeshare$weather <- as.factor(bikeshare$weather)

## convert "datetime" column datatype to posxict
bikeshare$datetime <-as.POSIXct(bikeshare$datetime,"%Y-%m-%d %H:%M:%S")
bikeshare$date <- as.Date(bikeshare$date)

## add hour variable
bikeshare$hour <- as.factor(strftime(bikeshare$datetime, "%H"))
bikeshare$month <- as.factor(strftime(bikeshare$date, "%m"))
bikeshare$year <- as.factor(strftime(bikeshare$date, "%Y"))

## Weekday 
bikeshare$weekday <- as.factor(weekdays(bikeshare$date))
summary(bikeshare)

## Test for correlation between continuous regressors

cor(bikeshare[,c(6,7,8,9)])

## Temp and atemp are highly correlated, so remove temp, rest of them look fine
bikeshare <- bikeshare[,-6]
summary(bikeshare)

## Factors to columns
library("nnet")
## For all factors, create columns
## Season 
seasonIndicator <- class.ind(bikeshare$season)
colnames(seasonIndicator)<-c("Spring","Summer","Fall","Winter")
summary(seasonIndicator)

## Join Seasons with bikeshare
bikeshare <- data.frame(bikeshare, seasonIndicator)

## weather
weatherIndicator <- class.ind(bikeshare$weather)
## Weather 4 is removed so only 3 types
colnames(weatherIndicator)<-c("Clear","Mist","Snow")
summary(weatherIndicator)
bikeshare <- data.frame(bikeshare, weatherIndicator)

## Year
yearIndicator <- class.ind(bikeshare$year)
colnames(yearIndicator)<-c("2011","2012")
summary(yearIndicator)
bikeshare <- data.frame(bikeshare, yearIndicator)

## Month
monthIndicator <- class.ind(bikeshare$month)
colnames(monthIndicator)<-c("Jan","Feb", "Mar","Apr","May","June","July","Aug","Sep","Oct","Nov","Dec")
summary(monthIndicator)
bikeshare <- data.frame(bikeshare, monthIndicator)

## Weekday
weekdayIndicator <- class.ind(bikeshare$weekday)
colnames(weekdayIndicator)<-c("Friday","Monday", "Saturday","Sunday","Thursday","Tuesday","Wednesday")
summary(weekdayIndicator)
bikeshare <- data.frame(bikeshare, weekdayIndicator)

## hour
hourIndicator <- class.ind(bikeshare$hour)
summary(hourIndicator)
bikeshare <- data.frame(bikeshare, hourIndicator)

## holiday
bikeshare$holiday <- as.numeric(bikeshare$holiday) - 1
bikeshare$workingday <- as.numeric(bikeshare$workingday) - 1

## Subset the essential columns 
bikeshare_forNN <- bikeshare[,-c(2,5,10,11,12,13,14)]

## PART 2
## creating function for normalizing values between 0 and 1
normalizefunction = function(x) {
  num = x - min(x)
  denom = max(x) - min(x)
  return(num/denom)
}

## Normalize continuous function
continuousData <- bikeshare_forNN[,c("atemp","humidity","windspeed","count")]

minCount<-min(bikeshare_forNN$count)
maxCount<-max(bikeshare_forNN$count)

continuousData = data.frame(sapply(continuousData, normalizefunction))
bikeshare_forNN$count <- continuousData$count
bikeshare_forNN$humidity <- continuousData$humidity
bikeshare_forNN$windspeed <- continuousData$windspeed
bikeshare_forNN$atemp <- continuousData$atemp

movetolast <- function(data, move) {
  data[c(setdiff(names(data), move), move)]
}

bikeshare_forNN <- movetolast(bikeshare_forNN, "count")

## Training and Test set
## Jan 2012 5472 - 5808 and 5809 - 5928
trainindex <- seq(from = 5473, to=5808)
testindex <- seq(from = 5809, to = 5928)

testset <- bikeshare_forNN[testindex,]
trainingset <- bikeshare_forNN[-testindex,]
predictors <- trainingset[,-c(1,length(trainingset))]
set.seed(1)
#train your neural net with the parameters you like best
h=4
d= 0.01
i = 400
nnetFit<-nnet(predictors, # the regressor variables
              trainingset$count, #what you are trying to predict
              size=4, #number of hidden nodes
              decay = 0.01, #gives a penalty for large weights
              linout = TRUE, #says you want a linear output (as oppposed to a classification output)
              trace=FALSE, #reduces amount of output printed to screen
              maxit = 400, # increases max iterations to 500 from default of 100
              MaxNWts = h*(ncol(predictors)+1)+h+1) #says you can have one weight for each input + an additional intercept term


fitted<-data.frame(trainingset[trainindex,1],nnetFit$fitted.values[trainindex])
plot(trainingset[trainindex,c(1,length(trainingset))], type='l',main="NN Jan 2012 Fitted/Actual ", col="black", xlab="Day of the month", ylab = "Count")
lines(fitted,col='red')
legend("topright", c("Actual","Fitted"), lty=c(1,1), lwd = c(2,2),col = c("black","red"))

predictorsTest<-testset[,-c(1,length(testset))]
predictions<-predict(nnetFit,predictorsTest)
predicted <- data.frame(testset$datetime, predictions)
plot(testset[,c(1,length(trainingset))], type='l', col="green",ylim = c(-0.10, 0.50), main="NN Jan 2012 normalized forecast ")
lines(predicted, col="blue")
legend("topright", c("Actual","Predicted"), lty=c(1,1), lwd = c(2,2),col = c("green","blue"))
## creating function for un-normalizing data
unnormalizefunction = function(x,min,max) {
  return(x*(max-min)+min)
}
unnormalizedPredictors = unnormalizefunction(x=predictions,min=minCount,max=maxCount)
unnormalizedCount=unnormalizefunction(x=testset$count,min=minCount,max=maxCount)
MAPE <-mean(abs(unnormalizedCount-unnormalizedPredictors)/unnormalizedCount*100)
MAPE
unnormalizedwithdate <- data.frame(testset$datetime, unnormalizedCount)
unPredictorswithdate <- data.frame(testset$datetime, unnormalizedPredictors)
plot(unnormalizedwithdate, type='l', col="green", main ="NN Jan 2012 Forecast", xlab="Day of the month", ylab="Count")
lines(unPredictorswithdate,col="blue")
text(1,350,labels=paste("MAPE=",round(MAPE,2),"%"), pos=4)
legend("topright", c("Actual","Predicted"), lty=c(1,1), lwd = c(2,2),col = c("green","blue"))
## Residuals of Training and Test set
## Training ############
res_train <- residuals(nnetFit)[trainindex]
fittedValues <- fitted[,2]
b <- Box.test(res_train,lag=48,fitdf=3,type="Ljung")
b
#plot ACF/PACF of residuals
par(mfrow=c(1,2))
acf(res_train,lag.max=100,type="correlation",main=paste("ACF - Residuals"))
acf(res_train,lag.max=100, type = "partial",main=paste("PACF - Residuals"))

#four in one plot of the residuals
par(mfrow=c(2,2), oma=c(0,0,0,0)) #draw a set of four graphs, 2 on each row
qqnorm(res_train,datax=TRUE,pch=16,xlab="Residual",main="Normal Probability Plot")
qqline(res_train,datax=TRUE)
plot(fittedValues,res_train,pch=16,xlab="Fitted Value",ylab="Residual", main="Fitted Value vs Residuals")
abline(h=0)
hist(res_train,col="gray",xlab="Residual", main="Histogram of Residuals")
plot(res_train,type="l",xlab="Observation Order",ylab="Residual", main="Residuals vs Order")
points(res_train,pch=16,cex=0.5)
abline(h=0)
## Test ############
res_test <- testset$count - predictions

b <- Box.test(res_test,lag=48,fitdf=3,type="Ljung")

#plot ACF/PACF of residuals
par(mfrow=c(1,2))
acf(res_test,lag.max=100,type="correlation",main=paste("ACF - Residuals"))
acf(res_test,lag.max=100, type = "partial",main=paste("PACF - Residuals"))

#four in one plot of the residuals
par(mfrow=c(2,2), oma=c(0,0,0,0)) #draw a set of four graphs, 2 on each row
qqnorm(res_test,datax=TRUE,pch=16,xlab="Residual",main="Normal Probability Plot")
qqline(res_test,datax=TRUE)
plot(predictions,res_test,pch=16,xlab="Fitted Value",ylab="Residual", main="Fitted Value vs Residuals")
abline(h=0)
hist(res_test,col="gray",xlab="Residual", main="Histogram of Residuals")
plot(res_test,type="l",xlab="Observation Order",ylab="Residual", main="Residuals vs Order")
points(res_test,pch=16,cex=0.5)
abline(h=0)


################### Neural Networks - Log transformed ###############################

## Subset the essential columns 
bikeshare_forNN <- bikeshare[,-c(2,5,10,11,12,13,14)]

bikeshare_forNN$count <- log(bikeshare_forNN$count+1)

## creating function for normalizing values between 0 and 1
normalizefunction = function(x) {
  num = x - min(x)
  denom = max(x) - min(x)
  return(num/denom)
}

## Normalize continuous function ##################### 
continuousData <- bikeshare_forNN[,c("atemp","humidity","windspeed","count")]

minCount<-min(bikeshare_forNN$count)
maxCount<-max(bikeshare_forNN$count)

continuousData = data.frame(sapply(continuousData, normalizefunction))
bikeshare_forNN$count <- continuousData$count
bikeshare_forNN$humidity <- continuousData$humidity
bikeshare_forNN$windspeed <- continuousData$windspeed
bikeshare_forNN$atemp <- continuousData$atemp

movetolast <- function(data, move) {
  data[c(setdiff(names(data), move), move)]
}

bikeshare_forNN <- movetolast(bikeshare_forNN, "count")

## Jan 2012 5472 - 5808 and 5809 - 5928
trainindex <- seq(from = 5473, to=5808)
testindex <- seq(from = 5809, to = 5928)

testset <- bikeshare_forNN[testindex,]
trainingset <- bikeshare_forNN[-testindex,]
predictors <- trainingset[,-c(1,length(trainingset))]
set.seed(1)

#train your neural net with the parameters you like best
h=10
d= 0.1
i = 600
nnetFit<-nnet(predictors, # the regressor variables
              trainingset$count, #what you are trying to predict
              size=h, #number of hidden nodes
              decay = d, #gives a penalty for large weights
              linout = TRUE, #says you want a linear output (as oppposed to a classification output)
              trace=FALSE, #reduces amount of output printed to screen
              maxit = i, # increases max iterations to 500 from default of 100
              MaxNWts = h*(ncol(predictors)+1)+h+1) #says you can have one weight for each input + an additional intercept term


fitted<-data.frame(trainingset[trainindex,1],nnetFit$fitted.values[trainindex])
plot(trainingset[trainindex,c(1,length(trainingset))], type='l',main="NN Jan 2012 Fitted/Actual ", col="black", xlab="Day of the month", ylab = "Count")
lines(fitted,col='red')
legend("topright", c("Actual","Fitted"), lty=c(1,1), lwd = c(2,2),col = c("black","red"))

predictorsTest<-testset[,-c(1,length(testset))]
predictions<-predict(nnetFit,predictorsTest)

predicted <- data.frame(testset$datetime, predictions)
plot(testset[,c(1,length(trainingset))], type='l', col="green",ylim = c(-0.10, 0.50), main="NN Jan 2012 normalized forecast ")
lines(predicted, col="blue")
legend("topright", c("Actual","Predicted"), lty=c(1,1), lwd = c(2,2),col = c("green","blue"))
## creating function for un-normalizing data
unnormalizefunction = function(x,min,max) {
  return(x*(max-min)+min)
}
unnormalizedPredictors = unnormalizefunction(x=predictions,min=minCount,max=maxCount)
unnormalizedCount=unnormalizefunction(x=testset$count,min=minCount,max=maxCount)
MAPE <-mean(abs(unnormalizedCount-unnormalizedPredictors)/unnormalizedCount*100)
MAPE
unloggedPredictors = exp(unnormalizedPredictors)-1
unloggedCount=exp(unnormalizedCount)-1
# MAPE
mean(abs(unloggedCount-unloggedPredictors)/unloggedCount*100)
unnormalizedwithdate <- data.frame(testset$datetime, unloggedCount)
unPredictorswithdate <- data.frame(testset$datetime, unloggedPredictors)
plot(unnormalizedwithdate, type='l', col="green", main ="NN Jan 2012 Forecast", xlab="Day of the month", ylab="Count")
lines(unPredictorswithdate,col="blue")
text(1,350,labels=paste("MAPE=",round(MAPE,2),"%"), pos=4)
legend("topright", c("Actual","Predicted"), lty=c(1,1), lwd = c(2,2),col = c("green","blue"))

## Residuals of Training and Test set
## Training ############
res_train <- residuals(nnetFit)[trainindex]
fittedValues <- fitted[,2]
b <- Box.test(res_train,lag=48,fitdf=3,type="Ljung")
b
res_trainlog <- exp(res_train)-1
fittedValueslog <- exp(fittedValues)-1
blog <- Box.test(res_trainlog,lag=48,fitdf=3,type="Ljung")
blog

#plot ACF/PACF of residuals
par(mfrow=c(1,2))
acf(res_trainlog,lag.max=100,type="correlation",main=paste("ACF - Residuals"))
acf(res_trainlog,lag.max=100, type = "partial",main=paste("PACF - Residuals"))

#four in one plot of the residuals
par(mfrow=c(2,2), oma=c(0,0,0,0)) #draw a set of four graphs, 2 on each row
qqnorm(res_trainlog,datax=TRUE,pch=16,xlab="Residual",main="Normal Probability Plot")
qqline(res_trainlog,datax=TRUE)
plot(fittedValueslog,res_trainlog,pch=16,xlab="Fitted Value",ylab="Residual", main="Fitted Value vs Residuals")
abline(h=0)
hist(res_trainlog,col="gray",xlab="Residual", main="Histogram of Residuals")
plot(res_trainlog,type="l",xlab="Observation Order",ylab="Residual", main="Residuals vs Order")
points(res_trainlog,pch=16,cex=0.5)
abline(h=0)

## Test ############
res_test <- testset$count - predictions

res_testlog <- exp(res_test) - 1
predictionslog <- exp(predictions) - 1
b <- Box.test(res_test,lag=48,fitdf=3,type="Ljung")
b

blog <- Box.test(res_testlog,lag=48,fitdf=3,type="Ljung")
blog

#plot ACF/PACF of residuals
par(mfrow=c(1,2))
acf(res_testlog,lag.max=100,type="correlation",main=paste("ACF - Residuals"))
acf(res_testlog,lag.max=100, type = "partial",main=paste("PACF - Residuals"))

#four in one plot of the residuals
par(mfrow=c(2,2), oma=c(0,0,0,0)) #draw a set of four graphs, 2 on each row
qqnorm(res_testlog,datax=TRUE,pch=16,xlab="Residual",main="Normal Probability Plot")
qqline(res_testlog,datax=TRUE)
plot(predictionslog,res_testlog,pch=16,xlab="Fitted Value",ylab="Residual", main="Fitted Value vs Residuals")
abline(h=0)
hist(res_testlog,col="gray",xlab="Residual", main="Histogram of Residuals")
plot(res_testlog,type="l",xlab="Observation Order",ylab="Residual", main="Residuals vs Order")
points(res_testlog,pch=16,cex=0.5)
abline(h=0)
