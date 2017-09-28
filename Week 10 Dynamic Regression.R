###################### Pre Processing #################################

## Load data from Week 1
bikeshare <- read.csv("train.csv", header = T, sep = ",")

## Factorize holiday, workingday, season, weather
bikeshare$season <- as.factor(bikeshare$season)
bikeshare$holiday <- as.factor(bikeshare$holiday)
bikeshare$workingday <- as.factor(bikeshare$workingday)
bikeshare$weather <- as.factor(bikeshare$weather)

## convert "datetime" column datatype to posxict
bikeshare$datetime <-as.POSIXct(bikeshare$datetime,tz="","%Y-%m-%d %H:%M:%S")


attach(bikeshare)
library("plyr")

## Jan 2011 Data
start <- as.POSIXct('2011-01-01 00:00:00', tz="EST")
end <- as.POSIXct('2011-01-19 23:00:00', tz="EST")
time = data.frame(seq.POSIXt(from=start, to=end , by="1 hour"))
colnames(time)= c("datetime")
evenlyspacedData <- join(time, bikeshare, by = "datetime", type="left")
dataselected_jan2011 <- evenlyspacedData[,c(1,12)]

## impute for all the sample months
##install.packages("zoo")
##install.packages("forecast")
library("zoo")
library("forecast")
dataselected_jan2011$count <- na.interp(dataselected_jan2011$count)

## Possible regressors
## From Week 1 EDA, it was clear that the Working days affect the seasonality of count significantly. 
## Other variables such as feels like temperature can be used, Weather can also be used.  
## Check if the regressors have na's 
sum(is.na(evenlyspacedData$workingday))
## Thus there are 30 missing values for Jan 2011 
## To impute them is to look up values from the same day since these varaibles are constant throught the day, except may be temperature

plot(evenlyspacedData)

evenlyspacedData$atemp
plot(evenlyspacedData$atemp, type="l")
lines(na.interp(evenlyspacedData$atemp), col="red")

regressors <- evenlyspacedData[,c(4, 5,7)]

regressors$atemp <- na.interp(regressors$atemp)

## For working day use the value of the same day for missing values.
evenlyspacedData$date <- strftime(evenlyspacedData$datetime, "%Y-%m-%d", tz = "EST" )
omitdata <- na.omit(evenlyspacedData)
b <- omitdata[!duplicated(omitdata$date),]
b1 <- b[,c(4,13)]

c <-join(b1, evenlyspacedData, by = "date", type="inner")
c$workingday
regressors$workingday <- c$workingday
regressors$weather <- na.locf(regressors$weather)

model.matrix(~regressors$weather)
## Get dummies for Weather variable in Regressors
regressors$weather1 <- ifelse(regressors$weather == 1, 1,0)
regressors$weather2 <- ifelse(regressors$weather == 2, 1,0)
regressors$weather3 <- ifelse(regressors$weather == 3, 1,0)
regressors$weather4 <- ifelse(regressors$weather == 4, 1,0)

#regressors$weather1 <- as.factor(regressors$weather1)
#regressors$weather2 <- as.factor(regressors$weather2)
#regressors$weather3 <- as.factor(regressors$weather3)
#regressors$weather4 <- as.factor(regressors$weather4)

################## ARIMA with Regressors #################
regressorsTs <- ts(regressors[,c(1,2,3)],start = c(1,1), frequency = 24)
bikeshareTs <- ts(dataselected_jan2011$count, frequency = 24)
mod <-arima(bikeshareTs, xreg = regressorsTs, order = c(0,0,1))
tsdisplay(arima.errors(mod), main="ARIMA errors")

mod003<-arima(arima.errors(mod),order=c(0,0,3), include.mean=FALSE)

model1 <- auto.arima(bikeshareTs, xreg = regressorsTs)
trainset.bike <- bikeshareTs[1:336]
trainset.reg <- regressorsTs[1:336,]
testset.bike <- bikeshareTs[337:456]
testset.reg <- regressorsTs[337:456,]

modelAuto <- auto.arima(trainset.bike, xreg = trainset.reg)

modelAuto.pred <- forecast(modelAuto, xreg = testset.reg, h = 120)
MAPE1 <-mean(abs(modelAuto.pred$mean - testset.bike)/testset.bike)*100

plot(modelAuto.pred)
## Observations
## Just like the MA(N=24), the forecasted values are representative of Moving average which shows lower magnitude in non working days and higher on working days 

#################### TRY other ARIMA models ##############################
## Set d, D 0:1 
## Choose P, Q = 0:2
## Choose p,q = 0:5
## Calculate and save MAPE, AIC with parameters.

results <- data.frame(p = integer(),d = integer(),q = integer(),
                      P = integer(),D = integer(),Q = integer(),
                      MAPE = double(), AIC = double()
                      )
results[nrow(results)+1,] <-c(0,0,0,0,0,0,0,0)

for(p in 1:5){
  for(q in 1:5){
    for(d in 0:1){
    D =1
    P = 1
    Q = 1

    forecastmodel <- arima(trainset.bike, xreg = trainset.reg, order=c(p,d,q),seasonal=list(order=c(P,D,Q),period=24))
    residuals <-as.vector(residuals(forecastmodel))
    fittedValues<-as.vector(fitted(forecastmodel))
    Box.test(residuals,lag=48,fitdf=3,type="Ljung")
    
    #plot ACF/PACF of residuals
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
    plot(dataselected_jan2011[1:336,],type="p",pch=16,cex=0.5,xlab="Day of the month",ylab="Total Count",main="Fitted Values vs Actuals")
    fittedValuesDF<-data.frame(dataselected_jan2011$datetime[1:336],fittedValues)
    lines(fittedValuesDF, col="red")
    legend("topleft",c("y(t)","yhat(t)"),pch=c(16,NA),col=c("black","red"),lwd=c(NA,0.5),cex=0.55)
  }
  }
}

p = 1
d = 1
q = 1
P = 1
D = 1
Q = 1

forecastmodel <- arima(trainset.bike, xreg = trainset.reg, order=c(p,d,q),seasonal=list(order=c(P,D,Q),period=24))
residuals <-as.vector(residuals(forecastmodel))
fittedValues<-as.vector(fitted(forecastmodel))
Box.test(residuals,lag=48,fitdf=3,type="Ljung")

#plot ACF/PACF of residuals
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
plot(dataselected_jan2011[1:336,],type="p",pch=16,cex=0.5,xlab="Day of the month",ylab="Total Count",main="Fitted Values vs Actuals")
fittedValuesDF<-data.frame(dataselected_jan2011$datetime[1:336],fittedValues)
lines(fittedValuesDF, col="red")
legend("topleft",c("y(t)","yhat(t)"),pch=c(16,NA),col=c("black","red"),lwd=c(NA,0.5),cex=0.55)



m.pred <- forecast(forecastmodel, xreg = testset.reg, h = 120)
MAPE1 <-mean(abs(m.pred$mean - testset.bike)/testset.bike)*100

plot(m.pred)
