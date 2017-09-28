########## EDA ##########

bikeshare <- read.csv("train.csv", header = T, sep = ",")

## look at the variables
summary(bikeshare)

## Factorize holiday, workingday, season, weather
bikeshare$season <- as.factor(bikeshare$season)
bikeshare$holiday <- as.factor(bikeshare$holiday)
bikeshare$workingday <- as.factor(bikeshare$workingday)
bikeshare$weather <- as.factor(bikeshare$weather)

## convert "datetime" column datatype to posxict
bikeshare$datetime <-as.POSIXct(bikeshare$datetime,tz="","%Y-%m-%d %H:%M:%S")

summary(bikeshare)

## From the above summary, some observations below 
## there are 311 holidays in the given dataset. 
## There are 3474 non-working days which includes(weekends, holidays)
## There are three bikeshare counts: Casual, Registered, Count(represents total)
## The total count has atleast 1 ridership at any given hour.
## The temperature has reached max of 41 degree celcius.
## The actual feel has reached max of 45.45 degree celcius
## There is only one rider in weather 4 (Heavy Rain + Ice Pallets + Thunderstorm + Mist, Snow + Fog)

## plotting all the variables against three counts. 
## attach the bikeshare to easily access its variables
attach(bikeshare)

## Plot casual, registered, count against Season
## season is represented as 1 = spring, 2 = summer, 3 = fall, 4 = winter
par(mfrow = c(1,3))
plot(season, casual, main="Season vs Casual riders", xlab="Season", ylab="Casual", col="blue")
plot(season, registered, main="Season vs Registered riders", xlab="Season", ylab="Registered", col="green")
plot(season, count, main="Season vs Total riders", xlab="Season", ylab="Total Count", col = "red")

## Observations: 
## 1. Casual riders are more during summer(2) and fall(3)
## 2. Regsitered riders are less in spring(1) in contrast to almost consistent riders in all other seasons

## Plot casual, registered, count against Holiday
par(mfrow = c(1,3))
plot(holiday, casual, main="Holiday vs Casual riders", xlab="Holiday", ylab="Casual", col="blue")
plot(holiday, registered, main="Holiday vs Registered riders", xlab="Holiday", ylab="Registered", col="green")
plot(holiday, count, main="Holiday vs Total riders", xlab="Holiday", ylab="Total Count", col = "red")

## Observations:
## 1. Casual riders are more on holidays
## 2. Registered riders are slightly less on holidays.
## 2. Total riders are slightly more on holidays

## Plot casual, registered, count against Weather
par(mfrow = c(1,3))
plot(weather, casual, main="Weather vs Casual riders", xlab="Weather", ylab="Casual", col="blue")
plot(weather, registered, main="Weather vs Registered riders", xlab="Weather", ylab="Registered", col="green")
plot(weather, count, main="Weather vs Total riders", xlab="Weather", ylab="Total Count", col = "red")

## Observations
## Weather is represented as  
## 1: Clear, Few clouds, Partly cloudy, Partly cloudy 
## 2: Mist + Cloudy, Mist + Broken clouds, Mist + Few clouds, Mist 
## 3: Light Snow, Light Rain + Thunderstorm + Scattered clouds, Light Rain + Scattered clouds 
## 4: Heavy Rain + Ice Pallets + Thunderstorm + Mist, Snow + Fog 
## There are high number of riders on Clear skies than other weather. 
## There is one row for Weather 4.

## Plot casual, registered, count against WorkingDay
par(mfrow = c(1,3))
plot(workingday, casual, main="WorkingDay vs Casual riders", xlab="WorkingDay", ylab="Casual", col="blue")
plot(workingday, registered, main="WorkingDay vs Registered riders", xlab="WorkingDay", ylab="Registered", col="green")
plot(workingday, count, main="WorkingDay vs Total riders", xlab="WorkingDay", ylab="Total Count", col = "red")

## Observations:
## Less number of casual riders on workingdays.
## Relatively equal number of registered riders on both days.


## Plot casual, registered, count against Temperature
par(mfrow = c(1,3))
plot(temp, casual, main="Temperature vs Casual riders", xlab="Temperature", ylab="Casual", col="blue")
plot(temp, registered, main="Temperature vs Registered riders", xlab="Temperature", ylab="Registered", col="green")
plot(temp, count, main="Temperature vs Total riders", xlab="Temperature", ylab="Total Count", col = "red")

## Observations:
## The casual ridership grows more after 10 degree celcius
## The Registered ridership grows after 5 degree celcius 


## Plot casual, registered, count against Feels like Temperature
par(mfrow = c(1,3))
plot(atemp, casual, xaxp = c(0,50,10),main="Feels like Temp vs Casual riders", xlab="Feels like Temp", ylab="Casual", col="blue")
plot(atemp, registered,xaxp = c(0,50,10), main="Feels like Temp vs Registered riders", xlab="Feels like Temp", ylab="Registered", col="green")
plot(atemp, count,xaxp = c(0,50,10),  main="Feels like Temp vs Total riders", xlab="Feels like Temp", ylab="Total Count", col = "red")

## Observations:
## There are low riderships at 19 and 28 degress celcius in all the counts.


## Plot casual, registered, count against Humidity
par(mfrow = c(1,3))
plot(humidity, casual,main="Humidity vs Casual riders", xlab="Humidity", ylab="Casual", col="blue")
plot(humidity, registered, main="Humidity vs Registered riders", xlab="Humidity", ylab="Registered", col="green")
plot(humidity, count, main="Humidity vs Total riders", xlab="Humidity", ylab="Total Count", col = "red")

## Observations:
## All plots look similar.

## Plot casual, registered, count against Wind Speed
par(mfrow = c(1,3))
plot(windspeed, casual,xaxp = c(0,70,20),main="Wind Speed vs Casual riders", xlab="Wind Speed", ylab="Casual", col="blue")
plot(windspeed, registered,xaxp = c(0,70,20), main="Wind Speed vs Registered riders", xlab="Wind Speed", ylab="Registered", col="green")
plot(windspeed, count, xaxp = c(0,70,20),main="Wind Speed vs Total riders", xlab="Wind Speed", ylab="Total Count", col = "red")

## Observations:
## all plots look similar.
## No data for windspeeds from 1-5

par(mfrow = c(1,1))
## Check for Time series 
plot(datetime, count, main = "Datetime vs Total Count", xlab="Date Time", ylab = "Total Count", col="red" )
## This plot is not helpful to determine whether the ridership is seasonal or not
## The gaps between blocks of count are the unavalability of data from 20 th day of every month
plot(datetime, count, main = "Datetime vs Total Count", xlab="Date Time", ylab = "Total Count", type="l", col="red" )
## This plot reveal little information about seasonality

bikeshare$YearMonth <- strftime(datetime, "%Y-%m" )
bikeshare$YearMonth <- as.factor(bikeshare$YearMonth)

plot(bikeshare$YearMonth, count, las=2, main="Month vs Total Count", xlab="Year-Month", ylab="Total Count", col="red")

## Observations:
## There is a yearly seasonality but the magnitude of ridership has significantly increased from 2011 to 2012.

bikeshare$hour <- strftime(datetime, "%H")
bikeshare$hour<- as.factor(bikeshare$hour)
par(mfrow = c(1,3))
plot(bikeshare$hour, casual,main="Hour vs Casual riders", xlab="Hour", ylab="Casual", col="blue")
plot(bikeshare$hour, registered, main="Hour vs Registered riders", xlab="Hour", ylab="Registered", col="green")
plot(bikeshare$hour, count, main="Hour vs Total riders", xlab="Hour", ylab="Total Count", col = "red")

## Observations
## The plots above show characteristics of daily/hourly seasonality
## Casual ridership is very low during 4 AM and rises after 7 AM. It shows most ridership during early afternoon through the evening.
## Registered riders are very low during 4 AM. They are high during peak hours 7 Am - 8 AM and 5 PM to 6 PM, showing characteristics of office goers.
## The Total count resembles Registered users except that the range of riders during early afternoon through the evening is increased due to casual riders.


############################## Plot data for different months #################################


## create a data frame with hourly sequence for a month
## use this as a table to join with our data frame to get a month's data
library("plyr")
par(mfrow = c(1,1))
## Jan 2011 Data
start <- as.POSIXct('2011-01-01 00:00:00', tz="EST")
end <- as.POSIXct('2011-01-19 23:00:00', tz="EST")
time = data.frame(seq.POSIXt(from=start, to=end , by="1 hour"))
colnames(time)= c("datetime")

evenlyspacedData <- join(time, bikeshare, by = "datetime", type="left")
dataselected_jan2011 <- evenlyspacedData[,c(1,12)]

par(mar=c(12,4,3,1))
startdate = as.POSIXct("2011-01-01 00:00:00", tz="","%Y-%m-%d %H:%M:%S")
enddate = as.POSIXct("2011-01-19 23:00:00", tz="","%Y-%m-%d %H:%M:%S")
plot(dataselected_jan2011, axes = FALSE,  type="n", xlab="", main="Jan 2011 Data", ylab="Total count" )
lines(dataselected_jan2011, col="red")
axis(2)
axis.POSIXct(1, # a special kind of axis that uses dates, 1 means place it on the bottom, 2 means place it on the left side
             dataselected_jan2011$datetime, #the second parameter is the entries that you want plotted on the x axis
             at=seq(from=startdate,to=enddate,by="12 hour"), #the at parameter is a sequence that states which of the times you actually want to show up
             format="%A, %d %H:%M", # format is the way you want to see the dates, see strptime for more options
             las=2) #las = 2 (turn the labels vertical), 
mtext("Date Time", side=1, line=8)
box()

## Since Monday 17, 2011 was a holiday, the ridership follows the same pattern as the weekend.

## Mar 2011 Data
start <- as.POSIXct('2011-03-01 00:00:00', tz="EST")
end <- as.POSIXct('2011-03-19 23:00:00', tz="EST")
time = data.frame(seq.POSIXt(from=start, to=end , by="1 hour"))
colnames(time)= c("datetime")

evenlyspacedData <- join(time, bikeshare, by = "datetime", type="left")
dataselected_mar2011 <- evenlyspacedData[,c(1,12)]

par(mar=c(12,4,3,1))

plot(dataselected_mar2011, axes = FALSE,  type="n", xlab="", main="March 2011 Data", ylab="Total count" )
lines(dataselected_mar2011, col= "red")
axis(2)
axis.POSIXct(1, # a special kind of axis that uses dates, 1 means place it on the bottom, 2 means place it on the left side
             dataselected_mar2011$datetime, #the second parameter is the entries that you want plotted on the x axis
             at=seq(from=start,to=end,by="12 hour"), #the at parameter is a sequence that states which of the times you actually want to show up
             format="%A, %d %H:%M", # format is the way you want to see the dates, see strptime for more options
             las=2) #las = 2 (turn the labels vertical), 
mtext("Date Time", side=1, line=8)
box()



## May 2011 Data
start <- as.POSIXct('2011-05-01 00:00:00', tz="EST")
end <- as.POSIXct('2011-05-19 23:00:00', tz="EST")
time = data.frame(seq.POSIXt(from=start, to=end , by="1 hour"))
colnames(time)= c("datetime")

evenlyspacedData <- join(time, bikeshare, by = "datetime", type="left")
dataselected_may2011 <- evenlyspacedData[,c(1,12)]

par(mar=c(12,4,3,1))

plot(dataselected_may2011, axes = FALSE,  type="n", xlab="", main="May 2011 Data", ylab="Total count" )
lines(dataselected_may2011, col= "red")
axis(2)
axis.POSIXct(1, # a special kind of axis that uses dates, 1 means place it on the bottom, 2 means place it on the left side
             dataselected_may2011$datetime, #the second parameter is the entries that you want plotted on the x axis
             at=seq(from=start,to=end,by="12 hour"), #the at parameter is a sequence that states which of the times you actually want to show up
             format="%A, %d %H:%M", # format is the way you want to see the dates, see strptime for more options
             las=2) #las = 2 (turn the labels vertical), 
mtext("Date Time", side=1, line=8)
box()



## Aug 2011 Data
start <- as.POSIXct('2011-08-01 00:00:00', tz="EST")
end <- as.POSIXct('2011-08-19 23:00:00', tz="EST")
time = data.frame(seq.POSIXt(from=start, to=end , by="1 hour"))
colnames(time)= c("datetime")

evenlyspacedData <- join(time, bikeshare, by = "datetime", type="left")
dataselected_aug2011 <- evenlyspacedData[,c(1,12)]

par(mar=c(12,4,3,1))

plot(dataselected_aug2011, axes = FALSE,  type="n", xlab="", main="August 2011 Data", ylab="Total count" )
lines(dataselected_aug2011, col= "red")
axis(2)
axis.POSIXct(1, # a special kind of axis that uses dates, 1 means place it on the bottom, 2 means place it on the left side
             dataselected_aug2011$datetime, #the second parameter is the entries that you want plotted on the x axis
             at=seq(from=start,to=end,by="12 hour"), #the at parameter is a sequence that states which of the times you actually want to show up
             format="%A, %d %H:%M", # format is the way you want to see the dates, see strptime for more options
             las=2) #las = 2 (turn the labels vertical), 
mtext("Date Time", side=1, line=8)
box()


## Oct 2011 Data
start <- as.POSIXct('2011-10-01 00:00:00', tz="EST")
end <- as.POSIXct('2011-10-19 23:00:00', tz="EST")
time = data.frame(seq.POSIXt(from=start, to=end , by="1 hour"))
colnames(time)= c("datetime")

evenlyspacedData <- join(time, bikeshare, by = "datetime", type="left")
dataselected_oct2011 <- evenlyspacedData[,c(1,12)]

par(mar=c(12,4,3,1))

plot(dataselected_oct2011, axes = FALSE,  type="n", xlab="", main="October 2011 Data", ylab="Total count" )
lines(dataselected_oct2011, col= "red")
axis(2)
axis.POSIXct(1, # a special kind of axis that uses dates, 1 means place it on the bottom, 2 means place it on the left side
             dataselected_oct2011$datetime, #the second parameter is the entries that you want plotted on the x axis
             at=seq(from=start,to=end,by="12 hour"), #the at parameter is a sequence that states which of the times you actually want to show up
             format="%A, %d %H:%M", # format is the way you want to see the dates, see strptime for more options
             las=2) #las = 2 (turn the labels vertical), 
mtext("Date Time", side=1, line=8)
box()

## There are low ridership on Wednesday October 19, 2011 compared to other weekdays, a possible anamoly.

## Dec 2011 Data
start <- as.POSIXct('2011-12-01 00:00:00', tz="EST")
end <- as.POSIXct('2011-12-19 23:00:00', tz="EST")
time = data.frame(seq.POSIXt(from=start, to=end , by="1 hour"))
colnames(time)= c("datetime")

evenlyspacedData <- join(time, bikeshare, by = "datetime", type="left")
dataselected_dec2011 <- evenlyspacedData[,c(1,12)]

par(mar=c(12,4,3,1))

plot(dataselected_dec2011, axes = FALSE,  type="n", xlab="", main="December 2011 Data", ylab="Total count" )
lines(dataselected_dec2011, col= "red")
axis(2)
axis.POSIXct(1, # a special kind of axis that uses dates, 1 means place it on the bottom, 2 means place it on the left side
             dataselected_dec2011$datetime, #the second parameter is the entries that you want plotted on the x axis
             at=seq(from=start,to=end,by="12 hour"), #the at parameter is a sequence that states which of the times you actually want to show up
             format="%A, %d %H:%M", # format is the way you want to see the dates, see strptime for more options
             las=2) #las = 2 (turn the labels vertical), 
mtext("Date Time", side=1, line=8)
box()

## There are low ridership on Wednesday Dec 7, 2011 compared to other weekdays, a possible anamoly.



########################### Data Cleaning ############################################

## Total number of rows needed are count for 24 hours for 19 days for 24 months.
19*24*24

## But we only have 10866 samples
19*24*24 - 10866 ## Total number of missing values


## There are 78 missing values all over the dataset. 

## Jan 2011 data
par(mar=c(12,4,3,1))
startdate = as.POSIXct("2011-01-01 00:00:00", tz="","%Y-%m-%d %H:%M:%S")
enddate = as.POSIXct("2011-01-19 23:00:00", tz="","%Y-%m-%d %H:%M:%S")
plot(dataselected_jan2011, axes = FALSE,  type="n", xlab="", main="Jan 2011 Data", ylab="Total count" )
lines(dataselected_jan2011, col="red")
axis(2)
axis.POSIXct(1, # a special kind of axis that uses dates, 1 means place it on the bottom, 2 means place it on the left side
             dataselected_jan2011$datetime, #the second parameter is the entries that you want plotted on the x axis
             at=seq(from=startdate,to=enddate,by="12 hour"), #the at parameter is a sequence that states which of the times you actually want to show up
             format="%A, %d %H:%M", # format is the way you want to see the dates, see strptime for more options
             las=2) #las = 2 (turn the labels vertical), 
mtext("Date Time", side=1, line=8)
box()

## There are different interpolation techniques
##install.packages("zoo")
library("zoo")
fit1 <- na.approx(dataselected_jan2011$count)
plot(fit1, type="l")
## Approx,which uses linear interpolation, cannot be used. It does not impute the last 5 missing values
##install.packages("forecast")
library("forecast")
fit2 <- na.interp(dataselected_jan2011$count)
plot(fit2, type="l")
## interp, which uses periodic stl decompostion, can be used.

fit3<- na.spline(dataselected_jan2011$count)
plot(fit3, type="l" )
## Spline interpolation cannot be used since it imputes very low values at the end using polynomial interpolation.

## impute for all the sample months
dataselected_jan2011$count <- na.interp(dataselected_jan2011$count)
dataselected_mar2011$count <- na.interp(dataselected_mar2011$count)
dataselected_may2011$count <- na.interp(dataselected_may2011$count)
dataselected_aug2011$count <- na.interp(dataselected_aug2011$count)
dataselected_oct2011$count <- na.interp(dataselected_oct2011$count)
dataselected_dec2011$count <- na.interp(dataselected_dec2011$count)
par(mfrow=c(1,1))
plot(dataselected_jan2011$count, type="l")
plot(dataselected_mar2011$count, type="l")
plot(dataselected_may2011$count, type="l")
plot(dataselected_aug2011$count, type="l")
plot(dataselected_oct2011$count, type="l")
plot(dataselected_dec2011$count, type="l")

###################### Smoothing ##################################

## Smoothing for Jan 2011 data
par(mar=c(9,4,4,1))
movingAverage5Periods = rollmean(dataselected_jan2011$count,5)
par(mfrow = c(1,1)) #make two graphs one above another
plot(dataselected_jan2011$count, type="l", xlab = 'Time',ylab = "Rental count", xaxt="n",main="Moving Average N=5 (January , 2011)", ylim =c(0, 300))
axis(1,at=c(seq(0,456,7)))
points(dataselected_jan2011$count, pch = 16, cex=0.5)
lines(movingAverage5Periods,col="red", lwd=2)
legend("topright", c("Actual","Smoothed"), lty=c(1,1), lwd = c(2,2),col = c("black","red"))

## Observations:
## The 5 hour moving average window smoothes the variance of Total count. 
## This plot reveal that the ridership transitions from high to low and high again within a day, during Workingdays.
## It also reveals that high ridership occurs only once during holidays and weekends.

movingAverage7Periods = rollmean(dataselected_jan2011$count,7)
par(mfrow = c(1,1)) #make two graphs one above another
plot(dataselected_jan2011$count, type="l", xlab = 'Time',ylab = "Rental count", xaxt="n",main="Moving Average N=7 (January , 2011)", ylim =c(0, 300))
axis(1,at=c(seq(0,456,7)))
points(dataselected_jan2011$count, pch = 16, cex=0.5)
lines(movingAverage7Periods,col="red", lwd=2)
legend("topright", c("Actual","Smoothed"), lty=c(1,1), lwd = c(2,2),col = c("black","red"))


movingAverage8Periods = rollmean(dataselected_jan2011$count,8)
par(mfrow = c(1,1)) #make two graphs one above another
plot(dataselected_jan2011$count, type="l", xlab = 'Time',ylab = "Rental count", xaxt="n",main="Moving Average N=8 (January , 2011)", ylim =c(0, 300))
axis(1,at=c(seq(0,456,7)))
points(dataselected_jan2011$count, pch = 16, cex=0.5)
lines(movingAverage8Periods,col="red", lwd=2)
legend("topright", c("Actual","Smoothed"), lty=c(1,1), lwd = c(2,2),col = c("black","red"))


movingAverage10Periods = rollmean(dataselected_jan2011$count,10)
par(mfrow = c(1,1)) #make two graphs one above another
plot(dataselected_jan2011$count, type="l", xlab = 'Time',ylab = "Rental count", xaxt="n",main="Moving Average N=10 (January , 2011)", ylim =c(0, 300))
axis(1,at=c(seq(0,456,10)))
points(dataselected_jan2011$count, pch = 16, cex=0.5)
lines(movingAverage10Periods,col="red", lwd=2)
legend("topright", c("Actual","Smoothed"), lty=c(1,1), lwd = c(2,2),col = c("black","red"))


movingAverage12Periods = rollmean(dataselected_jan2011$count,12)
par(mfrow = c(1,1)) #make two graphs one above another
plot(dataselected_jan2011$count, type="l", xlab = 'Time',ylab = "Rental count", xaxt="n",main="Moving Average N=12 (January , 2011)", ylim =c(0, 300))
axis(1,at=c(seq(0,456,10)))
points(dataselected_jan2011$count, pch = 16, cex=0.5)
lines(movingAverage12Periods,col="red", lwd=2)
legend("topright", c("Actual","Smoothed"), lty=c(1,1), lwd = c(2,2),col = c("black","red"))


movingAverage24Periods = rollmean(dataselected_jan2011$count,24)
par(mfrow = c(1,1)) #make two graphs one above another
plot(dataselected_jan2011$count, type="l", xlab = 'Time',ylab = "Rental count", xaxt="n",main="Moving Average N=24 (January , 2011)", ylim =c(0, 300))
axis(1,at=c(seq(0,456,7)))
points(dataselected_jan2011$count, pch = 16, cex=0.5)
lines(movingAverage24Periods,col="red", lwd=2)
legend("topright", c("Actual","Smoothed"), lty=c(1,1), lwd = c(2,2),col = c("black","red"))

## Observations:
## The 24 hour moving average shows the average ridership every 24 hours. 
## This plot reveals the average high ridership during working days and relatively lower ridership during holidays/weekends


