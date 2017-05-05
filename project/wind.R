###########################################
###              wind.R                 ###
###########################################

library(xtable)
par.ori <- par()

######     1. read data     ######
wind <- read.table("K_windspeed.txt", header = TRUE)
head(wind)
# check missing value
sum(is.na(wind))
# data set from the wind tower in the Pacific North-West of the United States.
# November 16, 2007 till December 30 2008.
# Wind speed is measured in meters per second while wind directions are recorded in degrees
# construct forecasts of wind speed for 1am to 11.59pm on December 31, 2008

timestr <- with(wind, paste0(2000 + Year, '-', Month, '-', Day, ' ', Hour, ':00:00'))
time <- strptime(timestr, "%Y-%m-%d %H:%M:%S")

#######      2. exploratory analysis     ######

wind.ts <- ts(as.matrix(wind[, c(5, 6)]), frequency = 24)
plot(wind.ts)
acf(wind.ts)
pacf(wind.ts)

wts <- data.frame(speed = ts(wind[, 5], frequency = 24),
                  direction = ts(wind[, 6], frequency = 24))
par(mfrow = c(2, 3))
plot(wts$speed)
acf(wts$speed)
pacf(wts$speed)
plot(wts$direction)
acf(wts$direction)
pacf(wts$direction)

# acf and pacf
par(mfrow = c(2, 3))
acf(wts$speed)
acf(diff(wts$speed))
acf(diff(diff(wts$speed)))
pacf(wts$speed)
pacf(diff(wts$speed))
pacf(diff(diff(wts$speed)))

# unit root
if (!require(tseries)) install.packages("tseries")
library(tseries)
adf.test(wts$speed, k = 0)
adf.test(wts$speed, k = 1)
adf.test(wts$speed, k = 2)

adf.test(diff(wts$speed), k = 0)
adf.test(diff(wts$speed), k = 1)
adf.test(diff(wts$speed), k = 2)


if (!require(TSA)) install.packages("TSA")
library(TSA)
plot(armasubsets(y = wts$speed,
                 nar = 24,
                 nma = 24,
                 y.name = 'test',
                 ar.method = 'ols'))
plot(armasubsets(y = diff(wts$speed),
                 nar = 24,
                 nma = 24,
                 y.name = 'test',
                 ar.method = 'ols'))

######     3. build models     ######

# training set
N <- dim(wts)[1]
train <- wts[1:(N - 24), ]
dim(train)

# testing set
test <- wts[(N - 23):N, ]
dim(test)

if (!require(forecast)) install.packages("forecast")
library(forecast)
arma11 <- Arima(train$speed, order = c(1, 0, 1), method = 'ML')
arma32 <- Arima(train$speed, order = c(3, 0, 2), method = 'ML')
arma33 <- Arima(train$speed, order = c(3, 0, 3), method = 'ML')

Box.test(arma11$residuals, lag = 12, type = "Ljung-Box")
Box.test(arma32$residuals, lag = 12, type = "Ljung-Box")
Box.test(arma33$residuals, lag = 12, type = "Ljung-Box")


arma33.fc <- forecast(arma33, h = 24)
str(arma33.fc)
arma33.fc$lower[, 2]

x <- data.frame(actual = test$speed,
                fcst = arma33.fc$mean,
                lb = arma33.fc$lower[, 2],
                ub = arma33.fc$upper[, 2])

matplot(x, type = "l",
        lty = c(1, 2, 3, 3),
        col = c(1, 2, 4, 4),
        main = "Test set: December 30 2008")

legend(18, 40, c("actual", "forecast"),
       lty = c(1, 2),
       col = c(1, 2))

RMSE <- sqrt(mean((x[, 1] - x[, 2]) ^ 2))

# result
mod1 <- Arima(wts$speed, order = c(3, 0, 3), method = 'ML')
mod1.fc <- forecast(mod1, h = 24)
x <- data.frame(fcst = mod1.fc$mean,
               lb = mod1.fc$lower[, 2],
               ub = mod1.fc$upper[, 2])

mod1 <- Arima(wts$speed, order = c(4, 0, 4), method = 'ML')

mod1.fc <- forecast(mod1, h = 24)
x <- data.frame(fcst = mod1.fc$mean,
               lb = mod1.fc$lower[, 2],
               ub = mod1.fc$upper[, 2])
x
