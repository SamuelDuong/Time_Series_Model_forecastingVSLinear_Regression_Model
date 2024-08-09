rm(list = ls())


library(forecast)
sales.data <- read.csv("C:\\Users\\duong\\OneDrive\\Desktop\\QUAN\\Spring 2024\\Big Data Analytics\\SouvenirSales.csv")
# create time series object using ts()
# ts() takes three arguments: start, end, and freq. 
# with monthly data, the frequency of periods per season is 12 (per year). 
# arguments start and end are (season number, period number) pairs. 
# here start is Jan 1995: start = c(1995, 1); end is Dec 2001: end = c(2001, 12).
sales.ts <- ts(sales.data$Sales, 
                   start = c(1995, 1), end = c(2001, 12), freq = 12)

# plot the series
plot(sales.ts, xlab = "Time", ylab = "Sales in $AUD", 
     ylim = c(1600, 120000))


# fitting a linear regression model to the time series, using quadratic trend.
# https://www.desmos.com/calculator/pfdwlq5qht
# tslm(): This function is used to fit a linear model to time series data in R. 
# It stands for "Time Series Linear Model."
# The I() function is used to indicate that trend^2 should be treated as 
# a separate feature rather than an exponentiation operation.
# Variables 'trend' and 'season' are created by the tslm() function on the fly.

sales.lm <- tslm(sales.ts ~ trend + I(trend^2))
summary(sales.lm)

# overlay the fitted values of the linear model
lines(sales.lm$fitted, lwd = 2) # lwd= line width

# partition the data
nValid <- 12 # length
nTrain <- length(sales.ts) - nValid
#A naive forecast is one in which the forecast for a given period is simply equal to the value observed in the previous period.
train.ts <- window(sales.ts, start = c(1995, 1), 
                   end = c(1995, nTrain))
valid.ts <- window(sales.ts, start = c(1995, nTrain + 1), 
                   end = c(1995, nTrain + nValid))

#  generate the seasonal naive forecasts

snaive.pred <- snaive(train.ts, h = nValid)

# plot forecasts and actuals in the training and validation sets
par(mfrow = c(1, 1))
plot(train.ts, ylim = c(1200, 2500),  ylab = "Sales", 
     xlab = "Time", bty = "l", 
     xaxt = "n", xlim = c(1995,2003.25), main = "")
# bty (box type) - c("o","l","7","c","u","n")

axis(1, at = seq(1995, 2003, 1), labels = format(seq(1995, 2003, 1)))
lines(snaive.pred$mean, lwd = 2, col = "blue", lty = 1)
617730
lines(valid.ts, col = "grey20", lty = 3)
lines(c(2001.25 - 1, 2001.25 - 1), c(0, 3500)) 
lines(c(2001.25, 2001.25), c(0, 3500))

# Prediction Accuracy
accuracy(snaive.pred, valid.ts)


#########################
# Trend and Seasonality
#########################

# produce a model with only linear trend 
sales.lm <- tslm(sales.ts ~ trend)

# plot the series
plot(sales.ts, xlab = "Time", ylab = "Sales", 
     ylim = c(1600,120000),
     bty = "l")
lines(sales.lm$fitted, lwd = 2)

# fit linear trend model to training set and create forecasts
train.lm <- tslm(train.ts ~ trend)
train.lm.pred <- forecast(train.lm, h = nValid, level = 0)
summary(train.lm)

# Plot forecasted values and residuals
par(mfrow = c(1, 1))

plot(train.lm.pred, ylim = c(1600,120000),  ylab = "Sales in AUD", xlab = "Time", 
     bty = "l", xaxt = "n", xlim = c(1995,2003.5), main = "", flty = 2)
axis(1, at = seq(1995, 2003, 1), labels = format(seq(1995, 2003, 1)))
lines(train.lm.pred$fitted, lwd = 2, col = "blue")
lines(valid.ts)

axis(1, at = seq(1995, 2003, 1), labels = format(seq(1995, 2003, 1)))



# fit linear trend using tslm() with argument lambda = 1 (no transform of y)
train.lm.linear.trend <- tslm(train.ts ~ trend, lambda = 1)
train.lm.linear.trend.pred <- forecast(train.lm.linear.trend, 
                                       h = nValid, level = 0)

plot(train.lm.expo.trend.pred, ylim = c(1600,150000),  ylab = "Ridership", 
     xlab = "Time", bty = "l", xaxt = "n", xlim = c(1995,2003.25), main = "", flty = 2)
lines(train.ts)
axis(1, at = seq(1995, 2003, 1), labels = format(seq(1995, 2003, 1)))
lines(train.lm.expo.trend.pred$fitted, lwd = 2, col = "blue")  # Added in 6-5
lines(train.lm.linear.trend.pred$fitted, lwd = 2, col = "black", lty = 3)
lines(train.lm.linear.trend.pred$mean, lwd = 2, col = "black", lty = 3)
lines(valid.ts)


# include season as a predictor in tslm(). Here it creates 11 dummies, 
# one for each month except for the first season, January.  
train.lm.season <- tslm(train.ts ~ season)
summary(train.lm.season)
?tslm
train.lm.season <- tslm(train.ts ~ season,lambda = 0) # lambda = 0 refers to log transformation.
summary(train.lm.season)

# Model with both linear trend and seasonality
train.lm.trend.season <- tslm(train.ts ~ trend + season)
summary(train.lm.trend.season)
lm.trend.season.pred <- forecast(train.lm.trend.season, h = nValid)
accuracy(lm.trend.season.pred, valid.ts)




# run Holt-Winters exponential smoothing
# use ets() with option model = "MAA" to fit Holt-Winter's exponential smoothing 
# with multiplicative error, additive trend, and additive seasonality. 
hwin <- ets(train.ts, model = "MAA")

# create predictions
hwin.pred <- forecast(hwin, h = nValid, level = 0)

# plot the series
plot(hwin.pred, ylim = c(1000, 120000),  ylab = "Sales", xlab = "Time", 
     bty = "l", xaxt = "n", xlim = c(1995,2003.25), main = "", flty = 2)
##plot(train.ts, ylim = c(1300, 2600),  ylab = "Ridership", xlab = "Time",  bty = "l", xaxt = "n", xlim = c(1991,2006.25), main = "", flty = 2)
axis(1, at = seq(1995, 2003, 1), labels = format(seq(1995, 2003, 1)))
lines(hwin.pred$fitted, lwd = 2, col = "blue")
#lines(hwin.pred$mean, lwd=2, col="green", lty=1)
lines(valid.ts)

accuracy(hwin.pred, valid.ts)
accuracy(train.lm.trend.season.pred, valid.ts)

hwin
