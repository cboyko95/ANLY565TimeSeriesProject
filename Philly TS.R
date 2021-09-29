setwd("C:\\Users\\cboyk\\OneDrive\\Desktop\\HU Analytics\\565 Time Series\\Research Project")

# Libraries
library(readxl)
library(tidyverse)

# Reading in data
philly <- read_excel("Philadelphia Precipitation 19702020.xlsx")

# Making a flag for week
week <- rep(1:2608,each = 7, 1)
week <- as.data.frame(week)
week2 <- rep(2609, each = 6, 1)
week2 <- as.data.frame(week2)
colnames(week2) <- "week"
week3 <- rbind(week, week2)
philly$week <- week3$week

# Making a Flag for Month
philly$month.year <- paste(philly$MONTH, philly$YEAR, sep = " ")

# Making a Flag for Season
philly$season.year <- paste(philly$SEASON, philly$YEAR, sep = " ")


# Making Week subset
philly_week <- philly %>% group_by(week) %>%
  summarize(PRCP = sum(PRCP))

# Making Month subset
philly_month <- philly %>% group_by(month.year) %>%
  summarize(PRCP = sum(PRCP))


# Making Seasonal Subset
philly_season <- philly %>% group_by(season.year) %>%
  summarize(PRCP = sum(PRCP))

# Making Yearly subset
philly_year <- philly %>% group_by(YEAR) %>%
  summarize(PRCP = sum(PRCP))

# Saving month and season series since they are sorted incorrectly 
library(openxlsx)
write.xlsx(philly_month, "Philly Month Series.xlsx")
write.xlsx(philly_season, "Philly Season Series.xlsx")

# Bringing Month and Season series back in
philly_month <- read_excel("Philly Month Series.xlsx")
philly_season <- read_excel("Philly Season Series.xlsx")

# Making TS
philly.daily.ts <- ts(philly$PRCP, start = c(1970), end = c(2019+364/365), freq = 365)
philly.weekly.ts <- ts(philly_week$PRCP, start = 1970, end = 2019 + 51/52, freq = 52)
philly.monthly.ts <- ts(philly_month$PRCP,  start = c(1970, 1), end = c(2019, 12), freq = 12)
philly.season.ts <- ts(philly_season$PRCP, start = 1970, end = 2019.75,freq = 4)
philly.year.ts <- ts(philly_year$PRCP, start = c(1970), end = c(2019), freq = 1)

# Making pre and post sets for each
philly.daily.ts.pre <- window(philly.daily.ts, end = 2019+354/365)
philly.daily.ts.post <- window(philly.daily.ts, start = 2019+355/365)
philly.weekly.ts.pre <- window(philly.weekly.ts, end = 2019+47/52)
philly.weekly.ts.post <- window(philly.weekly.ts, start = 2019+48/52)
philly.monthly.ts.pre <- window(philly.monthly.ts, end = c(2019, 8))
philly.monthly.ts.post <- window(philly.monthly.ts, start = c(2019, 9))
philly.season.ts.pre <- window(philly.season.ts, end = 2018.75) 
philly.season.ts.post <- window(philly.season.ts, start = 2019) 
philly.year.ts.pre <- window(philly.year.ts, end = 2015)
philly.year.ts.post <- window(philly.year.ts, start = 2016)

# Time series plots
ts.plot(philly.daily.ts)
ts.plot(philly.weekly.ts)
ts.plot(philly.monthly.ts)
ts.plot(philly.season.ts)
ts.plot(philly.year.ts)

# ACF plots
acf(philly.daily.ts)
acf(philly.weekly.ts)
acf(philly.monthly.ts)
acf(philly.season.ts)
acf(philly.year.ts)

# PACF plots
pacf(philly.daily.ts)
pacf(philly.weekly.ts)
pacf(philly.monthly.ts)
pacf(philly.season.ts)
pacf(philly.year.ts)

# Decomposition of each series
plot(decompose(philly.daily.ts))
plot(decompose(philly.weekly.ts))
plot(decompose(philly.monthly.ts))
plot(decompose(philly.season.ts))

# Holt Winters --------------------------------------------------------------------------------------
philly.daily.hw <- HoltWinters(philly.daily.ts.pre, beta =F, gamma = F) 
philly.daily.hw$SSE
philly.daily.hw.predict <- predict(philly.daily.hw, n.ahead=10)
ts.plot(philly.daily.ts.post, philly.daily.hw.predict, lty = c(1,3), col=c("red","blue"))
# Cannot do MAPE because of values being 0 so divide by 0 error

philly.weekly.hw <- HoltWinters(philly.weekly.ts.pre, beta =F, gamma = F) 
philly.weekly.hw$SSE
philly.weekly.hw.predict <- predict(philly.weekly.hw, n.ahead=4)
ts.plot(philly.weekly.ts.post, philly.weekly.hw.predict, lty = c(1,3), col=c("red","blue"))
sum((abs(philly.weekly.ts.post - philly.weekly.hw.predict))/(philly.weekly.ts.post))/
  length(philly.weekly.ts.post)*100 # 105.2883
acf(resid(philly.weekly.hw))

philly.monthly.hw <- HoltWinters(philly.monthly.ts.pre, beta =F, gamma = F) 
philly.monthly.hw$SSE
philly.monthly.hw.predict <- predict(philly.monthly.hw, n.ahead=4)
ts.plot(philly.monthly.ts.post, philly.monthly.hw.predict, lty = c(1,3), col=c("red","blue"))
sum((abs(philly.monthly.ts.post - philly.monthly.hw.predict))/(philly.monthly.ts.post))/
  length(philly.monthly.ts.post)*100 # 152.414
acf(resid(philly.monthly.hw))

philly.season.hw <- HoltWinters(philly.season.ts.pre, beta =F, gamma = F) 
philly.season.hw$SSE
philly.season.hw.predict <- predict(philly.season.hw, n.ahead=4)
ts.plot(philly.season.ts.post, philly.season.hw.predict, lty = c(1,3), col=c("red","blue"))
sum((abs(philly.season.ts.post - philly.season.hw.predict))/(philly.season.ts.post))/
  length(philly.season.ts.post)*100 # 31.84925
acf(resid(philly.season.hw))

philly.year.hw <- HoltWinters(philly.year.ts.pre, beta =F, gamma = F) 
philly.year.hw$SSE
philly.year.hw.predict <- predict(philly.year.hw, n.ahead=4)
ts.plot(philly.year.ts.post, philly.year.hw.predict, lty = c(1,3), col=c("red","blue"))
sum((abs(philly.year.ts.post - philly.year.hw.predict))/(philly.year.ts.post))/
  length(philly.year.ts.post)*100 # 17.59806
acf(resid(philly.year.hw))

# AR(p) ------------------------------------------------------------------------------------------
philly.weekly.ar <- ar(philly.weekly.ts.pre)
philly.weekly.ar$order
philly.weekly.ar.predict <- predict(philly.weekly.ar, n.ahead = 4)
ts.plot(philly.weekly.ts.post, philly.weekly.ar.predict$pred, lty = c(1,3), col=c("red","blue"))
sum((abs(philly.weekly.ts.post - philly.weekly.ar.predict$pred))/(philly.weekly.ts.post))/
  length(philly.weekly.ts.post)*100 # 79.7363
philly.weekly.ar$ar[1] + c(-2, 2) * sqrt(as.numeric(philly.weekly.ar$asy.var.coef[1,1]))
philly.weekly.ar$ar[2] + c(-2, 2) * sqrt(as.numeric(philly.weekly.ar$asy.var.coef[2,2]))
philly.weekly.ar$ar[3] + c(-2, 2) * sqrt(as.numeric(philly.weekly.ar$asy.var.coef[3,3]))
philly.weekly.ar$ar[4] + c(-2, 2) * sqrt(as.numeric(philly.weekly.ar$asy.var.coef[4,4]))
philly.weekly.ar$ar[5] + c(-2, 2) * sqrt(as.numeric(philly.weekly.ar$asy.var.coef[5,5]))
philly.weekly.ar$ar[6] + c(-2, 2) * sqrt(as.numeric(philly.weekly.ar$asy.var.coef[6,6]))
acf(philly.weekly.ar$resid[-c(1:6)])

philly.monthly.ar <- ar(philly.monthly.ts.pre)
philly.monthly.ar$order
philly.monthly.ar.predict <- predict(philly.monthly.ar, n.ahead = 4)
ts.plot(philly.monthly.ts.post, philly.monthly.ar.predict$pred, lty = c(1,3), col=c("red","blue"))
sum((abs(philly.monthly.ts.post - philly.monthly.ar.predict$pred))/(philly.monthly.ts.post))/
  length(philly.monthly.ts.post)*100 # 113.6695
philly.monthly.ar$ar[1] + c(-2, 2) * sqrt(as.numeric(philly.monthly.ar$asy.var.coef[1,1]))
acf(na.omit(philly.monthly.ar$resid))

philly.season.ar <- ar(philly.season.ts.pre)
philly.season.ar$order
philly.season.ar.predict <- predict(philly.season.ar, n.ahead = 4)
ts.plot(philly.season.ts.post, philly.season.ar.predict$pred, lty = c(1,3), col=c("red","blue"))
sum((abs(philly.season.ts.post - philly.season.ar.predict$pred))/(philly.season.ts.post))/
  length(philly.season.ts.post)*100  # 33.31567
acf(na.omit(philly.season.ar$resid))

philly.year.ar <- ar(philly.year.ts.pre)
philly.year.ar$order
philly.year.ar.predict <- predict(philly.year.ar, n.ahead = 4)
ts.plot(philly.year.ts.post, philly.year.ar.predict$pred, lty = c(1,3), col=c("red","blue"))
sum((abs(philly.year.ts.post - philly.year.ar.predict$pred))/(philly.year.ts.post))/
  length(philly.year.ts.post)*100 # 16.38687
acf(na.omit(philly.year.ar$resid))

# Linear Regression ---------------------------------------------------------------------------------
Seas <- cycle(philly.weekly.ts.pre) # saving the seasons
Time <- time(philly.weekly.ts.pre) # saving the time
philly.weekly.lm <- lm(philly.weekly.ts.pre ~ 0 + Time + factor(Seas))
summary(philly.weekly.lm)
new.t <- seq(2019+48/52., len = 4, by = 1/52)
alpha <- coef(philly.weekly.lm)[1]
beta <- rep(coef(philly.weekly.lm)[2:52], 1)
new.dat <- data.frame(Time = new.t, Seas = rep(1:4, 1))
philly.weekly.lm.predict <- predict(philly.weekly.lm, new.dat)[1:4]
ts.plot(philly.weekly.ts.post, philly.weekly.lm.predict, lty = c(1,3), col=c("red","blue"))
sum((abs(philly.weekly.ts.post - philly.weekly.lm.predict))/(philly.weekly.ts.post))/
  length(philly.weekly.ts.post)*100 # 82.84284
acf(philly.weekly.lm$resid)

Seas <- cycle(philly.monthly.ts.pre) # saving the seasons
Time <- time(philly.monthly.ts.pre) # saving the time
philly.monthly.lm <- lm(philly.monthly.ts.pre ~ 0 + Time + factor(Seas))
summary(philly.monthly.lm)
new.t <- seq(2019+9/12, len = 4, by = 1/12)
alpha <- coef(philly.monthly.lm)[1]
beta <- rep(coef(philly.monthly.lm)[2:12], 1)
new.dat <- data.frame(Time = new.t, Seas = rep(1:4, 1))
philly.monthly.lm.predict <- predict(philly.monthly.lm, new.dat)[1:4]
ts.plot(philly.monthly.ts.post, philly.monthly.lm.predict, lty = c(1,3), col=c("red","blue"))
sum((abs(philly.monthly.ts.post - philly.monthly.lm.predict))/(philly.monthly.ts.post))/
  length(philly.monthly.ts.post)*100 # 96. 30368
acf(philly.monthly.lm$resid)

Seas <- cycle(philly.season.ts.pre) # saving the seasons
Time <- time(philly.season.ts.pre) # saving the time
philly.season.lm <- lm(philly.season.ts.pre ~ 0 + Time + factor(Seas))
summary(philly.season.lm)
new.t <- seq(2019, len = 4, by = 1/4)
alpha <- coef(philly.season.lm)[1]
beta <- rep(coef(philly.season.lm)[2:52], 1)
new.dat <- data.frame(Time = new.t, Seas = rep(1:4, 1))
philly.season.lm.predict <- predict(philly.season.lm, new.dat)[1:4]
ts.plot(philly.season.ts.post, philly.season.lm.predict, lty = c(1,3), col=c("red","blue"))
sum((abs(philly.season.ts.post - philly.season.lm.predict))/(philly.season.ts.post))/
  length(philly.season.ts.post)*100 # 30.16681
acf(philly.season.lm$resid)


Time <- time(philly.year.ts.pre) # saving the time
philly.year.lm <- lm(philly.year.ts.pre ~ 0 + Time)
summary(philly.year.lm)
new.t <- seq(2016, len = 4, by = 1)
alpha <- coef(philly.year.lm)[1]
new.dat <- data.frame(Time = new.t, Seas = rep(1:4, 1))
philly.year.lm.predict <- predict(philly.year.lm, new.dat)[1:4]
ts.plot(philly.year.ts.post, philly.year.lm.predict, lty = c(1,3), col=c("red","blue"))
sum((abs(philly.year.ts.post - philly.year.lm.predict))/(philly.year.ts.post))/
  length(philly.year.ts.post)*100 # 16.54327
acf(philly.year.lm$resid)

# Harmonic -----------------------------------------------------------------------------------------
SIN <- COS <- matrix(nr = length(philly.monthly.ts.pre), nc = 6)
for (i in 1:6) {
  SIN[, i] <- sin(2 * pi * i * time(philly.monthly.ts.pre))
  COS[, i] <- cos(2 * pi * i * time(philly.monthly.ts.pre))
}
TIME <- (time(philly.monthly.ts.pre) - mean(time(philly.monthly.ts.pre)))/sd(time(philly.monthly.ts.pre))
mean(time(philly.monthly.ts.pre))
sd(time(philly.monthly.ts.pre))
philly.monthly.ts.pre.lm1 <- lm(log(philly.monthly.ts.pre) ~ TIME + I(TIME^2) + I(TIME^3) + I(TIME^4) + SIN[,1] + COS[,1] + SIN[,2] + COS[,2] +
               SIN[,3] + COS[,3] + SIN[,4] + COS[,4] + SIN[,5] + COS[,5] + SIN[,6] + COS[,6])
coef(philly.monthly.ts.pre.lm1)/sqrt(diag(vcov(philly.monthly.ts.pre.lm1)))



# ARMA ---------------------------------------------------------------------------------------------
best.aic <- Inf
for (i in 0:6) for (j in 0:6) {
  fit.aic <- AIC(arima(philly.weekly.ts.pre, order = c(i, 0,j) ) )
  if (fit.aic < best.aic) {
    best.order <- c(i, 0, j)
    best.arma <- arima(philly.weekly.ts.pre, order = best.order)
    best.aic <- fit.aic
  }}
best.order
acf(resid(best.arma))
philly.weekly.arma.predict <- predict(best.arma, n.ahead = 4)
ts.plot(philly.weekly.ts.post, philly.weekly.arma.predict$pred, lty = c(1,3), col=c("red","blue"))
sum((abs(philly.weekly.ts.post - philly.weekly.arma.predict$pred))/(philly.weekly.ts.post))/
  length(philly.weekly.ts.post)*100 # 82.936

best.aic <- Inf
for (i in 0:6) for (j in 0:6) {
  fit.aic <- AIC(arima(philly.season.ts.pre, order = c(i, 0,j) ) )
  if (fit.aic < best.aic) {
    best.order <- c(i, 0, j)
    best.arma <- arima(philly.season.ts.pre, order = best.order)
    best.aic <- fit.aic
  }}
best.order
acf(resid(best.arma))
philly.season.arma.predict <- predict(best.arma, n.ahead = 4)
ts.plot(philly.season.ts.post, philly.season.arma.predict$pred, lty = c(1,3), col=c("red","blue"))
sum((abs(philly.season.ts.post - philly.season.arma.predict$pred))/(philly.season.ts.post))/
  length(philly.season.ts.post)*100 # 36.81892

best.aic <- Inf
for (i in 0:6) for (j in 0:6) {
  fit.aic <- AIC(arima(philly.monthly.ts.pre, order = c(i, 0,j) ) )
  if (fit.aic < best.aic) {
    best.order <- c(i, 0, j)
    best.arma <- arima(philly.monthly.ts.pre, order = best.order)
    best.aic <- fit.aic
  }}
best.order
acf(resid(best.arma))
philly.monthly.arma.predict <- predict(best.arma, n.ahead = 4)
ts.plot(philly.monthly.ts.post, philly.monthly.arma.predict$pred, lty = c(1,3), col=c("red","blue"))
sum((abs(philly.monthly.ts.post - philly.monthly.arma.predict$pred))/(philly.monthly.ts.post))/
  length(philly.monthly.ts.post)*100 # 129.563

best.aic <- Inf
for (i in 0:6) for (j in 0:6) {
  fit.aic <- AIC(arima(philly.year.ts.pre, order = c(i, 0,j) ) )
  if (fit.aic < best.aic) {
    best.order <- c(i, 0, j)
    best.arma <- arima(philly.year.ts.pre, order = best.order)
    best.aic <- fit.aic
  }}
best.order
acf(resid(best.arma))
philly.year.arma.predict <- predict(best.arma, n.ahead = 4)
ts.plot(philly.year.ts.post, philly.year.arma.predict$pred, lty = c(1,3), col=c("red","blue"))
sum((abs(philly.year.ts.post - philly.year.arma.predict$pred))/(philly.year.ts.post))/
  length(philly.year.ts.post)*100 # 19.63586


# ADF Test -----------------------------------------------------------------------------------------
library(tseries)
adf.test(philly.daily.ts)
adf.test(philly.weekly.ts)
adf.test(philly.monthly.ts)
adf.test(philly.season.ts)
adf.test(philly.year.ts)
adf.test(decompose(philly.year.ts)$random)

# SARIMA
get.best.sarima <- function(x.ts, maxord = c(1,1,1,1,1,1))
{
  best.aic <- 1e8
  n <- length(x.ts)
  for (p in 0:maxord[1]) for(d in 0:maxord[2]) for(q in 0:maxord[3])
    for (P in 0:maxord[4]) for(D in 0:maxord[5]) for(Q in 0:maxord[6])
    {
      fit <- arima(x.ts, order = c(p,d,q),
                   seas = list(order = c(P,D,Q),
                               frequency(x.ts)), method = "CSS", optim.control = list(maxit = 10000) )
      fit.aic <- -2 * fit$loglik + (log(n) + 1) * length(fit$coef)
      if (fit.aic < best.aic)
      {best.aic <- fit.aic
      best.fit <- fit
      best.model <- c(p,d,q,P,D,Q)
      }
    }
  list(best.aic, best.fit, best.model)
}
get.best.sarima(philly.year.ts.pre, maxord = c(2, 2, 2, 2, 2, 2))

pi.best.sarima <- arima(philly.year.ts.pre, order = c(0,0,2), seas=list(order=c(2,1,1),1))
philly.year.sarima.predict <- predict(pi.best.sarima, n.ahead = 4)
ts.plot(philly.year.ts.post, philly.year.sarima.predict$pred, lty = c(1,3), col=c("red","blue"))
sum((abs(philly.year.ts.post - philly.year.sarima.predict$pred))/(philly.year.ts.post))/
  length(philly.year.ts.post)*100 # 22.60865  
acf(resid(pi.best.sarima))
