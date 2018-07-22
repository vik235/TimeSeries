install.packages("tseries")
library("tseries")
library(astsa)
library(fpp2)
library(forecast)
library(mgcv)
library(e1071) 
setwd("/626/Project/Data")
wells = read.csv(file = "US_OG_BAKKEN Production Time Series.CSV")
options(scipen=999)

head(wells)
colnames(wells) = c("EntityID" , "API_UWI" , "API_UWI_List", "ProdDate" , "Liquid" , "Gas" , "Water" , "WellCount" , "Days" , "DailyAvgOil" , "DailyAvgGas" , "DailyAvgWater")

head(wells)



length(unique(wells$API_UWI))
#60 wells 

unique(wells$API_UWI)[1:12]
unique(wells$API_UWI)[13:24]

##Gather the particular well data 
well_01_vk = wells[ wells$API_UWI == "33061005340000" , ]

##Convert the Monthly Averages to a timeseries object 
AvgGas_01_ts = ts(well_01_vk$DailyAvgGas , frequency = 12 , start = c(2007 , 8))
AvgOil_01_ts = ts(well_01_vk$DailyAvgOil , frequency = 12 , start = c(2007 , 8))
AvgWater_01_ts = ts(well_01_vk$DailyAvgWater[well_01_vk$DailyAvgWater != 0] , frequency = 12 , start = c(2007 , 8))

summary(AvgGas_01_ts)
summary(AvgOil_01_ts)
summary(AvgWater_01_ts)
kurtosis(AvgWater_01_ts)
skewness(AvgWater_01_ts)
##Plot the time series 
ts.plot(AvgGas_01_ts , AvgOil_01_ts , AvgWater_01_ts , 
        gpars = list(xlab = "Time" , ylab = "Quantity" , col = 3:5 , lwd = 2, 
                     main = "Time series plot for Well # 33061005340000 "
                     ) 
        )

legend("topright" , legend = c("Avg. Gas" , "Avg. Oil" , "Avg. Water") , text.col = 3:5)


par(mfrow = c(1,1))
plot.ts(cbind("Avg. Gas "=AvgGas_01_ts , "Avg. Oil "=AvgOil_01_ts 
              , "Avg. Water "=AvgWater_01_ts ) , yax.flip = TRUE , col = 4 
        , main = "Time series plot for Well # 33061005340000")


par(mfrow = c(1,1))
plot(density(AvgGas_01_ts) , main = "Density plot for Avg. Gas" , col = 4 , lwd = 2 )
plot(density(AvgOil_01_ts) , main = "Density plot for Avg. Oil" , col = 4 , lwd = 2)
plot(density(AvgWater_01_ts) , main = "Density plot for Avg. Water" , col = 4 , lwd = 2)


##Getting the power transfrom for the series
thetaGas = BoxCox.lambda(AvgGas_01_ts , method = "l")
thetaOil = BoxCox.lambda(AvgOil_01_ts , method = "l")
thetaWater = BoxCox.lambda(AvgWater_01_ts[AvgWater_01_ts != 0] , method = "l")

BcAvgGas_01_ts = AvgGas_01_ts^thetaGas
BcAvgOil_01_ts = AvgOil_01_ts^thetaOil
BcAvgWater_01_ts = AvgWater_01_ts[AvgWater_01_ts != 0]^thetaWater

par(mfrow = c(3,1))
tsplot(BcAvgGas_01_ts , col = 3 , lwd = 2 , ylab = "Avg. Gas Bc" )
tsplot(BcAvgOil_01_ts , col = 4 , lwd = 2, ylab = "Avg. Oil Bc" )
tsplot(BcAvgWater_01_ts , col = 5 , lwd = 2, ylab = "Avg. Water Bc" )


##Do the Gam fit on tranfsomred data 
fitGas_01 = gam(BcAvgGas_01_ts ~ s(time(BcAvgGas_01_ts) , bs = "cr" , k = 30))
fitOil_01 = gam(BcAvgOil_01_ts ~ s(time(BcAvgOil_01_ts) , bs = "cr" , k = 30))
AvgWater_01_ts =AvgWater_01_ts [AvgWater_01_ts != 0]
fitWater_01 = gam(BcAvgWater_01_ts ~ s(time(BcAvgWater_01_ts) , bs = "cr" , k = 30))
(fitWater_01$coefficients)
#plot the fits
par(mfrow=c(3,1))

ts.plot(BcAvgGas_01_ts , col = 2 , lwd = 2 , ylab = "Avg. Gas")
points(as.vector(time(BcAvgGas_01_ts)) , fitted(fitGas_01) , pch = "*" , cex = 1.1 , col = 4)
legend("topleft" , legend = c("Avg. Gas Time series Bc" , "Avg. Gas GAM point Est." ) , text.col = c(2,4))

ts.plot(BcAvgOil_01_ts , col = 2 , lwd = 2 , ylab = "Avg. Oil")
points(as.vector(time(BcAvgOil_01_ts)) , fitted(fitOil_01) , pch = "*" , cex = 1.1 , col = 4)
legend("topleft" , legend = c("Avg. Oil Time series Bc" , "Avg. Oil GAM point Est." ) , text.col = c(2,4))

ts.plot(BcAvgWater_01_ts , col = 2 , lwd = 2 , ylab = "Avg. Water")
points(as.vector(time(BcAvgWater_01_ts)) , fitted(fitWater_01) , pch = "*" , cex = 1.1 , col = 4)
legend("topleft" , legend = c("Avg. Water Time series Bc" , "Avg. Water GAM point Est." ) , text.col = c(2,4))

##Do the Gam fit on un transformed data 
fitGas_01_gam2 = gam(AvgGas_01_ts ~ s(time(AvgGas_01_ts) , bs = "cr" , k = 30))
fitOil_01_gam2 = gam(AvgOil_01_ts ~ s(time(AvgOil_01_ts) , bs = "cr" , k = 30))
AvgWater_01_ts =AvgWater_01_ts [AvgWater_01_ts != 0]
fitWater_01_gam2 = gam(AvgWater_01_ts ~ s(time(AvgWater_01_ts) , bs = "cr" , k = 30))

##residuals
par(mfrow=c(3,2))
plot(as.vector(time(AvgGas_01_ts)) , abs(residuals(fitGas_01_gam2)) , pch = "*" , cex = 2 , col = 4
     , main = "|Residual| Analysis by Time" , xlab = "Time" , ylab = "Avg. Gas GAM |Residuals|" )
acf(residuals(fitGas_01_gam2))

plot(as.vector(time(AvgOil_01_ts)) , abs(residuals(fitOil_01_gam2)) , pch = "*" , cex = 2 , col = 4
     , main = "|Residual| Analysis by Time" , xlab = "Time" , ylab = "Avg. Oil GAM |Residuals|" )
acf(residuals(fitOil_01_gam2))

plot(as.vector(time(AvgWater_01_ts)) , abs(residuals(fitWater_01_gam2)) , pch = "*" , cex = 2 , col = 4
     , main = "|Residual| Analysis by Time" , xlab = "Time" , ylab = "Avg. Water GAM |Residuals|" )
acf(residuals(fitWater_01_gam2))
q = rep(0 , 10)

for (i in 1:10) {
  f = Box.test(residuals(fitGas_01_gam2) , type = "Ljung-Box" , lag = i*10)
  q[i]                = f$p.value
       
}

q_oil = rep(0 , 10)

for (i in 1:10) {
  f = Box.test(residuals(fitOil_01_gam2) , type = "Ljung-Box" , lag = i*10)
  q_oil[i]                = f$p.value
  
}

q_water = rep(0 , 10)

for (i in 1:10) {
  f = Box.test(residuals(fitWater_01_gam2) , type = "Ljung-Box" , lag = i*10)
  q_water[i]                = f$p.value
  
}

par(mfrow=c(1,1))
plot(seq(10, 100 , 10) , q, xlab = "lags" , ylab = "Ljung-Box p-value" , col = 2, pch = "*" , cex = 3)
points(seq(10, 100 , 10) ,q_oil, col = 3, pch = "*" , cex = 3)
points(seq(10, 100 , 10) ,q_water, col = 4, pch = "*" , cex = 3)
abline(h=0.05 , lty = 2)
legend("topleft" , legend = c("Avg. Gas  " , "Avg. Oil " , "Avg. Water " ) , text.col = 2:4)

Box.test(residuals(fitGas_01_gam2) , type = "Ljung-Box" , lag = 50)
Box.test(residuals(fitGas_01_gam2) , type = "Ljung-Box" , lag = 100)

#plot the fits
par(mfrow=c(3,1))

ts.plot(AvgGas_01_ts , col = 2 , lwd = 3 , ylab = "Avg. Gas")
points(as.vector(time(AvgGas_01_ts)) , fitted(fitGas_01_gam2) , pch = "*" , cex = 2 , col = 4)
legend("topleft" , legend = c("Avg. Gas Time series " , "Avg. Gas GAM point Est." ) , text.col = c(2,4))

ts.plot(AvgOil_01_ts , col = 2 , lwd = 3 , ylab = "Avg. Oil")
points(as.vector(time(AvgOil_01_ts)) , fitted(fitOil_01_gam2) , pch = "*" , cex = 2 , col = 4)
legend("topleft" , legend = c("Avg. Oil Time series " , "Avg. Oil GAM point Est." ) , text.col = c(2,4))

ts.plot(AvgWater_01_ts , col = 2 , lwd = 3 , ylab = "Avg. Water")
points(as.vector(time(AvgWater_01_ts)) , fitted(fitWater_01_gam2) , pch = "*" , cex = 2 , col = 4)
legend("topleft" , legend = c("Avg. Water Time series " , "Avg. Water GAM point Est." ) , text.col = c(2,4))

summary(fitGas_01_gam2)
summary(fitOil_01_gam2)
summary(fitWater_01_gam2)
par(mfrow=c(2,2))
gam.check(fitGas_01_gam2)
gam.check(fitOil_01_gam2)
gam.check(fitWater_01_gam2)

###Log transforms 

lAvgGas_01_ts = log(AvgGas_01_ts)
lAvgOil_01_ts = log(AvgOil_01_ts)
lAvgWater_01_ts = log(AvgWater_01_ts)

par(mfrow = c(3,1))
plot(density(lAvgGas_01_ts) , main = "Density plot for Log Avg. Gas" , col = 4 , lwd = 2 )
plot(density(lAvgOil_01_ts) , main = "Density plot for Log Avg. Oil" , col = 4 , lwd = 2)
plot(density(lAvgWater_01_ts) , main = "Density plot for Log Avg. Water" , col = 4 , lwd = 2)


dlAvgGas_01_ts = diff(lAvgGas_01_ts)
dlAvgOil_01_ts= diff(lAvgOil_01_ts)
dlAvgWater_01_ts= diff(lAvgWater_01_ts)

par(mfrow = c(2,1))
ts.plot(lAvgGas_01_ts , lAvgOil_01_ts , lAvgWater_01_ts , 
        gpars = list(xlab = "Time" , ylab = "Log Quantity" , col = 3:5 , lwd = 2, 
                     main = "Time series plot for Well # 33061005340000 "
        ) 
)

legend("topright" , legend = c("Log Avg. Gas" , "Log Avg. Oil" , "Log Avg. Water") , text.col = 3:5)

ts.plot(dlAvgGas_01_ts , dlAvgOil_01_ts , dlAvgWater_01_ts , 
        gpars = list(xlab = "Time" , ylab = "Log Quantity, 1st Order Diff" , col = 3:5 , lwd = 2, 
                     main = "Time series plot for Well # 33061005340000 1st Order Diff"
        ) 
)

legend("topright" , legend = c("Log Avg. Gas, d = 1" , "Log Avg. Oil, d = 1" , "Log Avg. Water, d = 1") , text.col = 3:5)

acf2(dlAvgWater_01_ts , main = "ACF and ACF for differenced and transformed series - Avg. Water")
#S = 12 , 0 , Q = 2 , 0 , P = 0 , D = 0 , d = 1 , q = 1 ,4 , p = 2
s1_gas01 = sarima(lAvgGas_01_ts , 2 , 1 , 0 , 0 , 0 , 2 , 12 )
s2_gas01 = sarima(lAvgGas_01_ts , 0 , 1 , 1 , 0 , 0 , 2 , 12)
s3_gas01 = sarima(lAvgGas_01_ts , 0 , 1 , 4 , 0 , 0 , 2 , 12)
s4_gas01 = sarima(lAvgGas_01_ts , 0 , 1 , 4 , 0 , 0 , 0 , 0)
s5_gas01 = sarima(lAvgGas_01_ts , 0 , 1 , 1 , 0 , 0 , 0 , 0)
s6_gas01 = sarima(lAvgGas_01_ts , 2 , 1 , 0 , 0 , 0 , 0 , 0)
s7 = auto.arima(lAvgGas_01_ts)
summary(s7)
checkresiduals(s7)
s1_gas01$BIC
s2_gas01$BIC
s3_gas01$BIC

s1_gas01$AIC
s2_gas01$AIC
s3_gas01$AIC

acf2(dlAvgOil_01_ts)
s1_Oil01 = sarima(dlAvgOil_01_ts , 0 , 1 , 1 , 0 , 0 , 0 , 0)

acf2(dlAvgWater_01_ts)
#S=0 , P = Q = 0 , p = 1 , 2 , q = 1 , d = 1

s1_water01 = sarima(lAvgWater_01_ts , 1 , 1 , 0 , 0 , 0 , 0 , 0)
s2_water01 = sarima(lAvgWater_01_ts , 2 , 1 , 0 , 0 , 0 , 0 , 0)
s3_water01 = sarima(lAvgWater_01_ts , 0 , 1 , 1 , 0 , 0 , 0 , 0)


write.csv(well_01_vk, "33061005340000.csv")

###Phase 3 data and modelling for GARCH models and incorporate time series crsoovalidation 
###along with penalized likelihood methods. 

##lets read the data again 
setwd("F:/OneDrive/Learning/DataScience/Statistics Texas A&M University/626/Project/Well33061005340000/Well01")

well = read.csv("phaseIII.csv")

head(well)

colnames(well) = c("time" , "r")
well_ts = ts(well$r , frequency = 12 , start = c(2007 , 9))
par(mfrow = c(2,2))
acf(well_ts)
ts.plot(well_ts , col = 4 , type = "o" , cex = 1.2)
ts.plot(diff(well_ts) , col = 4 , lwd = 2 )



##Unit root test to check if it is AR versus randow walk. 
## Contradictory results by PP and ADF tests and hence, siding with ARIMA
adf.test(well_ts , k = 10)

##Augmented Dickey-Fuller Test

##data:  well_ts
##Dickey-Fuller = -8.0913, Lag order = 0, p-value = 0.01
##alternative hypothesis: stationary


adf.test(well_ts)
##Augmented Dickey-Fuller Test

###data:  well_ts
##Dickey-Fuller = -8.1749, Lag order = 4, p-value = 0.01
##alternative hypothesis: stationary

pp.test(well_ts)

##Phillips-Perron Unit Root Test

##data:  well_ts
##Dickey-Fuller Z(alpha) = -11.377, Truncation lag parameter = 4, p-value = 0.4587
##alternative hypothesis: stationary

dr = diff(well_ts)
acf2(well_ts, 10)

fit1 = sarima(well_ts , 1 , 1 , 0 , 0 , 0 , 0 , 0)
fit2 = sarima(well_ts , 1 , 1 , 1 , 0 , 0 , 0 , 0)
fit3 = sarima(well_ts , 0 , 1 , 1 , 0 , 0 , 0 , 0)

fit.arima1 = arima(well_ts , order = c(1 , 1, 0) ,
                    seasonal = list(order = c(0 , 0 , 0 ), period = NA ),
                                    method = 'ML')
fit.arima2 = arima(well_ts , order = c(1 , 1, 1) ,
                   seasonal = list(order = c(0 , 0 , 0 ), period = NA ),
                   method = 'ML')
fit.arima3 = arima(well_ts , order = c(0 , 1, 1) ,
                   seasonal = list(order = c(0 , 0 , 0 ), period = NA ),
                   method = 'ML')
fit.arima4 = arima(well_ts , order = c(1 , 0 , 1) ,
                   seasonal = list(order = c(0 , 0 , 0 ), period = NA ),
                   method = 'ML')
fit.arima5 = arima(well_ts , order = c(1 , 0 , 0) ,
                   seasonal = list(order = c(0 , 0 , 0 ), period = NA ),
                   method = 'ML')

summary(fit.arima1) #aic = 914.76

##                    ME     RMSE     MAE       MPE     MAPE     MASE       ACF1
##Training set -1.530629 9.194844 6.38557 -1.931483 13.58954 0.973327 -0.1767277
summary(fit.arima2) ##aic = 899.05

##ME     RMSE      MAE        MPE     MAPE      MASE       ACF1
#Training set -0.1065058 8.539538 5.945723 -0.5348611 12.27239 0.9062828 0.02688141

summary(fit.arima3)## aic = 931.55
##ME     RMSE      MAE      MPE    MAPE     MASE       ACF1
##Training set -2.515725 9.841339 6.574913 -3.01263 13.1876 1.002188 0.06207661

summary(fit.arima4)##aic = 949.04
##ME     RMSE     MAE       MPE     MAPE     MASE       ACF1
##Training set -2.624666 9.877012 6.68271 -3.756898 13.34938 1.018619 0.03396875


summary(fit.arima5)##aic = 975.7
##ME     RMSE      MAE       MPE     MAPE     MASE      ACF1
##Training set -3.633635 11.08433 6.665479 -4.922876 11.86151 1.015992 0.4645431


##Note : fitX in above is same as fit.arimaX, just different wrappers. 

##Below is time series cross validation on the fits/


k <- 60 
n <- length(well_ts)

mae1 <- mae2 <- mae3 <- mae4 <-mae5 <-matrix(NA,n-k,12)

st <- tsp(well_ts)[1]+(k-2)/12

for(i in 1:(n-k))
{
  #i = 1
  xshort <- window(well_ts, end=st + i/12)
  xnext <- window(well_ts, start=st + (i+1)/12, end=st + (i+12)/12)
  fit.arima1 = arima(xshort , order = c(1 , 1, 0) ,
                     seasonal = list(order = c(0 , 0 , 0 ), period = NA ),
                     method = 'ML')
  fit.arima2 = arima(xshort , order = c(1 , 1, 1) ,
                     seasonal = list(order = c(0 , 0 , 0 ), period = NA ),
                     method = 'ML')
  fit.arima3 = arima(xshort , order = c(0 , 1, 1) ,
                     seasonal = list(order = c(0 , 0 , 0 ), period = NA ),
                     method = 'ML')
  fit.arima4 = arima(xshort , order = c(1 , 0 , 1) ,
                     seasonal = list(order = c(0 , 0 , 0 ), period = NA ),
                     method = 'ML')
  fit.arima5 = arima(xshort , order = c(1 , 0 , 0) ,
                     seasonal = list(order = c(0 , 0 , 0 ), period = NA ),
                     method = 'ML')
  summary(fit.arima5)
  fcast1 <- forecast(fit.arima1, h=12)
  fcast2 <- forecast(fit.arima2, h=12)
  fcast3 <- forecast(fit.arima3, h=12)
  fcast4 <- forecast(fit.arima4, h=12)
  fcast5 <- forecast(fit.arima5, h=12)
  mae1[i,1:length(xnext)] <- abs(fcast1[['mean']]-xnext)
  mae2[i,1:length(xnext)] <- abs(fcast2[['mean']]-xnext)
  mae3[i,1:length(xnext)] <- abs(fcast3[['mean']]-xnext)
  mae4[i,1:length(xnext)] <- abs(fcast4[['mean']]-xnext)
  mae5[i,1:length(xnext)] <- abs(fcast5[['mean']]-xnext)
}

plot(1:12, colMeans(mae1,na.rm=TRUE), type="l", col=2, xlab="horizon", ylab="MAE",  lwd = 2
     )
lines(1:12, colMeans(mae2,na.rm=TRUE), type="l",col=3,  lwd = 2)
lines(1:12, colMeans(mae3,na.rm=TRUE), type="l",col=4,  lwd = 2)
lines(1:12, colMeans(mae4,na.rm=TRUE), type="l",col=5,  lwd = 2)
lines(1:12, colMeans(mae5,na.rm=TRUE), type="l",col=6,  lwd = 2)

legend("topleft",legend=c("ARIMA, fit 1","ARIMA, fit 2","ARIMA , fit 3",
                          "ARIMA , fit 4","ARIMA , fit 5"),col=2:6,lty=1)


###Final Forecast using model 3 above 
sarima.for(well_ts , 12 , 0 , 1 , 1 , 0 , 0 , 0 , 0)


