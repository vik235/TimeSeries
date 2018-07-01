##' Simluating Seasonal ARIMA - pure Time series . 
##' SARIMA(0 , 0 , 0, 12 , 0 ,0). Simulate it and plot is theoritical ACF and PACF.
##' An note its similarity with regular SARIMA 
##

library(astsa)
set.seed(666)

phi = c(rep(0, 11) , .9)

sAR = arima.sim(list(order = c(12, 0 ,0 ) , ar = phi) , n = 37)
sAR = ts(sAR , frequency = 12)

dev.off()
par(mfrow = c(2,2))
plot(sAR , main = "Seasonal AR(1) , s = 12")

Months = c("J" , "F" , "M" , "A" , "M" , "J" , "J" , "A" , "S" , "O", "N" , "D" )

points(sAR , pch = Months , cex = 1.25 , font = 4 , col = 1:4)

abline(v = 1:4 , lty = 2 , col = gray(.6))

ACF = ARMAacf(ar = phi , ma = 0 , 100)
PACF = ARMAacf(ar = phi , ma = 0 , 100 , pacf = TRUE)

plot(ACF , type = "h")

plot(PACF , type = "h")
