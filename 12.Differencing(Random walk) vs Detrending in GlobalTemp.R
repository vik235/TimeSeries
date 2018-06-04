library(astsa)

tsplot(globtemp)
tsplot(diff(globtemp) , main = "Differencing first order")
acf(diff(globtemp))
##much better 

tsplot(globtemp)
fit1 = lm(globtemp ~ time(globtemp))
abline(fit1 , lty = 2 , lwd = 2, col = 4)
##doestn look very good model 

acf(residuals(fit1))
##evident here as well

plot(residuals(fit1) , type = "o" , main = "Detrended")
plot(diff(globtemp) , type = "o" , main = "Differencing first order")

##estimate of the drift in random walk
mean(diff(globtemp))
