###'Vivek Kumar Gupta : Analysis of the cpg dataset included with astsa package on timeseries analysis
###'
###'One of the remarkable technological developments in the computer industry
###'has been the ability to store information densely on a hard drive. In addition, the
###'cost of storage has steadily declined causing problems of too much data as
###'opposed to big data. The data set for this assignment is cpg, which consists of the
###'median annual retail price per GB of hard drives, say ct, taken from a sample of
###'manufacturers from 1980 to 2008.
###'
###

###Acquire the data 
cpg 

### Plot the data and describe what you see. 

ts.plot(cpg , col = 4 , ylab = "Median annual price, Hard Drives" , lwd =2 , main = "Median Price/Year")

###' Notes 
###' Variance in the median price is large across the yearly horizon between 1980 to 2008
###' The Distribution of the data looks exponential or from one of its family such as Weibul and is right skewed.
###'  We will look at the transformation to perhaps normality. 
###

lcpg = log(cpg)

ts.plot(lcpg , col = 4 , ylab = "Median annual price, Hard Drives" , main = "Log Median Price/Year" ,lwd =2)

###' Converting to a fram 
###

cpg.df = data.frame(cpg = as.matrix(cpg) , time = time(cpg) , lcpg = log(as.matrix(cpg)))

plot(cpg.df , pch = "*" , col = 4 , cex = 1.5)

library(mgcv)

fit.gam = gam(lcpg ~ s(time , k = 8 , bs = "cr") , data = cpg.df)
fitted(fit.gam)

fit.lm = lm(lcpg ~ time  , data = cpg.df)

### Now we will check for the estimate order of the assumed AR(p) model. We 
### are placign a restriction that the model must be AR(p) model and not 
### ARM(p,q) or ARIMA model. 
### More generalized case to be dealt as the machinery is developed in the class 
PACF = ARMAacf(ar=c(0,cpg.ar.yw$ar), ma=0, 24, pacf=TRUE)
tsplot( PACF , type = "h")

fit.lm2 = lm(lcpg ~  lcpg1 + lcpg2 + time  , data = cpg.new)

dev.off()
plot(as.vector(cpg.df$time) , cpg.df$lcpg , pch = "*" , type = "p" , col = 4 , lwd = 2 , 
     cex = 2 , main = "Log Median Price/Year" , 
     ylab = "log Median price" , xlab = "Time")
lines(as.vector(cpg.df$time) , fitted(fit.lm) , col = 2 , cex = 1.4  , lwd = 2 , lty = 2 )
lines(as.vector(cpg.df$time) , fitted(fit.gam) , col = 5 , cex = 1.4 , lwd = 2 , lty = 2 )
lines(cpg.new$time,fitted(fit.lm2) , col = 6 , cex = 1.4 , lwd = 2 , lty = 2 )
legend("topright" , legend = c("Data" , "Linear" , "GAM","AR(2)") , 
       text.col = c(4,2,5,6))

dev.off()


## Using large sample theory of PACF 
cutoff = qnorm(1 - .05/2 , 0 , 1 / length(cpg))

abline(h = cutoff , lty = 2, col = 4 )
legend("topright" , "Asymptotic significance of PACF" , text.col = 4)

lines(cpg.new$time,fitted(fit.lm2) , col = 6 , cex = 1.4 , lwd = 2 , lty = 2 )



par(mfrow = c(2,2))
plot(fit.lm)
plot(fit.lm2)
acf(residuals(fit.lm2) , 20)
dev.off()
acf()

acf(residuals(fit.lm) , lag = 50 , main = "Residals Auto Correlation - Linear fit")
acf(residuals(fit.gam) , lag = 50 , main = "Residals Auto Correlation - GAM fit")
acf(residuals(fit.lm2) , lag = 50 , main = "Residals Auto Correlation - Linear fit")
ARMAacf()

cpg.ar.yw = ar.yw(lcpg , order = 2)
cpg.ar.yw$ar



cpg.new = data.frame(ts.intersect(lcpg , lcpg1 = lag(lcpg , -1) , lcpg2 = lag(lcpg , -2) , time = time(lcpg)))



plot(cpg.new$time , fitted(fit.lm2))
head(cpg.new)

lag.plot(lcpg , 20)
