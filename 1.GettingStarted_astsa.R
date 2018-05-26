install.packages("astsa")
library(astsa)
jj
#Johnson and Johnson data

tsplot(jj , type = "o" , ylab = "Quaterly Earnings per share")
?diff
tsplot(diff(jj)) #removing trend 
tsplot((diff(log(jj)))) #controlling variability 

diff
class(jj)
?ts

#Getting a ts object insitialized and set 
ts(1:10, frequency = 4, start = c(1959, 2)) # 2nd Quarter of 1959

#Global temp data 

tsplot(globtemp, type="o", ylab="Global Temperature Deviations")
abline(lm(as.matrix(globtemp) ~ time(globtemp)) , lty = 2 , col = 4) # note how time and values are extracted from a ts object
legend("topleft",
       c("Time series" , "Mean function"), 
       text.col = c(1 , 4))

#Dow Jones Returns

#Some issue in tsplot 
library(xts)
djiar = diff(log(djia$Close))[-1] # approximate returns
tsplot(djiar, main="DJIA Returns", xlab='', margins=.5)

##White noise with a gaussian distr 
par(mfrow = c(2,1))
norm = rnorm(100)
plot(norm, type = "o" , xlab = "Index of drawing / Time" , ylab = "White Noise (Std. Normal)" )
v = filter(norm , sides = 2 , rep(1/3 , 3)) #filter because linear comb of values of ts look eg. 1.7
tsplot(v , ylab = "Inducing correlation , Moving Average(3) model")

#Above model just induces correlation on top of white noise to model for 
#somethign like DJIA data 
##More complicated AR models allow for seasonal behavious as well. 

dev.off()
##Random walk model with a drift :

set.seed(154) # so you can reproduce the results

w = rnorm(200);
x = cumsum(w) 

wd = w +.2
xd = cumsum(wd)

tsplot(xd, ylim=c(-5,55), main="random walk", ylab='')
abline(a=0, b=.2, lty=2) # drift
lines(x, col=4)
abline(h=0, col=4, lty=2)
