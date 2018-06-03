## SOI and fish Recruitment 

dev.off()
par(mfrow=c(2,2))
    
library(astsa)
tsplot(soi , main = "Souther Oscillation Index")
tsplot(rec , main = "Recruitment of the fish")

ts.plot(soi , rec , col = 1:2 )
##cant do together, scales differ. 

acf(soi)
acf(rec)
ccf(soi, rec)
## note a soi at t-6 is related to rec at time t. 

plot(rec)
# no relaitonship with time

plot(rec, lag(soi , -6))

##definitive relationship

soir = data.frame(soi = as.matrix(soi) , rec = as.matrix(rec) , time = time(soi))
plot(soir)


## we note that we have to align the series based on the relationship. 

fish = ts.intersect(rec, soi6 = lag(soi , -6))

fit = lm(rec ~ soi6 , data = fish)
summary(fit)

plot(fit)
##bad 

acf(residuals(fit))
