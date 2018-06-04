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
##bad fit , so we arent capturing much with this model.. 

acf(residuals(fit))
##lets plot the rec vs soi data again with the smooth fit to check on the linearity 

lag2.plot(soi , rec , 10 )
## we see the non linear relationship here fo lag(soi , -6) . Thus consider usign GAM or 
## fitting using a dummy variable for soi > 0 

dummy = ifelse(soi < 0 , 0 , 1)

fish = ts.intersect(rec, soi6 = lag(soi , -6) , dum = lag(dummy , -6))

fit2 = lm(rec ~ soi6*as.factor(dum) , data = fish)

summary(fit2)


dev.off()
par(mfrow=c(2,2))

plot(fit2)

acf(residuals(fit2))

