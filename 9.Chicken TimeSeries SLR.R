## Modelling for chicken price per pound in US from 2001 to 2016 , 180 periods information. 

## Predicting xt the chicek series with time only via conventional linear regression (OLS). 
## We note soem issues with acf on the residuals via this process. 
## Inference is same as that of linear regression. 
## Validity of the model is questionable 


## Assumptions at this time , time are known and fixed. 
## wt is iid wn(0 , sigma_w ^2 ). 
## Both of these will be relaxed later on. 


library(astsa)

length(chicken)
dev.off()
par(mfrow = c(2,2))
plot(chicken)
time(chicken)
fit.lm.chicken = lm(chicken ~ time(chicken))
abline(lm(chicken ~ time(chicken)) , col = 4 , lty = 2, lwd = 2)
dev.off()
par(mfrow=c(2,2))
plot(fit.lm.chicken)
dev.off()
plot(residuals(fit.lm.chicken) , type = "o" , col = 4)

summary(fit.lm.chicken)
aov(fit.lm.chicken)
acf(residuals(fit.lm.chicken))
##does not look so good , cyclic patterns in resids , acf on resids showing dependence and not random