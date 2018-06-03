##' Mortality data 
##' Shumway et al. , 1988 , extracted series. 
##' Studies possible effects of temp and particulates in LA county 

dev.off()
par(mfromw=c(2,1))
library(astsa)

tsplot(cmort , main = "Cardiovascular mortality")
## decreasing trend 
##  cyclical patterns 
## Variance, not so much of an issue with time but it isnt perfect. 
## smotthening might help , yet to consider 


tsplot(tempr , main = "Temperature")
##  cyclical patterns 
## Variance, not so much of an issue with time but it isnt perfect. 
## smotthening might help , yet to consider 

tsplot(part , main = "Particulates")
##  cyclical patterns 
## Variance, not so much of an issue with time but it isnt perfect. 
## smotthening might help , yet to consider 

ts.plot(cmort , tempr , part , col = 1:3)

mort = data.frame(mortality = as.matrix(cmort) , temp = as.matrix(tempr) , part = as.matrix(part) , time = time(cmort))

head(mort)
##lets take a look at the relationship between these 
plot(mort[, 1:3])
plot(mort[, 1:4])
## seems definite a quadratice trend between mort and temp 
## linear between mort and part. 
## note we should use time as a caovariate too 
## relationships are strong. 
## Note 
dev.off()
par(mfrow=c(2,2))

fit1 = lm(mortality ~ temp + part + time , data = mort)
summary(fit1)
###Call:
###lm(formula = mortality ~ temp + part + time, data = mort)
###
###Residuals:
###     Min       1Q   Median       3Q      Max 
###-19.3902  -4.6295  -0.7917   3.8028  27.5423 
###
###Coefficients:
###              Estimate Std. Error t value Pr(>|t|)    
###(Intercept) 2931.13282  211.32125   13.87   <2e-16 ***
###temp          -0.45219    0.03343  -13.53   <2e-16 ***
###part           0.26811    0.01993   13.46   <2e-16 ***
###time          -1.42873    0.10703  -13.35   <2e-16 ***
###---
###Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
###
###Residual standard error: 6.772 on 504 degrees of freedom
###Multiple R-squared:  0.544,	Adjusted R-squared:  0.5413 
###F-statistic: 200.4 on 3 and 504 DF,  p-value: < 2.2e-16

plot(fit1)
## not a bad model , normality seesm a bit of an issue. Resids and fitted. 

mort$centertemp = scale(mort$temp , center = TRUE , scale = FALSE)

fit2 = lm(mortality ~ centertemp + I(centertemp^2) + part + time , data = mort)
summary(fit2)

###Call:
###lm(formula = mortality ~ centertemp + I(centertemp^2) + part + 
###    time, data = mort)
###
###Residuals:
###     Min       1Q   Median       3Q      Max 
###-19.0760  -4.2153  -0.4878   3.7435  29.2448 
###
###Coefficients:
###                  Estimate Std. Error t value Pr(>|t|)    
###(Intercept)      2.831e+03  1.996e+02   14.19  < 2e-16 ***
###centertemp      -4.725e-01  3.162e-02  -14.94  < 2e-16 ***
###I(centertemp^2)  2.259e-02  2.827e-03    7.99 9.26e-15 ***
###part             2.554e-01  1.886e-02   13.54  < 2e-16 ***
###time            -1.396e+00  1.010e-01  -13.82  < 2e-16 ***
###---
###Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
###
###Residual standard error: 6.385 on 503 degrees of freedom
###Multiple R-squared:  0.5954,	Adjusted R-squared:  0.5922 
###F-statistic:   185 on 4 and 503 DF,  p-value: < 2.2e-16

plot(fit2)
##still looks acceptable 

fit3 = lm(mortality ~ centertemp + I(centertemp^2) + time , data = mort)
plot(fit3)

anova(fit2 , fit3 , test = "F")

plot(residuals(fit1))
acf(residuals(fit1))
## We will look into the serially correlated errors later. 

plot(residuals(fit2))
acf(residuals(fit2))

plot(residuals(fit3))
acf(residuals(fit3))

## Note: We can do parameter selectiosn to obtain more parsimonious mode here. 
## It turns out that fit 2 works out best. 