##' ARIMA model building using US GNP - Gross National Product data in mind , 223 obs from 1947 to 2002. 
##' We will use general techniques of 
##' 1. Plotting 
##' 1a. If needed transform using Box-Cox power transform techniques.
##' 2. If needed, differencing if at all justified. Note, differencing more than needed induces 
##' unneccsary dependency and overly complicate the matter 
##' 3. Use ACF and PACF functions to figure out th elist of appropriate models. 
##' 4. Estimate parameters of ARIMA(p , d , q) model 
##' 5. Conclude if the models from the list are same or not 
##' 6. Validate model residuals. 
##' 
##

library(astsa)

tsplot(varve , col = 4 , lwd = 2)
acf(gnp)
## acf does not tails down fast to 0 , indicating need of differencing in the data 
## We wil check if we can stabilize the model by log transforms and modelling for growth rate 
## after that
## Variance in the data looks ok , letes model for the growht.

lvarve = log(varve)
dlvarve = diff(lvarve)
tsplot(dlvarve)
acf(dlvarve)

## reduced ACF giving an indication of MA(2) model but there is a possiblity of AR(1) as well as shown below

## 
acf2(dlvarve)

## two competing models ARIMA(0 , 1 , 2) and ARIMA( 1 , 1, 0) on lgnp while 
## ARIMA(0 , 0 , 2) or ARIMA(1 , 0 , 0) on dlgnp data 

ar_1.dlgnp = sarima(dlgnp , 1 , 0 , 0 )
## plots included in the model statement 
ma_2.dlgnp = sarima(dlgnp , 0 , 0 , 2 )

## both the model from the output really looks the same. 

ar_1.dlgnp$fit
ar_1.dlgnp$ttable

ma_2.dlgnp$fit
ma_2.dlgnp$ttable


resid(ar_1.dlgnp)

sarima.for(dlgnp)
