install.packages("astsa")
library("astsa")

acf2(rec)
##can be modelled by AR(2)

#mle estimates of ar(2)

ar_2.model = sarima(rec , 2 , 0 ,0, details = TRUE)

names(ar_2.model)

ar_2.model$fit
ar_2.model$ttable

##forecasts 

forecast = sarima.for(rec , 24 , 2 , 0 , 0 , plot.all = TRUE)
names(forecast)

forecast$pred
forecast$se

?sarima


ARMAtoMA(ar = c(1.3512 , -.4612) , ma = 0 , 20)
