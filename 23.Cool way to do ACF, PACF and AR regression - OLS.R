library(astsa)
acf2(rec , 48) ##this gives acf and the pacf directly 
regr = ar.ols(rec , order = 2 , demean = FALSE , intercept = TRUE)
regr

par(mfrow = c(2,2))
plot(regr)
