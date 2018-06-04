## Smoothing a time series 
## e smooth to detect patterns , seasonal components , trennds in a time series. 

## popular method is to create a a movign avaerage smoother. 
## mt = sigma from -k to k aj * x_t-j such that a_j = a_-j and sigma a_j = 1. 

## this is symmetric movign avergae of the data. 

## lets take a look at some example usign moving average smoother

dev.off()
par(mfrow = c(2,1))
tsplot(soi , main = "Southern Oscillation Index")
## cant make out anything , too much noise in the data 

weights = c (.5 , rep(1,11) , .5)/12
soif = filter(soi , sides = 2 , filter = weights)
tsplot(soif , main = "Southern Oscillation Index - filter")

weights1 = rep(1/12 , 12)
soi1 = filter(soi , sides = 2 , filter = weights1)
tsplot(soif , main = "Southern Oscillation Index - filter")

ts.plot(soi , soif , soi1 , col = 2:4 , main = "Smoothing SOI data")
##We see that the weights do not matter when filtering. 



####Add a kernel smoother , apparently much better smoother than MA 
#tsplot(soi)
lines(ksmooth(time(soi) , soi , kernel = "normal" , bandwidth = 1) , lwd = 2 , lty =2 , col = 5)

####Add a lowess smoother , apparently much better smoother than MA 

lines(lowess(soi) , lwd = 2 , lty =2 , col = 6)

legend("topright" , legend =c( "Moving average smoother", 
                               "Kernel Smoother", 
                               "Lowess Smoother"), 
       text.col = 4:6
       )
