## Simulating Random Walk with a Drift , del = .01 , sigma_w = 1 , n = 500
## WE are plotting 4 rw with drift series, wont set the seed. 
## In the second part we are plotting 4 simulations of linear trend 

## In both cases we are also trying to stationarize the series and note that 
## Unless the trend is linear , detrending with classical regression techiniques i.e. 
## OLS doest work 

dev.off()
par(mfrow = c(2,2))

#set.seed(1234)

##Classical Regression doesnt work
for (i in 1:4) {
  w = rnorm(500)
  rw = cumsum(.01 + w)
  x = ts(rw)
  ts.plot(x ,  col = 'darkgray' , main = paste( "Random Walk with Drift, " , i))
  abline(lm(rw ~ time(rw)) , col ="dodgerblue" , lty = 2 , lwd = 2)
  abline(a = 0 , b = .01 , col ="springgreen" , lty = 2 , lwd = 2)
  legend("topright" , legend = c("Fitted" , "True Mean") , text.col = c("dodgerblue" ,"springgreen"))
}

##Classical Regression works
for (i in 1:4) {
  w = ts(rnorm(500))
  lt = 0.01 *time(w) + w 
  x = ts(lt)
  ts.plot(x ,  col = 'darkgray' , main = paste( "Linear Trend with Noise, " , i))
  abline(lm(x ~ time(x)) , col ="dodgerblue" , lty = 2 , lwd = 2)
  abline(a = 0 , b = .01 , col ="springgreen" , lty = 2 , lwd = 2)
  legend("bottomright" , legend = c("Fitted" , "True Mean") , text.col = c("dodgerblue" ,"springgreen"))
}
