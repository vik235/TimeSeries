dev.off()
par(mfrow = c(3,1))

acf1(soi, 48 , main = "Southern Oscillation Index")

#acf1 removes lag 0 
##note SE lines makes sense only if xt is white noise which isnt the case here 
##so ignore it from analysis. 


acf1(rec, 48 , main = "Fish recruits"  )

ccf(soi , rec , , ylab = "CCF" , type = "covariance")
abline(v=0 , lty = 2)
##note SE lines makes sense only if xt is white noise which isnt the case here 
##so ignore it from analysis. 
##CCF , at h = -6 , the ccf peaks indicating that SOI measured at t - 6 is related to recruitment at time t. 
## sign of CCF is -ve at h = -6 , thus neg relationship. 