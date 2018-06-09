
for (i in 1:2) {
  x1 = rnorm(100)
  y1 = rnorm(100)
  rw1 = cumsum(x1)
  rw2 = cumsum(y1)
  x = ts(rw1)
  y = ts(rw2)
}

par(mfrow=c(1,2))
plot( x  , type = "o" , main = "Timeseries x" )
plot( y  , type = "o" , main = "Timeseries y" )

dev.off()
plot(rw1 , rw2 , xlab = "Series, x" , ylab = "Series, y" , pch = "*" , col = 4 , cex = 1.5 , main = "Lag 0 plot")
lag2.plot(x , y , 10)

fit.lm = lm(y ~ x)
summary(fit.lm)
par(mfrow = c(2,2))
plot(fit.lm)
names(fit.lm)
fit.lm$qr

summary(fit.lm)$coef[,4][2]

pvalue = rep(0 , 1000)

for (j in 1:1000) {
  for (i in 1:2) {
    x1 = rnorm(100)
    y1 = rnorm(100)
    rw1 = cumsum(x1)
    rw2 = cumsum(y1)
    x = ts(rw1)
    y = ts(rw2)
  }
  
  fit.lm = lm(y ~ x)
  pvalue[j] = summary(fit.lm)$coef[,4][2]
}

mean(pvalue < 0.05)

par(mfrow = c(2,2))

plot(fit.lm)
