## Simulation of Prediction of y with x via examinign CCF

library(astsa)

x = rnorm(100)
y = lag(x , -5) + rnorm(100)

dev.off()
ccf(y , x, ylab = "CCF" , type = "covariance")
ccf(y , x, ylab = "CCorrelation" , type = "correlation")
abline( v = 0 , lty = 2)
dev.off()
lm.fit = lm(y ~ x)
par(mfrow = c(2,2))
tsplot(x)
tsplot(y)
tsplot(fitted(lm.fit))
plot(residuals(lm.fit))
