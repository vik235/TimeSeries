set.seed(12345)
dev.off() 
par(mfrow = c(3,1))

x1 = rnorm(500 )
x2 = rnorm(50)
x3 = rnorm(1000)

acf_x1 = acf(x1 , 20 , type = "correlation" , plot = FALSE)
acf_x2 = acf(x2 , 20, type = "correlation", plot = FALSE)
acf_x3 = acf(x3 , 20, type = "correlation", plot = FALSE)



plot(acf_x1$acf , pch = "*" , col = 3 , cex = 2 , main = "Covariance plot, correlogram, n = 500" 
     , ylab = "ACF" , xlab = "lag")
points(c(1 , rep(0 , 20)) ,pch = "*" , col = 1 , cex = 2)
abline(h = 0 )
abline(h = c(-1 , 1) * sqrt(1/500) , col = 2 )
legend("topright" , legend = c("Sample" , "Actual") , text.col = c(3,1))

plot(acf_x2$acf , pch = "*" , col = 4 , cex = 2 , main = "Covariance plot, correlogram, n = 50"
     , ylab = "ACF" , xlab = "lag")
points(c(1 , rep(0 , 20)) ,pch = "*" , col = 1 , cex = 2)
abline(h = 0 )
abline(h = c(-1 , 1) * sqrt(1/50) , col = 2 )
legend("topright" , legend = c("Sample" , "Actual") , text.col = c(4,1))

plot(acf_x3$acf , pch = "*" , col = 5 , cex = 2 , main = "Covariance plot, correlogram, n = 1000"
     , ylab = "ACF" , xlab = "lag")
points(c(1 , rep(0 , 20)) ,pch = "*" , col = 1 , cex = 2)
abline(h = 0 )
abline(h = c(-1 , 1) * sqrt(1/1000) , col = 2 )
legend("topright" , legend = c("Sample" , "Actual") , text.col = c(5,1))