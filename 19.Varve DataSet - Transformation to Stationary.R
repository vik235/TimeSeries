library(astsa)
tsplot(varve)
varve
length(varve)

lvarve = log(varve)

n = length(varve)
v1 = var(varve[1:floor(n/2)])
v2 = var(varve[floor(n/2):n])

v3 = var(log(varve[1:floor(n/2)]))
v4 = var(log(varve[floor(n/2):n]))

dev.off()
par(mfrow=c(2,2))


plot(density(varve[1:floor(n/2)]) , main = "Density among two groups , varve" , 
     pch = "*" , cex = 2 , col = 3 , lwd = 2 )

lines(density(varve[floor(n/2):n]) , 
     pch = "*" , cex = 2 , col = 4 , lwd = 2)

legend("topright" , legend = c(paste("var = " ,round(v1 , 2)),  
                               paste("var = " ,round(v2 , 2))) , text.col = c(3,4))

plot(density(log(varve[1:floor(n/2)])), main = "Density among two groups, log(varve)" , 
     pch = "*" , cex = 2 , col = 5 , lwd = 2)

lines(density(log(varve[floor(n/2):n])),  
     pch = "*" , cex = 2 , col = 6 , lwd = 2)
legend("topright" , legend = c(paste("var = " ,round(v3 , 2)),paste("var = " ,round(v4 , 2))) , text.col = c(5,6))

lines(density(log(varve)),  
      pch = "*" , cex = 2 , col = 8 , lwd = 2)
shapiro.test(lvarve)
shapiro.test(varve)
par(mfrow = c(2,1))
qqnorm(lvarve , col = 4 , main = "Reference distr. plot for log(varve)")
qqnorm(varve , col = 3, main = "Reference distr. plot for varve")

qqline(varve)

plot(lvarve , col = 3 , main = "Time series plot of Varve")
lines(ksmooth(time(lvarve) , lvarve , kernel = "normal" , bandwidth = 1) , lwd = 2 , lty =2 , col = 4)
lines(lowess(lvarve) , lwd = 2 , lty =2 , col = 4)

plot(varve , col = 3 , main = "Time series plot of log(Varve)")
lines(lowess(varve) , lwd = 2 , lty =2 , col = 4)
dev.off()


acf(lvarve , main = "Sample ACF , log(Varve)" , col = 4)

newVarve = ts.intersect(lvarve , lag(lvarve , -1))

str(newVarve)

plot(diff(lvarve) , col = 4 , main = "Time series plot of first order differencing, log(Varve)")
#lines(ksmooth(time(diff(lvarve)) , diff(lvarve) , kernel = "normal" , bandwidth = 3) , lwd = 2 , lty =2 , col = 2)
lines(lowess(diff(lvarve)) , lwd = 2 , lty =2 , col = 2)

acf(diff(lvarve) , main = "Sample ACF , first order differencing, log(Varve)" , col = 4)
