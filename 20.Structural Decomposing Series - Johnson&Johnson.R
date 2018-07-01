x = log(JohnsonJohnson)


tsplot(x)

plot(decompose(x), col = "coral3", lwd =2 )

plot(stl(x , s.window = 'periodic') , col = 4)
plot(stl(x , s.window = 15), col = "dodgerblue" , lwd =2 , main = "Decomposition via loess")

resid = residuals(lm(x ~ time(x)))
acf(resid)

tsplot(resid)

lines(ksm)