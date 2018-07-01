
#Simulating ARMA model specifically AR(2) model x_t = -0.9 x_t-2 + wt
sim_ar2 = arima.sim(model = list(order=c(2,0,0), ar=c(0 , -.9)) , n = 500)
ts.plot(sim_ar2 , col =4 , ylab="AR(2) Process")

z = c(1 , 0 , 0.9)
a = polyroot((z))[1]
#roots are polyroot((z))
#0+1.054093i 0-1.054093i

#cycles per unit time 
(arg = Arg(a)/(2*pi))
#0.25
#sample acf plot
ACF = ARMAacf(ar = c(0 , -0.9) , ma = 0 , 100)
plot(ACF , type = "h" , ylab="Theoritical ACF" , xlab="h")

sqrt(1/.9)

atan(1.054093/0)
cos(pi/2)

(pi/2) - 1.570796

##Theoritical sim 

h = 1:100
ACF = ((1.54093)^(-h))*cos(Arg(a)*h)

plot(ACF , type = "h" , ylab="Sample ACF" , xlab="h")


sqrt(1/.9)
