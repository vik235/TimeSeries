##Auto regressive 2nd order model simulation 
#The model is given by x_t = x__t-1 - 0.9 * x_t-2 + w_t , assume x_0 and x_-1 is given to avoid startup troubles
#Another easier way is to use arima.sim , more on that later/ 

set.seed(1234)

library(astsa)

w = rnorm(550 , 0 , 1) # We intend to use 500 but extra 50 is to cover for startup problems
x = filter(w , filter = c(1 , -.1) , method = "recursive")[-(1:50)]
tsplot(x , main = "autoregession model")#Inducing seasonality , periodicity etc in the xt.

x = filter(w , filter = c(1 , -.9) , method = "recursive")[-(1:50)]
tsplot(x , main = "autoregession model")#Inducing seasonality , periodicity etc in the xt.

x = filter(w , filter = c(1 , .1) , method = "recursive")[-(1:50)]
tsplot(x , main = "autoregession model")#Inducing seasonality , periodicity etc in the xt.

set.seed(1234)
w = rnorm(550 , 0 , 1) # We intend to use 500 but extra 50 is to cover for startup problems
x = filter(w , filter = c(-1) , method = "recursive")[-(1:50)]
tsplot(x , main = "autoregession model")#Inducing seasonality , periodicity etc in the xt.



acf(globtemp)

