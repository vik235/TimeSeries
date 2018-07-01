##' This script simulates an IMA(1,1) - i.e. an integrated moving average model, 
##' also written succintly as x_t = x_t-1 + w_t + lamda*w_t-1
##'  (think of lamda as theta in MA model). The forecasting model reduces to 
##'  exponential smotthign models also called as exponential weighted moving avrage model
##

## We use Holt-Winters in R to do this. 

##' Model is simulated with lamda = - theta = 0.8
##' 
##' It is still in use as it is simple. By the old men who smell bad because they 
##' are simple and easy to use.
##' 
##

set.seed(666)

x = arima.sim(list(order = c(0 ,1 ,1) , ma = -0.8) , n = 100 )

(x.ima = HoltWinters(x , beta = FALSE , gamma = FALSE))

tsplot(x)
plot(x.ima , main = "EWMA")

names(x.ima)
