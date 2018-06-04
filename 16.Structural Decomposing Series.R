## Structural decompostion of a time series into Trend, Seasonal and Random component. 
## A Classical Structural Modelling Technique where 
## a time series may be written as x_t = T_t + s_t + N_t 
## We can decompose this into such terms. However, not applicable everywhere. 

## Demonstrating via Hawaiian Quarterly Occupancy rate/

tsplot(hor)

x = window(hor , start = 2002)
plot(decompose(x))
plot(stl(x , s.window = 'per'))
plot(stl(x , s.window = 15))
