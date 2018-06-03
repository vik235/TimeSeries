##Random walk with a drift

set.seed(154) # so you can reproduce the results

w = rnorm(200)
x = cumsum(w)


wd = w +.2
xd = cumsum(wd)

tsplot(xd, ylim=c(-5,55), main="random walk", ylab='')
abline(a=0, b=.2, lty=2) # drift
lines(x, col=4)
abline(h=0, col=4, lty=2)
