##Demo of plotting the various lag data automatically
library(astsa)

(r = round(acf(soi, 6, plot=FALSE)$acf[-1], 3)) # sample acf values

#[1] 0.604 0.374 0.214 0.050 -0.107 -0.187

par(mfrow=c(1,2), mar=c(3,3,1,1), mgp=c(1.6,.6,0))
plot(lag(soi,-1), soi); legend('topleft', legend=r[1])
plot(lag(soi,-6), soi); legend('topleft', legend=r[6])
