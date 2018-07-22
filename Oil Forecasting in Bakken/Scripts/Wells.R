library(astsa)
library(forecast)
library(mgcv)

wells = read.csv(file = "C:/Users/vigupta/OneDrive/Learning/DataScience/Statistics Texas A&M University/626/Project/Data/US_OG_BAKKEN Production Time Series.CSV")
options(scipen=999)

head(wells)
colnames(wells) = c("EntityID" , "API_UWI" , "API_UWI_List", "ProdDate" , "Liquid" , "Gas" , "Water" , "WellCount" , "Days" , "DailyAvgOil" , "DailyAvgGas" , "DailyAvgWater")

head(wells)

length(unique(wells$API_UWI))
#60 wells 

unique(wells$API_UWI)[1:12]
unique(wells$API_UWI)[13:24]

well_01_vk = wells[ wells$API_UWI == "33061005340000" , ]

dim(wells)
dim(well_01_vk)

plot.ts(cbind(well_01_vk$Liquid , well_01_vk$Gas , well_01_vk$Water ))

plot(density(well_01_vk$Liquid))

fc = auto.arima(well_01_vk$Liquid , lambda = 0.5)

autoplot(fc)


ts(well_01_vk$Liquid) %>% BoxCox(lambda = 0) %>% autoplot()
ts(well_01_vk$Liquid) %>% BoxCox(lambda = 1) %>% autoplot()
ts(well_01_vk$Liquid) %>% BoxCox(lambda = -.5) %>% autoplot()
ts(well_01_vk$Liquid) %>% BoxCox(lambda = -1) %>% autoplot()
ts(well_01_vk$Liquid) %>% BoxCox(lambda = -0.0414) %>% autoplot()
plot(density(well_01_vk$Liquid^(-0.0414)))

BoxCox.lambda(well_01_vk$Liquid , method = "l")


ts01 = ts(well_01_vk$Liquid)

fit01 = auto.arima(ts01 , lambda = -0.0414)
checkresiduals(fit01)
summary(fit01)

fit01 %>% forecast(h = 10) %>% autoplot()

well_01_vk$LiquidTf = well_01_vk$Liquid^(-0.0414)
well_01_vk$LiquidTf2 = well_01_vk$Liquid^(-0.15)

ts.plot(well_01_vk$LiquidTf2)
tsplot(diff(well_01_vk$LiquidTf2))
Box.test(diff(well_01_vk$LiquidTf2) , lag = 100 , type = "Ljung-Box" )
acf2(diff(well_01_vk$LiquidTf2))

sarima(well_01_vk$LiquidTf , 0 , 1, 0 , 0 , 0 , 0 , 0)

plot(density(well_01_vk$Gas))

tsplot(well_01_vk$Gas)

BoxCox.lambda(well_01_vk$Gas , method = "l")
well_01_vk$GasTf = log(well_01_vk$Gas)

plot(density(well_01_vk$GasTf))

tsplot(well_01_vk$GasTf)
tsplot(diff(well_01_vk$GasTf))

fit_01_gas = Arima(ts(well_01_vk$Gas), order = c(0 , 1, 0) , include.mean = TRUE , lambda = 0 , biasadj = TRUE )
checkresiduals(fit_01_gas)
accuracy(fit_01_gas)

tsplot(well_01_vk$Water)
BoxCox.lambda(well_01_vk$Water , method = "l")


well_02_vk = wells[ wells$API_UWI == "33053027990000" , ]

plot.ts(well_01_vk$Water)


fit1 = gam(well_01_vk$Liquid ~s(well_01_vk$ProdDate))

head(well_01_vk)


acf2(diff(well_01_vk$Liquid))
tsplot(well_01_vk$DailyAvgOil)
acf(diff(well_01_vk$DailyAvgOil))
