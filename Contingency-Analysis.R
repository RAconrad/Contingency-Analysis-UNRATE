#Contingency Analysis Forecast COVID-19 Unemployment Rate

unemp <- read.csv("~/Downloads/unemployment.csv")
jobless_claims <- read.csv("~/Downloads/claims3.csv")

library(urca)
library(forecast)
library(ggplot2)

unemp

unrate <- ts(unemp[,2], start=c(1980,1), frequency=12)
plot(unrate)

Arima(unrate,order=c(2,0,2), seasonal = c(1,0,1))

forecast(Arima(unrate, order=c(2,0,2), seasonal=c(1,0,1)), h=2)

jobless_claims

claims= ts(jobless_claims[,2], start=c(1980,1), frequency=12,end = c(2020,2))
plot(claims)

summary(ur.df(claims, type="drift", lags=25, selectlags="AIC"))

#xreg = exogenous regressor
Arima(unrate,order=c(2,0,2), seasonal=c(1,0,1), xreg = claims/1000)

modelXREG=Arima(unrate,order=c(2,0,2), seasonal=c(1,0,1), xreg = claims/1000)

forecast(modelXREG, h=1, xreg=618.375)

unrate2 =ts(c(unrate, 4.4), start=c(1980, 1), frequency=12)
claims2=ts(jobless_claims[,2], start=c(1980,1), frequency=12)
tail(claims2,10)

claims2

Arima(unrate2, order=c(2,0,2), seasonal=c(1,0,1), xreg = claims2/1000)

finalMODEL=Arima(unrate2, order=c(2,0,2), seasonal=c(1,0,1), xreg = claims2/1000)

forecast(Arima(unrate2, order=c(2,0,2), seasonal=c(1,0,1)), h=1)

#4 million jobless claims unemployment rate in April
forecast(finalMODEL, h=1, xreg=4000)

#5 million jobless claims unemployment rate in April
forecast(finalMODEL, h=1, xreg=5000)

#6 million jobless claims unemployment rate in April
forecast(finalMODEL, h=1, xreg=6000)

#unrate is a function of claims and claims(-1) LINING UP DATA SERIES
#What is a lagged value relative to Jan 1980
unrate3=ts(unrate2[2:483], start=c(1980,2), frequency=12)
unrate3
#match february unrate of 1980 with claims of Jan 1980
claimsLAG=ts(jobless_claims[1:482,2], start =c(1980,2), frequency=12)
claimsLAG
claimsNoLAG=ts(jobless_claims[2:483,2], start=c(1980,2), frequency=12)
claimsNoLAG

Arima(unrate3, order=c(2,0,2), seasonal = c(1,0,1), xreg = cbind(claimsLAG,claimsNoLAG/1000))

modelFINAL=Arima(unrate3, order=c(2,0,2), seasonal = c(1,0,1), xreg = cbind(claimsLAG/1000,claimsNoLAG/1000))
forecast(modelFINAL, xreg=cbind(618.375, 5000))

forecast(modelFINAL, xreg=cbind(618.375, 6000))



