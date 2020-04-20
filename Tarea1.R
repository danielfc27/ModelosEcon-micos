##Nivel básico
library(quantmod)
library(PerformanceAnalytics)
#Descargando MSFT
getSymbols.yahoo('MSFT',env=globalenv(),return.class= "xts",from='1996-01-01',to=Sys.Date(),periodicity = 'daily')
View(MSFT)
chartSeries(MSFT,theme="black")
names(MSFT) <- c("Open","High","Low","Close","Volume","Adjusted")
par(mfrow=c(2,2))
chart.TimeSeries(MSFT$Open,main="Open Price MSFT",colorset = "red")
chart.TimeSeries(MSFT$High,main="High Price MSFT",colorset = "green")
chart.TimeSeries(MSFT$Low,main="Low Price MSFT",colorset = "black")
chart.TimeSeries(MSFT$Close,main="Close Price MSFT",colorset = "darkmagenta")
#TSLA
getSymbols.yahoo('TSLA',env=globalenv(),return.class= "xts",from='1996-01-01',to=Sys.Date(),periodicity= 'daily')
View(TSLA)
names(TSLA) <- c("Open","High","Low","Close","Volume","Adjusted")
par(mfrow=c(1,2))
chart.TimeSeries(OHLC(TSLA),main = "OHLC TSLA")
chart.TimeSeries(TSLA$Close, main = "Close Price TSLA",colorset ="blue")
#Apple
getSymbols.yahoo('AAPL',env=globalenv(),return.class= "xts",from='1996-01-01',to=Sys.Date(),periodicity= 'daily')
View(AAPL)
names(AAPL) <- c("Open","High","Low","Close","Volume","Adjusted")
#Calculando los retornos
AAPL_ret <- Return.calculate(AAPL$Close,method = "compound")
View(AAPL_ret)
AAPL_ret <- AAPL_ret[-1,]
par(mfrow=c(2,1))
chart.TimeSeries(AAPL$Close,main="Close Price AAPL",colorset = "orange")
chart.TimeSeries(AAPL_ret,main="Retornos de AAPL",colorset = "skyblue")
#ID, gráfica adicional para el análisis
library(readxl)
ID2 <- read_excel("ID.xlsx")
names(ID2) <- c("Year","IDEEUU")
Gasto_en_Investigación_y_desarrollo_EEUU_en_términos_de_PBI <- ts(ID2$IDEEUU,start = 1996,end = 2017)
chartSeries(Gasto_en_Investigación_y_desarrollo_EEUU_en_términos_de_PBI)

#/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

##Nivel intermedio
getSymbols.yahoo('KO',env=globalenv(),return.class= "xts",from='2000-01-01',to=Sys.Date(),periodicity = 'daily')
getSymbols.yahoo('DAX',env=globalenv(),return.class= "xts",from='2000-01-01',to=Sys.Date(),periodicity = 'daily')
getSymbols.yahoo('EURUSD=X',env=globalenv(),return.class= "xts",from='2000-01-01',to=Sys.Date(),periodicity = 'daily')
names(KO) <- c("Open","High","Low","Close","Volume","Adjusted")
names(DAX) <- c("Open","High","Low","Close","Volume","Adjusted")
names(`EURUSD=X`) <- c("Open","High","Low","Close","Volume","Adjusted")
KOclose <- KO$Close
DAXclose <- DAX$Close
EUclose <- `EURUSD=X`$Close
#install.packages("urca")
library(urca)
tko <- ur.df(KOclose,type = c("trend"),selectlags = c("BIC"))
summary(tko)
tdax <- ur.df(DAXclose,type = c("trend"),selectlags = c("BIC"))
summary(tdax)
EUclose<- EUclose[-which(is.na(EUclose)),]
teu <- ur.df(EUclose,type = c("trend"),selectlags = c("BIC"))
summary(teu)
###Calculando retornos
KO_ret <- Return.calculate(KO$Close,method = "compound")
DAX_ret <- Return.calculate(DAXclose$Close,method = "compound")
EU_ret <- Return.calculate(EUclose,method = "compound")
KO_ret <- KO_ret[-1,]
DAX_ret <- DAX_ret[-1,]
EU_ret <- EU_ret[-1,]
tkoret <- ur.df(KO_ret,type = c("trend"),selectlags = c("BIC"))
summary(tkoret)
tdaxret <- ur.df(DAX_ret,type = c("trend"),selectlags = c("BIC"))
summary(tdaxret)
teuret <- ur.df(EU_ret,type = c("trend"),selectlags = c("BIC"))
summary(teuret)
#Correlogramas
par(mfcol=c(2,3))
acf(KOclose,main="Correlograma en su nivel KO")
acf(KO_ret,main="Correlograma de los retornos KO")
acf(DAXclose,main="Correlograma en su nivel DAX")
acf(DAX_ret,main="Correlograma de los retornos DAX")
acf(EUclose,main="Correlograma en su nivel EURUSD=X")
acf(EU_ret,main="Correlograma de los retornos EURUSD=X")
par(mfcol=c(1,1))

#//////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#Nivel avanzado
library(forecast)
library(MLmetrics)
library(normtest)
library(readxl)
library(xts)
Series <- read_excel("Bono.xlsx")
names(Series) <- c("Date","BPER","USDPEN")
BPER <- xts(Series$BPER,order.by = as.Date(Series$Date))
USDPEN <- xts(Series$USDPEN, order.by = as.Date(Series$Date))
cor(x=USDPEN,y=BPER,method="pearson")
#Retronos
USDPEN_ret <- diff(log(USDPEN),lag=1)
USDPEN_ret <- USDPEN_ret[-which(is.na(USDPEN_ret)),]
BPER_ret <- diff(log(BPER),lag=1)
BPER_ret <- BPER_ret[-which(is.na(BPER_ret)),]
#correlaciónDinámica
ccf(as.ts(USDPEN_ret),as.ts(BPER_ret),main=bquote(USDPEN[t] == alpha[0] + alpha[1]*BONOPER[t+h]+epsilon[t]))
#BOX-JENKINS 
# 1) IDENTIFICACIÓN

tUSDPEN <- ur.df(USDPEN_ret,type = c("trend"),selectlags = c("BIC"))
summary(tUSDPEN)
tBPER <- ur.df(USDPEN_ret,type = c("trend"),selectlags = c("BIC"))
summary(tBPER)
#Correlogramas
par(mfrow=c(1,2))
acf(USDPEN, main="ACF de los retornos de USDPEN")
pacf(USDPEN, main="PACF de los residuos de USDPEN")
par(mfrow=c(1,1))
s
# 2) ESTIMACIÓN
# /// Recortamos la muestra
USDPEN_smpl <- USDPEN_ret[1:123,]
BPER_smpl <- BPER_ret[1:123,]

## SELECCION DEL MODELO 
#CRITERIO BAYESIANO

m1 = auto.arima(USDPEN_smpl,d=NA,D=NA,max.p=1,max.q=2,max.P=NA,max.Q=NA,max.d=0,max.D=NA,
                seasonal=FALSE,xreg=BPER_smpl,ic=c("bic"))
m2 = auto.arima(USDPEN_smpl,d=NA,D=NA,max.p=2,max.q=1,max.P=NA,max.Q=NA,max.d=0,max.D=NA,
                seasonal=FALSE,xreg=NULL,ic=c("bic"))
#CRITERIO AKAIKE

m3 = auto.arima(USDPEN_smpl,d=NA,D=NA,max.p=2,max.q=1,max.P=NA,max.Q=NA,max.d=0,max.D=NA,
                seasonal=FALSE,xreg=BPER_smpl,ic=c("aic"))
m4 = auto.arima(USDPEN_smpl,d=NA,D=NA,max.p=2,max.q=1,max.P=NA,max.Q=NA,max.d=0,max.D=NA,
                seasonal=FALSE,xreg=NULL,ic=c("aic"))
summary(m1)
summary(m2)
summary(m3)
summary(m4)

## Almacenamos los datos ajutados de cada modelo

Date_S <- as.Date(index(USDPEN_smpl))
Dates <- as.Date(index(USDPEN_ret))

USDPEN_Hat1 <- m1$fitted 
USDPEN_Hat1 <- xts(USDPEN_Hat1,order.by = Date_S)

USDPEN_Hat2 <- m2$fitted 
USDPEN_Hat2 <- xts(USDPEN_Hat2,order.by = Date_S)

USDPEN_Hat3 <- m3$fitted 
USDPEN_Hat3 <- xts(USDPEN_Hat3,order.by = Date_S)

USDPEN_Hat4 <- m4$fitted 
USDPEN_Hat4 <- xts(USDPEN_Hat4,order.by = Date_S)

Model.Series1 <- cbind(USDPEN_smpl,USDPEN_Hat1)
Model.Series2 <- cbind(USDPEN_smpl,USDPEN_Hat2)
Model.Series3 <- cbind(USDPEN_smpl,USDPEN_Hat3)
Model.Series4 <- cbind(USDPEN_smpl,USDPEN_Hat4)

names(Model.Series1) <- c("Observado","Ajustado")
names(Model.Series2) <- c("Observado","Ajustado")
names(Model.Series3) <- c("Observado","Ajustado")
names(Model.Series4) <- c("Observado","Ajustado")

par(mfrow=c(2,2))
chart.TimeSeries(Model.Series1,main="ARMAX(1,0)",legend.loc="bottomleft")
chart.TimeSeries(Model.Series1,main="ARMA(0,1)",legend.loc="bottomleft")
chart.TimeSeries(Model.Series1,main="ARMAX(1,1)",legend.loc="bottomleft")
chart.TimeSeries(Model.Series1,main="ARMA(0,1)",legend.loc="bottomleft")

##Calculamos el error cuadrático medio

MSE1 = MSE(USDPEN_Hat1,USDPEN_ret)
MSE2 = MSE(USDPEN_Hat2,USDPEN_ret)
MSE3 = MSE(USDPEN_Hat3,USDPEN_ret)
MSE4 = MSE(USDPEN_Hat4,USDPEN_ret)

MSE_T = cbind(MSE1,MSE2,MSE3,MSE4)
MSE_T

## 3)DIAGNÓSTICO

checkresiduals(m1)
checkresiduals(m2)
checkresiduals(m3)
checkresiduals(m4)

#Test de Jarque Bera, libreria normtest

jb.norm.test(m1$residuals)
jb.norm.test(m2$residuals)
jb.norm.test(m3$residuals)
jb.norm.test(m4$residuals)

## 4)PREDICCIÓN

M1_F <- forecast(m1,h=5,level=c(30,60,90),xreg=BPER_ret[124:128,])
M2_F <- forecast(m2,h=5,level=c(30,60,90))
M3_F <- forecast(m3,h=5,level=c(30,60,90),xreg=BPER_ret[124:128,])
M4_F <- forecast(m4,h=5,level=c(30,60,90))

par(mfrow=c(2,2))
plot(M1_F,main = "Forecast ARMAX(1,0)")
plot(M2_F,main = "Forecast ARMA(0,1)")
plot(M3_F,main = "Forecast ARMAX(1,1)")
plot(M4_F,main = "Forecast ARMA(0,1)")

Date_F <- index(USDPEN[124:128])
USDPEN_SMPL_F = as.ts(USDPEN_ret[124:128,],start=124,end=128)

#Error Cuadrático Medio de las proyecciones

MSE1.F <- MSE(M1_F$mean,USDPEN_SMPL_F)
MSE2.F <- MSE(M2_F$mean,USDPEN_SMPL_F)
MSE3.F <- MSE(M3_F$mean,USDPEN_SMPL_F)
MSE4.F <- MSE(M4_F$mean,USDPEN_SMPL_F)

MSE.T.F <- cbind(MSE1.F,MSE2.F,MSE3.F,MSE4.F)
MSE.T.F

Forecasts <- cbind(USDPEN_SMPL_F,M1_F$mean,M2_F$mean,M3_F$mean,M4_F$mean)
Forecasts <- xts(Forecasts,order.by = Date_F)
names(Forecasts) <- c("USDPEN","ARMAX(1,0)","ARMA(0,1)","ARMAX(1,1)","ARMA(0,1)")
Forecasts <- as.ts(Forecasts)

#Graficamos
par(mfrow=c(1,1))
chart.TimeSeries(Forecasts, main="Proyecciones para USDPEN",legend.loc = "bottomleft")



