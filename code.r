setwd("c:/") #此處需更改路徑
library(aTSA)
library(TSA)
library(tseries)
library(fGarch)

#data
data1<-read.csv("CPI.csv")
data2<-read.csv("unemployment_rate.csv")
cpi<-data1[['CPI']]
rate<-data2[['rate']]

######################################################### CPI #########################################################

#time series
plot(cpi,type="l",ylab="CPI",xlab="Time",xaxt="n")
axis(1, at=seq(0,479,12), labels = (seq(1981,2020,1)))

#monthplot
m<-matrix(cpi,nrow=40,ncol=12,byrow=TRUE)
colnames(m)<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Set","Oct","Nov","Dec")
rownames(m)<-c(1981:2020)
monthplot(m)

#difference
adf.test(cpi)
kpss.test(cpi) #need difference
adf.test(diff(cpi))
kpss.test(diff(cpi)) #just need first diff

#acf&pacf&eacf
acf(diff(cpi),lag=60,main="Fisrt Difference of CPI")
pacf(diff(cpi),lag=60,main="Fisrt Difference of CPI")
eacf(diff(cpi))

acf(diff(diff(cpi), lag=12),lag.max=60,ci.type='ma',main="Fisrt Difference and Seasonal First Difference of CPI")
pacf(diff(diff(cpi), lag=12),lag.max=60,main="Fisrt Difference and Seasonal First Difference of CPI")
eacf(diff(diff(cpi), lag=12))

#SARIMA(0,1,3)*(0,1,1)
arima(cpi,order=c(0,1,3),seasonal=list(order=c(0,1,1),period=12)) #aic=832.36
arima(cpi,order=c(0,1,2),seasonal=list(order=c(0,1,1),period=12)) #aic=831.61

#model diagonstic ... SARIMA(0,1,2)*(0,1,1)
mod1<-arima(cpi,order=c(0,1,2),seasonal=list(order=c(0,1,1),period=12))

detectAO(mod1) #no
detectIO(mod1) #188 242 322
##
arimax(cpi,order=c(0,1,2),seasonal=list(order=c(0,1,1),period=12),io=c(188,242,322)) #aic=791.55
mod2<-arimax(cpi,order=c(0,1,2),seasonal=list(order=c(0,1,1),period=12),io=c(188,242,322))

detectAO(mod2) #no
detectIO(mod2) #no

res2=residuals(mod2)
plot(res2,ylab="Residuals")
hist(res2,main="Histogram of Residuals")
qqnorm(res2);qqline(res2)

t.test(res2) #zero-mean...ok
shapiro.test(res2) #nromal...not

acf(res2,lag = 36,main="Residuals")
Box.test(res2, lag = 11,type = "Ljung") #white-noise...ok
Box.test(res2, lag = 36,type = "Ljung") #white-noise...ok

#SARIMA(1,1,1)*(0,1,1)
arima(cpi,order=c(1,1,1),seasonal=list(order=c(0,1,1),period=12)) #aic=831.05

#model diagonstic ... SARIMA(1,1,1)*(0,1,1)
mod3<-arima(cpi,order=c(1,1,1),seasonal=list(order=c(0,1,1),period=12))

detectAO(mod3) #no
detectIO(mod3) #188 322
##
arimax(cpi,order=c(1,1,1),seasonal=list(order=c(0,1,1),period=12),io=c(188,322)) #aic=803.58
mod4<-arimax(cpi,order=c(1,1,1),seasonal=list(order=c(0,1,1),period=12),io=c(188,322))

detectAO(mod4) #no
detectIO(mod4) #no

res4=residuals(mod4)
plot(res4,ylab="Residuals")
hist(res4,main="Histogram of Residuals")
qqnorm(res4);qqline(res4)

t.test(res4) #zero-mean...ok
shapiro.test(res4) #nromal...not

acf(res4,lag = 36,main="Residuals")
Box.test(res4, lag = 11,type = "Ljung") #white-noise...ok
Box.test(res4, lag = 36,type = "Ljung") #white-noise...ok

#Garch
McLeod.Li.test(y=res2) #no
win.graph(width=6, height=2.5,pointsize=8)
par(mfrow=c(1,2))
acf(res2^2,main="Squared Residuals")
pacf(res2^2,main="Squared Residuals")

#cpi_res
cpi_res<-res2
######################################################### CPI #########################################################

#time series
plot(rate,type="l",ylab="Unemployment Rate",xlab="Time",xaxt="n")
axis(1, at=seq(0,479,12), labels = (seq(1981,2020,1)))

#monthplot
m<-matrix(rate,nrow=40,ncol=12,byrow=TRUE)
colnames(m)<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Set","Oct","Nov","Dec")
rownames(m)<-c(1981:2020)
monthplot(m)

#difference
adf.test(rate)
kpss.test(rate) #need difference
adf.test(diff(rate))
kpss.test(diff(rate)) #just need first diff

#acf&pacf&eacf
acf(diff(rate),lag=60,main="Fisrt Difference of Unemployment Rate")
pacf(diff(rate),lag=60,main="Fisrt Difference of Unemployment Rate")
eacf(diff(rate))

acf(diff(diff(rate), lag=12),lag.max=60,ci.type='ma',main="Fisrt Difference and Seasonal First Difference of Unemployment Rate")
pacf(diff(diff(rate), lag=12),lag.max=60,main="Fisrt Difference and Seasonal First Difference of Unemployment Rate")
eacf(diff(diff(rate), lag=12))

#SARIMA(0,1,4)*(0,1,1)
arima(rate,order=c(0,1,4),seasonal=list(order=c(0,1,1),period=12)) #aic=-530.37
arima(rate,order=c(0,1,4),seasonal=list(order=c(0,1,1),period=12),fixed=c(0,0,0,NA,NA)) #aic=-531.32

#model diagonstic ... SARIMA(0,1,4)*(0,1,3)
mod1<-arima(rate,order=c(0,1,4),seasonal=list(order=c(0,1,1),period=12),fixed=c(NA,NA,NA,NA,NA))

detectAO(mod1) #51 68 80 86
detectIO(mod1) #29 51 55 57 62 86 336...(7)
##
mod2<-arimax(rate,order=c(0,1,4),seasonal=list(order=c(0,1,1),period=12)
       ,io=c(29,51,55,57,62,86,87,336)
       ,xreg=data.frame(Apr.2020=1*(seq(rate)==472))) #aic=-668.75

detectAO(mod2) #68 80 110 122
detectIO(mod2) #no

res2=residuals(mod2)
plot(res2,ylab="Residuals")
hist(res2,main="Histogram of Residuals")
qqnorm(res2);qqline(res2)

t.test(res2) #zero-mean...ok
shapiro.test(res2) #nromal...not

acf(res2,lag = 36,main="Residuals") #white-noise...ok

#GARCH
McLeod.Li.test(y=res2) #yes
win.graph(width=6, height=2.5,pointsize=8)
par(mfrow=c(1,2))
acf(res2^2,main="Squared Residuals")
pacf(res2^2,main="Squared Residuals")
eacf(res2^2)

mod3<-garchFit(~garch(4, 0), data = res2, include.mean=F)
summary(mod3)

vol=volatility(mod3)
res=res2/vol

plot(res,type='l')
hist(res)
qqnorm(res);qqline(res)

t.test(res)
shapiro.test(res)
ks.test(res,"pnorm",mean(res),sd(res))
###############################################以下需要更改
win.graph(width=6, height=2.5,pointsize=8)
par(mfrow=c(1,2))
acf(res^2,main="Squared Residuals")
pacf(res^2,main="Squared Residuals") #這邊圖跑出來不太一樣，code你看看有沒有哪裡跟你不一樣

Box.test(res , lag = 5,type = "Ljung")
Box.test(res , lag = 12,type = "Ljung")
Box.test(res^2 , lag = 8,type = "Ljung")
Box.test(res^2 , lag = 19,type = "Ljung")

#rate_res
rate_res<-res

#CCF
ccf(cpi_res,rate_res)

#Corr
res=lm(cpi_res[9:length(cpi)]~rate_res[1:(length(rate)-8))])
summary(res)

res=lm(cpi_res[11:length(cpi)]~rate_res[1:(length(rate)-10)])
summary(res)

res=lm(rate_res[14:length(rate)]~cpi_res[1:(length(cpi)-13)])
summary(res)

res=lm(rate_res[16:length(rate)]~cpi_res[1:(length(cpi)-15)])
summary(res)

#SARIMA(0,1,0)*(0,1,1)
arima(rate,order=c(0,1,0),seasonal=list(order=c(0,1,1),period=12)) #aic=-515.38

#model diagonstic ... SARIMA(0,1,0)*(0,1,1)
mod4<-arima(rate,order=c(0,1,0),seasonal=list(order=c(0,1,1),period=12)) #這邊要補上你的code

detectAO(mod4) #51 68 80 86
detectIO(mod4) #29 51 55 86 336
