install.packages("readxl")
library(readxl)
install.packages("gdata")
library(gdata)
install.packages("timeDate")
library(timeDate)
install.packages("timeSeries")
library(timeSeries)
library(forecast)
install.packages("ggfortify")
library(ggfortify)
library(grid)
library(gridExtra)
install.packages("dynlm")
library(dynlm)


### Import GFT Data
GFluTrends<-read_excel("/Users/fsmh/Desktop/case_study_1_fluwatch.xlsx",sheet="Google Flu Trends", skip = 1)
head(names(GFluTrends))
gft.Canada<-as.matrix(GFluTrends[,"Canada"])
first(GFluTrends[,"Date"])
last(GFluTrends[,"Date"])

## Plot
tim<-timeSequence(from = "2003-09-28", to = "2015-08-09", by = "week")
gft.CA<- timeSeries(GFluTrends[,"Canada"], charvec = tim)
par(mfrow = c(1,1), cex = 0.8)
plot(gft.CA,type = "b", pch = 19, main = "Google Flu Trends data")
grid()

## Model GFT for prewhitening
# Model selection using forecast package
mod<-auto.arima(gft.CA, max.p = 60, max.q = 0, stationary = TRUE)
mod
par(mfrow = c(1,2))
acf(gft.CA)
acf(mod$resid)
# Model adequacy test
p1<-autoplot(acf(gft.CA, plot = FALSE), conf.int.fill = '#0000FF', conf.int.value = 0.8, conf.int.type = 'ma') 
p2<-autoplot(acf(mod$resid, plot = FALSE), conf.int.fill = '#0000FF', conf.int.value = 0.8, conf.int.type = 'ma') 
grid.arrange(p1, p2, ncol=2)
# Create 'filter'
f1<-c(1,-mod$coef[1:3])


### Import FluWatch Data
fluWatch.Canada<-read_excel("/Users/fsmh/Desktop/case_study_1_fluwatch.xlsx", sheet="FluWatch-Canada", skip = 2)
head(names(fluWatch.Canada))
first(fluWatch.Canada[,"Date"])
last(fluWatch.Canada[,"Date"])

## Plot
tim1<-timeSequence(from = "2003-09-07", to = "2015-08-23", by = "week")
fluPos.CA<- timeSeries(fluWatch.Canada[,"FluPos"], charvec = tim1)
fluTest.CA<- timeSeries(fluWatch.Canada[,"FluTest"], charvec = tim1)
# Time series plot of numbers of flu tests and positive flu tests on Canada
par(mfrow = c(1,1), cex =0.75)
plot(fluTest.CA, type = "b", pch = 19, ylab = "#")
lines(fluPos.CA, col =2, type = "b", pch = 20)
legend("topleft",c("flu test", "flu postive"), col = c(1,2), pch = c(19,20))
grid()



### Prewhitening
fFluTest.CA<-filter(window(fluTest.CA, start = "2003-09-28", end = "2015-08-09"),f1,method=c("convo"),sides=1) 
fFluPos.CA<-filter(window(fluPos.CA, start = "2003-09-28", end = "2015-08-09"),f1,method=c("convo"),sides=1) 
par(mfrow = c(2,1))
yf<-fFluTest.CA[-(1:3)]
xf<-mod$res[-(1:3)]
par(cex=0.75,bg="gray95")
ccf(xf, yf, lwd=1, ylab="Cross-correlation functions", 
    main="CCF of prewhitened GFT and flu test")
abline(v=0, col="gold", lwd=2, lty="dashed")
text(-1, 0.2, "-1", col=2)
text(-2, 0.2, "-2", col=2)
yf1<-fFluPos.CA[-(1:3)]   
par(cex=0.75,bg="gray95")
ccf(xf, yf1, lwd=1, ylab="Cross-correlation functions", 
    main="CCF of prewhitened GFT and flu postive")
abline(v=0, col="gold", lwd=2, lty="dashed")
text(-1, 0.2, "-1", col=2)



### TFN Estimation
y<-fluTest.CA
y1<-fluPos.CA
x<-gft.CA
dat<- cbind(y,x, lag(x), lag(x,2))[-(1:2),]
dat1<- cbind(y1, x,lag(x))[-1,]
colnames(dat)<-c("fluTest", "gtf", "gtfL1", "gftL2")
colnames(dat1)<-c("fluPos", "gtf", "gtfL1")

## TFN model for flu tests Method 1
# Do not consider serially correlated error terms
mod.test<-dynlm(dat[,1]~dat[,-1])
summary(mod.test)
autoplot(acf(mod.test$resid, plot = FALSE), conf.int.fill = '#0000FF', conf.int.value = 0.8, conf.int.type = 'ma') 

## TFN model for flu tests Method 2
# Consider serially correlated error terms
mod.flutest<-auto.arima(dat[,1],xreg=dat[,-1], stationary = TRUE)
mod.flutest
autoplot(acf(tseries::na.remove(mod.flutest$resid),plot = FALSE), conf.int.fill = '#0000FF', conf.int.value = 0.8,conf.int.type = 'ma') 

#### Exercise: Read R output and write down the model in mathematics.
#### Exercise: Carry out the model diagnostic tests (Readmodel diagnostic lecture note for more details).

## TFN model for positive flu tests
mod.pos<-dynlm(dat1[,1]~dat1[,-1])
summary(mod.pos)
autoplot(acf(tseries::na.remove(mod.pos$resid), plot = FALSE), conf.int.fill = '#0000FF', conf.int.value = 0.8, conf.int.type = 'ma') 
mod.flupos<-auto.arima(dat1[,1],xreg=dat1[,-1], stationary = TRUE)
mod.flupos
autoplot(acf(tseries::na.remove(mod.flupos$resid), plot = FALSE), conf.int.fill = '#0000FF', conf.int.value = 0.8, conf.int.type = 'ma') 
mod.flupos1<-arima(dat1[,1],c(1,0,4), xreg=dat1[,-1])
mod.flupos1
autoplot(acf(tseries::na.remove(mod.flupos1$resid), plot = FALSE), conf.int.fill = '#0000FF', conf.int.value = 0.8, conf.int.type = 'ma') 

#### Exercise: Read R output and write down the model in mathematics


### Prediction
par(mfrow=c(2,1), cex = 0.75)
plot(fluTest.CA, type = "b", pch = 21); grid()
plot(gft.CA, type = "b", pch = 15); grid()
par(mfrow = c(1,1))
plot(fluTest.CA, type = "b", pch = 21)
lines(timeSeries(c(rep(NA,7),predict(mod.test)),charvec = timeSequence(from = "2003-09-07", to = "2015-08-23", by = "week")),
      col = 2, pch = 15, type = "b", main = "# of flu tests and Google flu trends"); grid()

#### Exercise: Calculate MSE and MAE of the above prediction.



#### Are the seasonal trends in the GFT data more strongly associated with the number of influenza
#infections (i.e., number of positive tests) or the total number of tests? 

####Does GFT lead or lag the number of positive influenza tests?

#### Are the seasonal trends in the GFT data associated with the number of tests and positive test results 
#for other respiratory viruses?

#### How many weeks ahead can you predict the peak in the number of influenza positive tests?
#### Does the data from GFT help to predict the peak?
  
  

