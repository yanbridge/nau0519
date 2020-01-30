rm(list=ls())
library(quantmod)
library(rmgarch)
library(copula)


setwd("~/YanFiles/MyPapers/Crypto/Cryptodata")
bit.data<-read.csv("bitcoin4.csv")
date<-bit.data[,1]
bitcoin<-bit.data[,5]
litecoin<-bit.data[,9]
gold<-bit.data[,13]
sp500<-bit.data[,17]
Time <- as.Date(date,"%Y-%m-%d")
time<-Time[-1]

plot(Time,bitcoin,type="l",lwd=3,col=2)

ret.b<-diff(log(bit.data[,5]))
ret.l<-diff(log(bit.data[,9]))
ret.g<-diff(log(bit.data[,13]))
ret.s<-diff(log(bit.data[,17]))

#####     画出对数收益率图  ######
par(mfrow=c(2,2))
plot(time,ret.b,type="l",lwd=2,col=4,ylab="",xlab="")
plot(time,ret.l,type="l",lwd=2,col=4,ylab="",xlab="")
plot(time,ret.s,type="l",lwd=2,col=4,ylab="",xlab="")
plot(time,ret.g,type="l",lwd=2,col=4,ylab="",xlab="")


#####     1.将bitcoin and litecoin数据转化成xts数据   #####
data.bl<-as.data.frame(cbind(time,ret.l,ret.b))
data.bl<-xts(data.bl[,-1],order.by=as.Date(data.bl[,1]))
colnames(data.bl)<-c("Litecoin","Bitcoin")

####  DCC-GARCH  bitcoin with litecoin#####
uspec<- ugarchspec(mean.model = list(armaOrder=c(0,0)),
                   variance.model=list(model="gjrGARCH",garchOrder=c(1,1)),
                   distribution.model="sstd")
dcc.spec<-dccspec(uspec=multispec(replicate(2,uspec)),dccOrder=c(1,1),
                   distribution="mvt")     ## mvnorm, mvt

dcc.bl<-dccfit(dcc.spec,data=data.bl,solver = "lbfgs",
               fit.control = list(eval.se=TRUE))
class(dcc.bl)
slotNames(dcc.bl)
names(dcc.bl@mfit)
names(dcc.bl@model)
dcc.bl
plot(dcc.bl)

##### 提取动态相关系数 #####
id <- seq(from = 2, to = 2008, by = 2)
a12 <- dcc.bl@mfit$R
a12 <- as.data.frame(a12)
a12 <- t(a12)
a12 <- a12[,1]
a12 <- a12[id]
plot(time,a12,type="l",col="red")

## another method
bl.cor<-rep(0,1004)
for (i in 1:1004) bl.cor[i]<-dcc.bl@mfit$R[[i]][2]
plot(time,bl.cor,type="l",col="red")


#  Copula MODEL SPEPCIFICATION 
uspec.cop = ugarchspec(mean.model = list(armaOrder = c(1,1)), 
                       variance.model = list(garchOrder = c(1,1), model = "sGARCH"), 
                       distribution.model = "snorm") 

#COPULA APPROACH SPECIFICATIOIN
copspec    = cgarchspec(uspec = multispec(replicate(2, uspec.cop)), VAR = TRUE, robust = FALSE, lag = 2, lag.max = NULL, 
                          lag.criterion = c("AIC", "HQ", "SC", "FPE"), 
                          external.regressors = NULL, 
                          robust.control = list("gamma" = 0.25, "delta" = 0.01, "nc" = 10, "ns" = 500), 
                          dccOrder = c(1,1), asymmetric = FALSE, 
                        distribution.model = list(copula = c("mvnorm", "mvt")[2],
                                                  method = c("Kendall", "ML")[2], time.varying = TRUE, 
                                                  transformation = c("parametric", "empirical", "spd")[1])) 
copula.bl  = cgarchfit(copspec, data = data.bl, parallel = parallel, parallel.control = parallel.control, 
                         fit.control = list(eval.se=TRUE)) 

bl.copcor<-rep(0,1004)
for (i in 1:1004) bl.copcor[i]<-copula.bl@mfit$Rt[[i]][2]
plot(time,bl.copcor,type="l",col="red")

#####    2. 将bitcoin and gold数据转化成xts数据   #####
data.bg<-as.data.frame(cbind(time,ret.g,ret.b))
data.bg<-xts(data.bg[,-1],order.by=as.Date(data.bg[,1]))
colnames(data.bg)<-c("Gold","Bitcoin")

uspec<- ugarchspec(mean.model = list(armaOrder=c(0,0)),
                   variance.model=list(model="gjrGARCH",garchOrder=c(1,1)),
                   distribution.model="sstd")
dcc.spec<-dccspec(uspec=multispec(replicate(2,uspec)),dccOrder=c(1,1),
                  distribution="mvt")

dcc.bg<-dccfit(dcc.spec,data=data.bg,solver = "solnp",
               fit.control = list(eval.se=TRUE))

show(dcc.bg)
plot(dcc.bg)

bg.cor<-rep(0,1004)
for (i in 1:1004) bg.cor[i]<-dcc.bg@mfit$R[[i]][2]
plot(time,bg.cor,type="l",col="red")




#####    3. 将bitcoin and S&P 500 数据转化成xts数据   #####
data.bs<-as.data.frame(cbind(time,ret.s,ret.b))
data.bs<-xts(data.bs[,-1],order.by=as.Date(data.bs[,1]))
colnames(data.bs)<-c("S&P500","Bitcoin")

####  DCC-GARCH  bitcoin with S&P500 #####
uspec<- ugarchspec(mean.model = list(armaOrder=c(0,0)),
                   variance.model=list(model="gjrGARCH",garchOrder=c(1,1)),
                   distribution.model="sstd")
dcc.spec<-dccspec(uspec=multispec(replicate(2,uspec)),dccOrder=c(1,1),
                  distribution="mvt")     ## mvnorm, mvt

dcc.bs<-dccfit(dcc.spec,data=data.bs,solver = "solnp",
               fit.control = list(eval.se=TRUE))
dcc.bs

#####    4. 将litecoin and Gold 数据转化成xts数据   #####
data.lg<-as.data.frame(cbind(time,ret.g,ret.l))
data.lg<-xts(data.lg[,-1],order.by=as.Date(data.lg[,1]))
colnames(data.lg)<-c("Gold","litecoin")

####  DCC-GARCH  litecoin with Gold #####
uspec<- ugarchspec(mean.model = list(armaOrder=c(0,0)),
                   variance.model=list(model="gjrGARCH",garchOrder=c(1,1)),
                   distribution.model="std")
dcc.spec<-dccspec(uspec=multispec(replicate(2,uspec)),dccOrder=c(1,1),
                  distribution="mvt")     ## mvnorm, mvt

dcc.lg<-dccfit(dcc.spec,data=data.lg,solver = "solnp",
               fit.control = list(eval.se=TRUE))
dcc.lg

#####    5. 将litecoin and S&P500 数据转化成xts数据   #####
data.ls<-as.data.frame(cbind(time,ret.s,ret.l))
data.ls<-xts(data.ls[,-1],order.by=as.Date(data.ls[,1]))
colnames(data.ls)<-c("S&P500","litecoin")

####  DCC-GARCH  litecoin with S&P500 #####
uspec<- ugarchspec(mean.model = list(armaOrder=c(0,0)),
                   variance.model=list(model="gjrGARCH",garchOrder=c(1,1)),
                   distribution.model="std")
dcc.spec<-dccspec(uspec=multispec(replicate(2,uspec)),dccOrder=c(1,1),
                  distribution="mvt")     ## mvnorm, mvt

dcc.ls<-dccfit(dcc.spec,data=data.ls,solver = "solnp",
               fit.control = list(eval.se=TRUE))
dcc.ls

#####    6. 将Gold and S&P500 数据转化成xts数据   #####
data.gs<-as.data.frame(cbind(time,ret.g,ret.s))
data.gs<-xts(data.gs[,-1],order.by=as.Date(data.gs[,1]))
colnames(data.gs)<-c("Gold","S&P500")

####  DCC-GARCH  Gold with S&P500 #####
uspec<- ugarchspec(mean.model = list(armaOrder=c(0,0)),
                   variance.model=list(model="eGARCH",garchOrder=c(1,1)),
                   distribution.model="sstd")
dcc.spec<-dccspec(uspec=multispec(replicate(2,uspec)),dccOrder=c(1,1),
                  distribution="mvt")     ## mvnorm, mvt

dcc.gs<-dccfit(dcc.spec,data=data.gs,solver = "lbfgs",
               fit.control = list(eval.se=TRUE))
dcc.gs
