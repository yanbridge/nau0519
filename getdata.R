rm(list=ls())            #删除当前环境中的变量和数据

#Download data from Yahoo!Finance
getwd()                  #获取当前工作路径
setwd("D:/Rdata")        #改变工作路径至D盘的Rdata文件夹

#########  3月28日教学 #################
#Download data from Yahoo!Finance
install.packages("quantmod")     #  如果未安装过此程序包,请运行此命令

library(quantmod)               # 导入quantmod程序包

getSymbols("JD",src="yahoo",from = "2014-05-22",
           to = Sys.Date())     # or to = Sys.Date(),  #  AAPL,GOOG,MSFT

head(JD)
tail(JD)

JD2014<- JD["2014"]            # 提取京东2014年股票交易数据
JD1819<- JD["2018::2019"]      # 提取京东2018-2019年股票交易数据

chartSeries(JD,theme="white",TA=NULL)   # 默认K线风格为欧美式
chartSeries(JD,up.col='red',dn.col='green',
            theme="white",TA=NULL,subset="2020")   #画出京东2020年以来的中国式K线图

## 下载国内股票数据
setSymbolLookup(招商银行=list(name="600036.ss",src="yahoo",from = "1990-01-01",
                          to = Sys.Date()))  
getSymbols("招商银行")
chartSeries(招商银行,up.col='red',dn.col='green',theme="white") #中国式K线图

setSymbolLookup(SH=list(name="000001.ss",src="yahoo",from = "2000-01-01",
                          to = Sys.Date()))     
getSymbols("SH")                # 上海综合指数
chartSeries(SH,up.col='red',dn.col='green',theme="white") #中国式K线图

dreturn.sh <- dailyReturn(SH)
chartSeries(dreturn.sh)

######################### 3月31日 教学 ####################################
library(quantmod)

getSymbols("JD",src="yahoo",from = "2014-05-22",
           to = Sys.Date())     # or to = Sys.Date(),  #  AAPL,GOOG,MSFT

## 提取京东的收盘价

jdclose <- JD$JD.Close  # 第一种方法
jdclose1<- JD[,4]

head(cbind(jdclose,jdclose1))
tail(cbind(jdclose,jdclose1))

chartSeries(jdclose)

## 计算京东股价日内波动幅度：最高价-最低价
jdvol <- JD$JD.High-JD$JD.Low
chartSeries(jdvol)

################# 计算股票收益率 ######################

dreturn.jd <- dailyReturn(JD)

dayreturn.jd <- diff(log(JD[,6]))

par(mfrow=c(1,2))
plot(dreturn.jd,col="blue")
plot(dayreturn.jd,col="red")

chartSeries(dreturn.jd,theme="white")

### 如何计算周收益率,月收益率,季度收益率和年收益率？###
wreturn <- weeklyReturn(JD)
mreturn <- monthlyReturn(JD)
qreturn <- quarterlyReturn(JD)
yreturn <- yearlyReturn(JD)

###############  统计股票收益率小程序  ####################
sumstats<-function(x,na.omit=FALSE){
        if (na.omit)
                x<-x[!is.na(x)]
        m<-mean(x)
        n<-length(x)
        s<-sd(x)
        max<-max(x)
        min<-min(x)
        skew<-sum((x-m)^3/s^3)/n
        kurt<-sum((x-m)^4/s^4)/n-3
        options(scipen=100)
        return(c(obs=n,mean=m,stdev=s,max=max,min=min,skewness=skew,kurtosis=kurt))
}
############### 显示统计数据 ##########################

sumstats(dreturn.jd)

#### 画 Q-Q 图
qqnorm(dreturn.jd,col="blue")
qqline(dreturn.jd,col="red",lwd=3)

### 画直方图加正态分布线
hist(dreturn.jd)
hist(dreturn.jd,probability = T,col="green",main = "京东收益率直方图",ylim=c(0,24))

lines(density(dreturn.jd),col="red",lwd=5)

### 添加正态分布
xfit<-seq(min(dreturn.jd),max(dreturn.jd),length=100)
yfit<-dnorm(xfit,mean(dreturn.jd),sd(dreturn.jd))
lines(xfit,yfit,col="blue",lwd=3,lty=2)

## 保存并再次读取,转化为xts类型数据
write.csv(JD,file="jd.csv")
write.csv(data.frame(JD),file="jd.csv")

jd<-read.csv("jd.csv")
jd<-xts(jd[,2:7],order.by = as.Date(jd[,1]))

#通过tseries程序包也可以下载股票数据
install.packages("tseries")

library(tseries)
ZSbank<-get.hist.quote(instrument="600036.ss",start="2001-01-01",end=Sys.Date(),
                       quote=c("Open","High","Low","Close","Volume","Adjusted"))
chartSeries(ZSbank,up.col="red",dn.col="green",theme="white")
chartSeries(ZSbank,up.col="red",dn.col="green",theme="white",subset="2019")
chartSeries(ZSbank,up.col="red",dn.col="green",theme="white",subset="2019::2020")
chartSeries(ZSbank,up.col="red",dn.col="green",theme="white",subset="2019-06::2019-08")
chartSeries(ZSbank,up.col="red",dn.col="green",theme="white",subset="2019-06-10::2019-08-20")

## 可以根据需要,选择下载数据,如只需要Close
ZSbank1<-get.hist.quote(instrument="600036.ss",start="2012-01-01",end=Sys.Date(),
                       quote="Adjusted")

chartSeries(ZSbank1,up.col="red",dn.col="green",theme="white")

##将下载的数据写入Excel
write.csv(ZSbank,file="ZSbank.csv")   # 保存后没有时间
write.csv(JD,file="JD.csv") 

write.csv(data.frame(ZSbank),file="ZSbank1.csv")  # 有时间

############# 常见指数代码 ############################
#上海股市数据: 600000.ss, 深圳股市数据：000001.sz
##上证指数：000001.ss 深证成指：399001.sz,沪深300指数代码：000300.ss
##道琼斯：^DJI, 标准普尔：^GSPC, 纳斯达克：^IXIC
##英国：^FTSE, 法国：^FCHI 德国：^GDAXI 日本：^N225
## 恐慌指数 ^VIX  十年国债收益率 ^TNX

getSymbols("^DJI")

setSymbolLookup(贵州茅台=list(name="600036.ss",src="yahoo",from = "2012-01-01",
                          to = "2012-08-31"))  
getSymbols("贵州茅台")
chartSeries(贵州茅台,theme="white")
dev.new()  #添加一副新图利于比较
chartSeries(招商银行,up.col='red',dn.col='green',theme="white") #中国式K线图

##获取分红信息
getDividends("600036.ss",  from = "1990-01-01", to = Sys.Date())   # 招商银行
getDividends("600519.ss",  from = "1990-01-01", to = Sys.Date())   # 贵州茅台

#####################  FRED  #######################################

#Download exchange rate data from FRED: https://research.stlouisfed.org/fred2/

library(quantmod)
getSymbols("DTWEXM",src="FRED",from="2009-01-01",to="2020-03-31") 

#	DTWEXM	 Trade Weighted Exchange Index: Major Currencies	          1973-01-02	 D
#	DEXUSEU	 U.S. / Euro Foreign Exchange Rate	                     1999-01-04	 D		 
#	DEXJPUS	 Japan / U.S. Foreign Exchange Rate	                     1971-01-04	 D	 	 
#	DTWEXB	 Trade Weighted Exchange Index: Broad               	          1995-01-04	 D	 	 
#	DEXCHUS	 China / U.S. Foreign Exchange Rate	                     1981-01-02	 D	 	 
#	DEXCAUS	 Canada / U.S. Foreign Exchange Rate                 	          1971-01-04	 D	 	 
#	DEXUSUK	 U.S. / U.K Foreign Exchange Rate                    	          1971-01-04	 D	 	 
#	DEXUSAL	 U.S. / Australia Foreign Exchange Rate              	          1971-01-04	 D	 	 
#	DEXSZUS	 Switzerland / U.S. Foreign Exchange Rate         	          1971-01-04	 D	 	 
#	DEXBZUS	 Brazil / U.S. Foreign Exchange Rate	                     1995-01-02	 D	 	 
#	DEXTAUS	 Taiwan / U.S. Foreign Exchange Rate	                     1983-10-03	 D	 	 
#	DEXKOUS	 South Korea / U.S. Foreign Exchange Rate            	          1981-04-13	 D	  
#	DEXMXUS	 Mexico / U.S. Foreign Exchange Rate	                     1993-11-08	 D	  
#	DEXINUS	 India / U.S. Foreign Exchange Rate	                     1973-01-02	 D	 	 
#	DTWEXO	 Trade Weighted Exchange Index:                  	          1995-01-04	 D	 	 
#	DEXUSNZ	 U.S. / New Zealand Foreign Exchange Rate	                     1971-01-04	 D		 
#	DEXHKUS	 Hong Kong / U.S. Foreign Exchange Rate	                     1981-01-02	 D	 	 
#	DEXSDUS	 Sweden / U.S. Foreign Exchange Rate	                     1971-01-04	 D	 	 
#	DEXSIUS	 Singapore / U.S. Foreign Exchange Rate                          1981-01-02	 D	 	 
#	DEXSFUS	 South Africa / U.S. Foreign Exchange Rate          	          1971-01-04	 D	  
#	DEXTHUS	 Thailand / U.S. Foreign Exchange Rate	                     1981-01-02	 D	 	 
#	DEXDNUS	 Denmark / U.S. Foreign Exchange Rate	                     1971-01-04	 D	 	 
#	DEXMAUS	 Malaysia / U.S. Foreign Exchange Rate	                     1971-01-04	 D		 
#	DEXNOUS	 Norway / U.S. Foreign Exchange Rate	                     1971-01-04	 D	 	 
#	DEXVZUS	 Venezuela / U.S. Foreign Exchange Rate	                     1995-01-02	 D		 
#	DEXSLUS	 Sri Lanka / U.S. Foreign Exchange Rate	                     1973-01-02	 D	

##################### 4月7日教学 ################################################
library(quantmod)
library(timeSeries)   # 如果没有安装，请 run:  install.packages("timeSeries")
library(fPortfolio)   # 如果没有安装，请 run:  install.packages("fPortfolio")
 
getSymbols("^GSPC",from="2015-01-01")
getSymbols("AAPL",from="2015-01-01")

setSymbolLookup(SH=list(name="000001.ss",src="yahoo",from = "2015-01-01"))  
getSymbols("SH")       # 上海综合指数

setSymbolLookup(MT=list(name="600519.ss",src="yahoo",from = "2015-01-01"))  
getSymbols("MT")       # 贵州茅台

addSMA(n = 10, on = 1, with.col = Cl, overlay = TRUE, col = "brown") #简单移动平均线,n=5,10,20,30,60,250
addEMA(n = 10, wilder = FALSE, ratio=NULL, on = 1,with.col = Cl, overlay = TRUE, col = "blue")

addWMA()      # 加权移动平均线
addMACD()     # 加MACD线
addBBands()   # 布林线指标
addRSI()      # 相对强弱RSI指标

# 下载美国6只股票数据
getSymbols(c("AAPL","AMZN","DIS","MSFT","GS","NKE"),from="2015-01-01")

# 计算6只股票收益率
AMZN_ret <- dailyReturn(AMZN)
AAPL_ret <- dailyReturn(AAPL)
DIS_ret <- dailyReturn(DIS)
MSFT_ret <- dailyReturn(MSFT)
GS_ret <- dailyReturn(GS)
NKE_ret <- dailyReturn(NKE)

# 合并6只股票收益率
data_ret <- merge(AAPL_ret,AMZN_ret,DIS_ret,MSFT_ret,GS_ret,NKE_ret)
colnames(data_ret)<-c("AAPL","AMZN","DIS","MSFT","GS","NKE")
data_ret<-as.timeSeries(data_ret)

# 6只股票有效前沿
frontier_us<-portfolioFrontier(data_ret)
frontier_us
plot(frontier_us)

###### 中国股市的有效前沿模拟

setSymbolLookup(ZS=list(name="600036.ss",src="yahoo",from = "2015-01-01",
                          to = Sys.time()))    ## 招商银行
getSymbols("ZS")

setSymbolLookup(ZX=list(name="600030.ss",src="yahoo",from = "2015-01-01",
                          to = Sys.time()))    ## 中信证券
getSymbols("ZX")

setSymbolLookup(PA=list(name="601318.ss",src="yahoo",from = "2015-01-01",
                          to = Sys.time()))    ## 中国平安
getSymbols("PA")

setSymbolLookup(MT=list(name="600519.ss",src="yahoo",from = "2015-01-01",
                            to = Sys.time()))   ## 贵州茅台
getSymbols("MT")

setSymbolLookup(HR=list(name="600276.ss",src="yahoo",from = "2015-01-01",
                        to = Sys.time()))      ## 恒瑞医药
getSymbols("HR")

setSymbolLookup(XF=list(name="002230.sz",src="yahoo",from = "2015-01-01",
                        to = Sys.time()))     ## 科大讯飞
getSymbols("XF")

setSymbolLookup(GL=list(name="000651.sz",src="yahoo",from = "2015-01-01",
                        to = Sys.time()))     ## 格力电器
getSymbols("GL")

# 计算7只股票收益率
ZS_ret <- dailyReturn(ZS)
ZX_ret <- dailyReturn(ZX)
PA_ret <- dailyReturn(PA)
MT_ret <- dailyReturn(MT)
HR_ret <- dailyReturn(HR)
XF_ret <- dailyReturn(XF)
GL_ret <- dailyReturn(GL)

data.ret <- merge(ZS_ret,ZX_ret,PA_ret,MT_ret,HR_ret,XF_ret,GL_ret)
colnames(data.ret)<-c("ZS","ZX","PA","MT","HR","XF","GL")
data.ret<-as.timeSeries(data.ret)

# 有效前沿
data.ret<-as.timeSeries(data.ret)
data.ret<-as.timeSeries(data.ret)
frontier_ch<-portfolioFrontier(data.ret)
frontier_ch
plot(frontier_ch)

## 重新调整组合
options(scipen = 5)
options(digits = 3)
mean_ret<-apply(data.ret,2,mean)
sd_ret<-apply(data.ret,2,sd)
cbind(mean_ret,sd_ret)  # 去掉中信证券

data.ret1 <- merge(ZS_ret,PA_ret,MT_ret,HR_ret,XF_ret,GL_ret)
colnames(data.ret1)<-c("ZS","PA","MT","HR","XF","GL")
data.ret1<-as.timeSeries(data.ret1)
frontier_ch1<-portfolioFrontier(data.ret1)
frontier_ch1
plot(frontier_ch1)

## 再调整组合
cbind(mean_ret,sd_ret)   ## 科大讯飞标准差大，去掉

data.ret2 <- merge(ZS_ret,PA_ret,MT_ret,HR_ret,GL_ret)
colnames(data.ret2)<-c("ZS","PA","MT","HR","GL")
data.ret2<-as.timeSeries(data.ret2)
frontier_ch2<-portfolioFrontier(data.ret2)
frontier_ch2
plot(frontier_ch2)

## 再调整一次
cbind(mean_ret,sd_ret)  # 去掉格力电器

data.ret3 <- merge(ZS_ret,PA_ret,MT_ret,HR_ret)
colnames(data.ret3)<-c("ZS","PA","MT","HR")
data.ret3<-as.timeSeries(data.ret3)
frontier_ch3<-portfolioFrontier(data.ret3)
frontier_ch3
plot(frontier_ch3)

##################### 4月14日 教学 #########################
library(quantmod)

getSymbols(c("AAPL","AMZN","DIS","MSFT","GS","NKE"),from="2015-01-01")

# 计算6只股票收益率
AMZN_ret <- dailyReturn(AMZN)
AAPL_ret <- dailyReturn(AAPL)
DIS_ret <- dailyReturn(DIS)
MSFT_ret <- dailyReturn(MSFT)
GS_ret <- dailyReturn(GS)
NKE_ret <- dailyReturn(NKE)

# 合并6只股票收益率
data_ret <- merge(AAPL_ret,AMZN_ret,DIS_ret,MSFT_ret,GS_ret,NKE_ret)
colnames(data_ret)<-c("AAPL","AMZN","DIS","MSFT","GS","NKE")

## 查看收益率和标准差
options(scipen = 5)
options(digits = 3)
mean_ret<-apply(data_ret,2,mean)*100
sd_ret<-apply(data_ret,2,sd)*100
cbind(mean_ret,sd_ret) 

plot(sd_ret,mean_ret,xlim = c(1,2.1),ylim = c(-0.01,0.21),pch=1:6,col=2:7)
text(1.79,0.09,"Apple")
text(1.90,0.165,"Amazon")
text(1.56,0.025,"Disney")
text(1.73,0.12,"Microsoft")
text(1.83,0.01,"Goldman Sachs")
text(1.7,0.065,"Nike")

##########  单指数模型
setSymbolLookup(SH=list(name="000001.ss",src="yahoo",from = "2015-01-01",
                        to = Sys.time()))    ## 上海综合指数
getSymbols("SH")
setSymbolLookup(ZS=list(name="600036.ss",src="yahoo",from = "2015-01-01",
                        to = Sys.time()))    ## 招商银行
getSymbols("ZS")

setSymbolLookup(MT=list(name="600519.ss",src="yahoo",from = "2015-01-01",
                        to = Sys.time()))   ## 贵州茅台
getSymbols("MT")
setSymbolLookup(XF=list(name="002230.sz",src="yahoo",from = "2015-01-01",
                        to = Sys.time()))     ## 科大讯飞
getSymbols("XF")
setSymbolLookup(GL=list(name="000651.sz",src="yahoo",from = "2015-01-01",
                        to = Sys.time()))     ## 格力电器
getSymbols("GL")
setSymbolLookup(BD=list(name="600130.ss",src="yahoo",from = "2015-01-01",
                        to = Sys.time()))     ## 波导股份
getSymbols("BD")

SH_ret <- dailyReturn(SH)*100
ZS_ret <- dailyReturn(ZS)*100
MT_ret <- dailyReturn(MT)*100
XF_ret <- dailyReturn(XF)*100
GL_ret <- dailyReturn(GL)*100
BD_ret <- dailyReturn(BD)*100

ZSdata <- merge(ZS_ret,SH_ret)
MTdata <- merge(MT_ret,SH_ret)
XFdata <- merge(XF_ret,SH_ret)
GLdata <- merge(GL_ret,SH_ret)
BDdata <- merge(BD_ret,SH_ret)

ZSdata[ZSdata==0] <- NA
MTdata[MTdata==0] <- NA
XFdata[XFdata==0] <- NA
GLdata[GLdata==0] <- NA
BDdata[BDdata==0] <- NA

ZSdata <- na.omit(ZSdata)
MTdata <- na.omit(MTdata)
XFdata <- na.omit(XFdata)
GLdata <- na.omit(GLdata)
BDdata <- na.omit(BDdata)

######################## 4月18日教学 ################################
library(quantmod)

setSymbolLookup(SH=list(name="000001.ss",src="yahoo",from = "2015-01-01",
                        to = Sys.time()))    ## 上海综合指数
getSymbols("SH")
setSymbolLookup(ZS=list(name="600036.ss",src="yahoo",from = "2015-01-01",
                        to = Sys.time()))    ## 招商银行
getSymbols("ZS")
setSymbolLookup(MT=list(name="600519.ss",src="yahoo",from = "2015-01-01",
                        to = Sys.time()))   ## 贵州茅台
getSymbols("MT")
setSymbolLookup(XF=list(name="002230.sz",src="yahoo",from = "2015-01-01",
                        to = Sys.time()))     ## 科大讯飞
getSymbols("XF")
setSymbolLookup(GL=list(name="000651.sz",src="yahoo",from = "2015-01-01",
                        to = Sys.time()))     ## 格力电器
getSymbols("GL")
setSymbolLookup(BD=list(name="600130.ss",src="yahoo",from = "2015-01-01",
                        to = Sys.time()))     ## 波导股份
getSymbols("BD")
setSymbolLookup(ZY=list(name="603986.ss",src="yahoo",from = "2015-01-01",
                        to = Sys.time()))     ## 兆易创新
getSymbols("ZY")

SH_ret <- dailyReturn(SH)*100
ZS_ret <- dailyReturn(ZS)*100
MT_ret <- dailyReturn(MT)*100
XF_ret <- dailyReturn(XF)*100
GL_ret <- dailyReturn(GL)*100
BD_ret <- dailyReturn(BD)*100
ZY_ret <- dailyReturn(ZY)*100

ZSdata <- merge(ZS_ret,SH_ret)
MTdata <- merge(MT_ret,SH_ret)
XFdata <- merge(XF_ret,SH_ret)
GLdata <- merge(GL_ret,SH_ret)
BDdata <- merge(BD_ret,SH_ret)
ZYdata <- merge(ZY_ret,SH_ret)

ZSdata[ZSdata==0] <- NA
MTdata[MTdata==0] <- NA
XFdata[XFdata==0] <- NA
GLdata[GLdata==0] <- NA
BDdata[BDdata==0] <- NA
ZYdata[ZYdata==0] <- NA

ZSdata <- na.omit(ZSdata)
MTdata <- na.omit(MTdata)
XFdata <- na.omit(XFdata)
GLdata <- na.omit(GLdata)
BDdata <- na.omit(BDdata)
ZYdata <- na.omit(ZYdata)

####### 贵州茅台单指数模型
mtmodel <- lm(MTdata[,1]~MTdata[,2])
summary(mtmodel)

mt<-as.vector(MTdata[,1])
index_mt<-as.vector(MTdata[,2])
par(family="STKaiti")    # just for MacOS
plot(index_mt,mt,col=4,main="贵州茅台单指数模型")
abline(mtmodel,col=2,lwd=3)
abline(h=0,lty=2)
abline(v=0,lty=2)
Sys.time()
print("你的班级+名字")

### 如何利用周（月）数据构建单指数模型呢？
## 仿照贵州茅台的命令，做出科大讯飞的单指数图
SH_w <- weeklyReturn(SH)*100
XF_w <- weeklyReturn(XF)*100

length(SH_w);length(XF_w)  # 查看数据长度
xfmodel <- lm(XF_w ~ SH_w)
summary(xfmodel)

xf<-as.vector(XF_w)
index_xf<-as.vector(SH_w)

plot(index_xf,xf,col="green",main="科大讯飞单指数模型（周）")
abline(xfmodel,col="red",lwd=3)
abline(h=0,lty=2)
abline(v=0,lty=2)



########### VaR（Value at Risk）值计算 ############

##### 招商银行
## ① delta method：方差-协方差法
mu_zs <- mean(ZS_ret)
sigma_zs <- sd(ZS_ret)
VaR1.zs_95 <- mu_zs-qnorm(0.05)*sigma_zs ;VaR1.zs_95   # 95%置信水平
VaR1.zs_975 <- mu_zs-qnorm(0.025)*sigma_zs ;VaR1.zs_975   # 95%置信水平
VaR1.zs_99 <- mu_zs-qnorm(0.01)*sigma_zs ;VaR1.zs_99  # 99%置信水平

hist(ZS_ret,col="lightblue",probability = T,ylim=c(0,0.3),main = "招商银行收益直方图")
lines(density(ZS_ret),col="red",lwd=5)

### 添加正态分布
xfit<-seq(min(ZS_ret),max(ZS_ret),length=100)
yfit<-dnorm(xfit,mean(ZS_ret),sd(ZS_ret))
lines(xfit,yfit,col="blue",lwd=3,lty=2)

## 画出VaR位置
abline(v=-VaR1.zs_95,col="green",lwd=3)   # for long position
abline(v= VaR1.zs_95,col="green",lwd=3)   # for short position

abline(v= -VaR1.zs_99,col=6,lwd=3)
abline(v=  VaR1.zs_99,col=6,lwd=3)

### ② 历史模拟法
VaR2.zs_95 <- -quantile(ZS_ret, 0.05) ; VaR2.zs_95
VaR2.zs_99 <- -quantile(ZS_ret, 0.01) ; VaR2.zs_99

### ③ 蒙特卡洛模拟法
MC_zs<-rnorm(100000,mu_zs,sigma_zs)    # 假设服从正态分布
VaR3.zs_95 <- -quantile(MC_zs, 0.05) ; VaR3.zs_95  
VaR3.zs_99 <- -quantile(MC_zs, 0.01) ; VaR3.zs_99 

## 比较三种方法的计算结果
cbind(VaR1.zs_95,VaR2.zs_95,VaR3.zs_95)  # 95%置信水平
cbind(VaR1.zs_99,VaR2.zs_99,VaR3.zs_99)  # 99%置信水平

### 计算兆易创新的VaR
mu_zy <- mean(ZY_ret)
sigma_zy <- sd(ZY_ret)
VaR1.zy_95 <- mu_zy-qnorm(0.05)*sigma_zy ;VaR1.zy_95   # 95%置信水平
VaR1.zy_99 <- mu_zy-qnorm(0.01)*sigma_zy ;VaR1.zy_99   # 99%置信水平

hist(ZY_ret,col="lightblue",breaks = 20,probability = T,ylim=c(0,0.2),main = "兆易创新收益直方图")
lines(density(ZY_ret),col="red",lwd=3)

### 添加正态分布
xfit<-seq(min(ZY_ret),max(ZY_ret),length=100)
yfit<-dnorm(xfit,mean(ZY_ret),sd(ZY_ret))
lines(xfit,yfit,col="blue",lwd=3,lty=2)

abline(v=-VaR1.zy_95,col="green",lwd=3)
abline(v= VaR1.zy_95,col="green",lwd=3)

abline(v= -VaR1.zy_99,col=6,lwd=3)
abline(v=  VaR1.zy_99,col=6,lwd=3)

### ② 历史模拟法
VaR2.zy_95 <- -quantile(ZY_ret, 0.05) ; VaR2.zy_95
VaR2.zy_95 <- -quantile(ZY_ret, 0.01) ; VaR2.zy_95

### ③ 蒙特卡洛模拟法
MC_zs<-rnorm(100000,mu_zy,sigma_zy)    # 假设服从正态分布
VaR3.zy_95 <- -quantile(MC_zy, 0.05) ; VaR3.zy_95  
VaR3.zy_99 <- -quantile(MC_zy, 0.01) ; VaR3.zy_99 

## 比较三种方法的计算结果
cbind(VaR1.zy_95,VaR2.zy_95,VaR3.zy_95)  # 95%置信水平
cbind(VaR1.zy_99,VaR2.zy_99,VaR3.zy_99)  # 99%置信水平

## 模拟股票价格
S0=50;      #initial price S0
mu=0.04;    #drift mu
sigma=0.25;  # volatility sigma
T=1;        #1 year
NSteps=252; 
NReps=10000; 
SPaths = matrix(NA,NReps,NSteps+1);
SPaths[,1] = S0;
dt = T/NSteps
nudt = (mu-0.5*sigma^2)*dt
sidt = sigma*sqrt(dt)
x=seq(1:(NSteps+1))
colno=c(1,2,3,4,5,6,7,8,9,10)
for (i in 1:NReps){
  for (j in 1:NSteps){
    SPaths[i,j+1] = SPaths[i,j]*exp(nudt + sidt*rnorm(1))
  }
  if(i == 1) plot(x,SPaths[i,],"l", ylim=c(20,150),col =sample(colno,1))
  if (i != 1) lines(x,SPaths[i,],ylim=c(20,150),col =sample(colno,1))
 }


################### 4月21日教学 #####################
library(quantmod)
library(rugarch)
library(tseries)

load("stock7.RData")   

SH_ret <- dailyReturn(SH)*100
ZS_ret <- dailyReturn(ZS)*100
MT_ret <- dailyReturn(MT)*100
XF_ret <- dailyReturn(XF)*100
GL_ret <- dailyReturn(GL)*100
BD_ret <- dailyReturn(BD)*100
ZY_ret <- dailyReturn(ZY)*100

## 自相关图
acf(ZS_ret)
acf(MT_ret)
acf(ZY_ret)

## 偏相关图
pacf(ZS_ret)
pacf(MT_ret)
pacf(ZY_ret)

## 单位根检验
adf.test(ZS_ret)
adf.test(MT_ret)

## ARCH效应检验
## FinTS程序包里有ArchTest函数，可以先安装FinTS包。
## 如果不想安装FinTS，可运行下面的小程序
ArchTest<-function (x, lags = 12, demean = FALSE) 
{
        xName <- deparse(substitute(x))
        x <- as.vector(x)
        if (demean) 
                x <- scale(x, center = TRUE, scale = FALSE)
        lags <- lags + 1
        mat <- stats::embed(x^2, lags)
        arch.lm <- summary(stats::lm(mat[, 1] ~ mat[, -1]))
        STATISTIC <- arch.lm$r.squared * length(stats::resid(arch.lm))
        names(STATISTIC) <- "Chi-squared"
        PARAMETER <- lags - 1
        names(PARAMETER) <- "df"
        PVAL <- stats::pchisq(STATISTIC, df = PARAMETER, lower.tail = FALSE)
        METHOD <- paste("ARCH LM-test; ", "Null hypothesis:  no ARCH effects")
        result <- list(statistic = STATISTIC, parameter = PARAMETER, 
                       p.value = PVAL, method = METHOD, data.name = xName)
        class(result) <- "htest"
        return(result)
}

ArchTest(MT_ret)      # 贵州茅台的ARCH效应检验



#### GARCH model
spec = ugarchspec(mean.model = list(armaOrder = c(0,0)),
                  variance.model = list(garchOrder = c(1,1),
                                        model = "sGARCH"), distribution.model = "snorm")    
#distribution:snorm,std,sstd,ged,sged,nig,ghyp,sju
#model = eGARCH,fGARCH,gjrGARCH,apARCH,iGARCH and csGARCH
#if model = "fGARCH", then submodel can change to TGARCH, AVGARCH, #NGARCH,APARCH,GJRGARCH and ALLGARCH  
fit_mt <- ugarchfit(data = MT_ret, spec = spec)
show(fit_mt)

### 计算动态VaR并画出图
sigma_mt <- sigma(fit_mt)
mu_mt <- mean(MT_ret)
VaR_mt <- mu_mt-qnorm(0.05)*sigma_mt
plot(MT_ret,col="blue",lwd=1)
lines(-VaR_mt,col="red",lwd=3)
lines(VaR_mt,col="purple",lwd=3)

## 回测检验
T<-length(MT_ret)
Number1<--VaR_mt-MT_ret
N1<-length(Number1[Number1>0])  # 对于多头
N1/T

Number2<-VaR_mt-MT_ret
N2<-length(Number2[Number2<0])  # 对于空头
N2/T
