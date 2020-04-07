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

## 下载中国股票数据
setSymbolLookup(招商银行=list(name="600036.ss",src="yahoo",from = "1990-01-01",
                          to = Sys.Date()))  
getSymbols("招商银行")
chartSeries(招商银行,up.col='red',dn.col='green',theme="white") #中国式K线图

setSymbolLookup(SH=list(name="000001.ss",src="yahoo",from = "2000-01-01",
                          to = Sys.Date()))  
getSymbols("SH")
chartSeries(WK,up.col='red',dn.col='green',theme="white") #中国式K线图

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

write.csv(data.frame(ZSbank),file="ZSbank1.csv")

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
getSymbols("SH")

setSymbolLookup(MT=list(name="600519.ss",src="yahoo",from = "2015-01-01"))  
getSymbols("MT")

addSMA(n = 10, on = 1, with.col = Cl, overlay = TRUE, col = "brown") #简单移动平均线,n=5,10,20,30,60,250
addEMA(n = 10, wilder = FALSE, ratio=NULL, on = 1,with.col = Cl, overlay = TRUE, col = "blue")

addWMA()      # 加权移动平均线
addMACD()     # 加MACD线
addBBands()   # 布林线指标
addRSI()      # 相对强弱RSI指标

# 下载6只股票数据
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
setSymbolLookup(Shindex=list(name="000001.ss",src="yahoo",from = "2015-01-01",
                        to = Sys.time()))    ## 上海综合指数
getSymbols("Shindex")
setSymbolLookup(ZS=list(name="600036.ss",src="yahoo",from = "2015-01-01",
                        to = Sys.time()))    ## 招商银行
getSymbols("ZS")

setSymbolLookup(MT=list(name="600519.ss",src="yahoo",from = "2015-01-01",
                        to = Sys.time()))   ## 贵州茅台
getSymbols("MT")
setSymbolLookup(XF=list(name="002230.sz",src="yahoo",from = "2015-01-01",
                        to = Sys.time()))     ## 科大讯飞
getSymbols("XF")
