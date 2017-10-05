#Download data from Yahoo!Finance
install.packages("quantmod")

library(quantmod)
par(family="STSong")
getSymbols("JD",src="yahoo",from = "2014-05-22",
            to = "2015-07-31")     # or to = Sys.Date(),  #  AAPL,GOOG,MSFT
candleChart(JD,multi.col=F,theme="white",TA=NULL) #TA=NULL  multi.col=F
chartSeries(JD,theme="white",TA=NULL)
chartSeries(GOOG,up.col='red',dn.col='green',theme="white",TA=NULL)


##change data to time series for analysis
tsibm<-ts(IBM$IBM.Close)

#没有成交量和调整后的价格（Adjusted）
library(tseries)
SANYI<-get.hist.quote(instrument='600031.ss',start='2012-01-01',end='2017-08-16')
candleChart(SANYI,multi.col=TRUE,theme="white")
chartSeries(SANYI,theme="white")

###################################################
#上海股市数据: 600000.ss, 深圳股市数据：000001.sz
##上证指数^SSEC,000001,ss 深圳成指：399001.sz,沪深300指数：000300.ss
getSymbols("^SSEC",src="yahoo",from = "2014-10-01",
            to = Sys.Date())
chartSeries(SSEC,theme="white",subset="last 5 year")  #TA=NULL  multi.col=F
chartSeries(SSEC,up.col='red',dn.col='green',theme="white")
zj<- getSymbols("601318.SS",auto.assign=FALSE,from="2014-11-17",to=Sys.Date())
chartSeries(wk,theme="white")
zj1<- getSymbols("2318.HK",auto.assign=FALSE,from="2014-11-17",to=Sys.Date())
chartSeries(wk1,theme="white")
rwk<-dailyReturn(wk)
rwk1<-dailyReturn(wk1)

rzj<-dailyReturn(zj)
rzj1<-dailyReturn(zj1)
rzj11<-cbind(rzj,rzj1)
rzj22<-rzj11
rzj22[rzj22==0]<-NA
rzj22<-na.omit(rzj22)
cor(rzj22)



setSymbolLookup(万科=list(name="000002.sz",src="yahoo",from = "2015-11-01",
            to = "2016-03-15"))  
getSymbols("万科")
chartSeries(万科,u.col='red',dn.col='green',theme="white")

setSymbolLookup(招商银行=list(name="600036.ss",src="yahoo",from = "2012-01-01",
            to = "2012-08-31"))  
getSymbols("招商银行")
chartSeries(招商银行,theme="white")
dev.new()  #添加一副新图进行比较
chartSeries(招商银行,up.col='red',dn.col='green',theme="white") #中国式K线图

addSMA(n = 10, on = 1, with.col = Cl, overlay = TRUE, col = "brown") #简单移动平均线,n=5,10,20,30,60,250
addEMA(n = 10, wilder = FALSE, ratio=NULL, on = 1,with.col = Cl, overlay = TRUE, col = "blue")

addWMA()  #加权移动平均线
addMACD() #加MACD线
addBBands()  #布林线
addRSI()     # 相对强弱指标

##求日、月、季度收益率
getSymbols("^SSEC",src="yahoo",from = "2012-01-01",
            to = Sys.Date())
periodReturn(SSEC,period='daily')  #weekly,monthly,quarterly or dailyReturn(SSEC)
plot(periodReturn(SSEC,period='daily'),main="上证指数日收益率走势图")


#获取公司财务数据
getFinancials('CHL')    #中国移动的财务数据
viewFin(CHL.f,"IS","Q")  ###viewFinancials(CHL, type=c(¡¯BS¡¯,¡¯IS¡¯,¡¯CF¡¯), period=c(¡¯A¡¯,¡¯Q¡¯))


#####################################################################

#日本股市数据，
install.packages("RFinanceYJ")

library(RFinanceYJ)
library(quantmod)

sony <- quoteStockTsData('6758.t', since='2010-01-01',date.end='2012-07-31')
names(sony)<-c("Date","Open","High","Low","Close","Volume")
sony<-read.zoo(sony,tz="")
candleChart(sony)


#Download exchange rate data from FRED: http://research.stlouisfed.org/fred2/

library(quantmod)
getSymbols("DEXUSEU",src="FRED",from="2009-01-01",to="2013-03-31") 

#	DTWEXM	 Trade Weighted Exchange Index: Major Currencies	 1973-01-02	 D
#	DEXUSEU	 U.S. / Euro Foreign Exchange Rate	                   1999-01-04	 D		 
#	DEXJPUS	 Japan / U.S. Foreign Exchange Rate	                   1971-01-04	 D	 	 
#	DTWEXB	 Trade Weighted Exchange Index: Broad             	 1995-01-04	 D	 	 
#	DEXCHUS	 China / U.S. Foreign Exchange Rate	                   1981-01-02	 D	 	 
#	DEXCAUS	 Canada / U.S. Foreign Exchange Rate                 	 1971-01-04	 D	 	 
#	DEXUSUK	 U.S. / U.K Foreign Exchange Rate                    	 1971-01-04	 D	 	 
#	DEXUSAL	 U.S. / Australia Foreign Exchange Rate              	 1971-01-04	 D	 	 
#	DEXSZUS	 Switzerland / U.S. Foreign Exchange Rate         	 1971-01-04	 D	 	 
#	DEXBZUS	 Brazil / U.S. Foreign Exchange Rate	             1995-01-02	 D	 	 
#	DEXTAUS	 Taiwan / U.S. Foreign Exchange Rate	             1983-10-03	 D	 	 
#	DEXKOUS	 South Korea / U.S. Foreign Exchange Rate            	 1981-04-13	 D	  
#	DEXMXUS	 Mexico / U.S. Foreign Exchange Rate	             1993-11-08	 D	  
#	DEXINUS	 India / U.S. Foreign Exchange Rate	                   1973-01-02	 D	 	 
#	DTWEXO	 Trade Weighted Exchange Index:                  	 1995-01-04	 D	 	 
#	DEXUSNZ	 U.S. / New Zealand Foreign Exchange Rate	             1971-01-04	 D		 
#	DEXHKUS	 Hong Kong / U.S. Foreign Exchange Rate	             1981-01-02	 D	 	 
#	DEXSDUS	 Sweden / U.S. Foreign Exchange Rate	             1971-01-04	 D	 	 
#	DEXSIUS	 Singapore / U.S. Foreign Exchange Rate          	 1981-01-02	 D	 	 
#	DEXSFUS	 South Africa / U.S. Foreign Exchange Rate          	 1971-01-04	 D	  
#	DEXTHUS	 Thailand / U.S. Foreign Exchange Rate	             1981-01-02	 D	 	 
#	DEXDNUS	 Denmark / U.S. Foreign Exchange Rate	             1971-01-04	 D	 	 
#	DEXMAUS	 Malaysia / U.S. Foreign Exchange Rate	             1971-01-04	 D		 
#	DEXNOUS	 Norway / U.S. Foreign Exchange Rate	             1971-01-04	 D	 	 
#	DEXVZUS	 Venezuela / U.S. Foreign Exchange Rate	             1995-01-02	 D		 
#	DEXSLUS	 Sri Lanka / U.S. Foreign Exchange Rate	             1973-01-02	 D	

##get the data of exchange rate from OANDA:  http://www.oanda.com/
library(quantmod)
getSymbols("USD/RUB",src="oanda",from = "2012-04-01",to="2013-03-31")  

##将下载的数据写入Excel
write.csv(600031.ss.f,file="data.csv")  #ss为保存在R里面的数据，data.csv为Excel数据