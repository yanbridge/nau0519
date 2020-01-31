library(tseries)
library(quantmod)

## 日经225指数
n225 <- get.hist.quote(instrument = "^N225", start = "1999-12-31",
                                     end = "2019-12-31",quote="Close")
## 标准普尔500指数
sp<- get.hist.quote(instrument = "^GSPC", start = "1999-12-31",
                                        end = "2019-12-31",quote="Close")
## 英国富时100指数
ft<- get.hist.quote(instrument = "^FTSE", start = "1999-12-31",
                               end = "2019-12-31",quote="Close")
## 德国DAX指数
dax<- get.hist.quote(instrument = "^GDAXI", start = "1999-12-31",
                    end = "2019-12-31",quote="Close")
## 中国上海综合指数    
sh<-get.hist.quote(instrument='000001.ss',start = "1999-12-31",
                        end = "2019-12-31",quote="Close")

## 计算收益率
ret_n225<-dailyReturn(n225)
ret_ft<-dailyReturn(ft)
ret.dax<-dailyReturn(dax)
ret_sh<-dailyReturn(sh)
ret_sp<-dailyReturn(sp)

rn225<-diff(log(n225))
rft<-diff(log(ft))
rdax<-diff(log(dax))
rsh<-diff(log(sh))
rsp<-diff(log(sp))

data<-merge(rft,rdax,rn225,rsh,rsp)
data<-na.omit(data)

## download data by quantmod
getSymbols("^N225",from="1999-12-31",to="2019-12-31")
getSymbols("^FTSE",from="1999-12-31",to="2019-12-31")
getSymbols("^GDAXI",from="1999-12-31",to="2019-12-31")
getSymbols("^GSPC",from="1999-12-31",to="2019-12-31")
getSymbols("000001.SS",from="1999-12-31",to="2019-12-31")
sh<-`000001.SS`

## 计算收益率
ret_n225<-dailyReturn(N225)
ret_ft<-dailyReturn(FTSE)
ret_dax<-dailyReturn(GDAXI)
ret_sh<-dailyReturn(sh)
ret_sp<-dailyReturn(GSPC)

data<-merge(ret_ft,ret_dax,ret_n225,ret_sh,ret_sp)
colnames(data)<-c("ft","dax","n225","sh","sp")
data[data==0]<-NA
data<-na.omit(data)

#####其他国家指数代码
# ^DJI   道琼斯
# ^IXIC  纳斯达克
# ^RUT   罗素2000 Russell 200
# ^VIX   恐慌指数
# ^FTCHI 巴黎CAC40
# ^HSI   香港恒生指数
# ^STI   新加坡海峡时报指数
# ^KLSE  雅加达综合指数
# ^KS11  韩国指数
