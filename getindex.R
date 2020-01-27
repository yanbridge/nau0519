library(quantmod)
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
