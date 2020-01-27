library(quantmod)
library(zoo)
library(xts)
library(TTR)
library(tseries)

## 用quantmod包导入数据
getSymbols("AAPL",from = "2017-01-01",to = Sys.Date(),src = "yahoo")
head(AAPL)

## 用tseries包导入数据  quote = c("Open", "High", "Low", "Close")
goog<-get.hist.quote(instrument = "GOOG", start="2017-01-01", end="2017-07-01",quote = "AdjClose")
head(goog)
# quote = c("Open", "High", "Low", "Close"，"AdjClose")

## 以 chartSeries 为例做 K 线图
chartSeries(AAPL)
chartSeries(AAPL,theme='white')
chartSeries(GOOG,theme='white.mono')

## 中国习惯是涨红跌绿
chartSeries(AAPL,up.col='red', dn.col='green',theme="white")

## 增加参数类型
chartSeries(AAPL,name = "AAPLBARCHART",subset="2017-01-01：：2017-07-17"，type="bars")
chartSeries(AAPL,name = "AAPLLINECHART",subset="2017-01-01：：2017-07-17"，type="line")
chartSeries(AAPL,name = "AAPLCANDCHART",subset="2017-01-01：：2017-07-17"，type="candlesticks")

## addBBands() 布林线指标
chartSeries(AAPL,up.col='red', dn.col='green',theme="white")
addBBands(n=14,sd=2,draw='bands')
#只画区间还有 percent 百分比 width 宽度

## addADX()平均趋向指标
chartSeries(AAPL,up.col='red', dn.col='green',theme="white")
addADX()

## 其他指标
addMACD()  # 指数平滑异同移动平均线
addCCI()   # 测量股价是否超出常态分布范围
addRSI()   # 测量速度和变化的价格变动
addVo()    # 测量成交量
addWPR()   # 表示市场处于超卖还是超买状态
addDPO()   # 排除价格趋势的震荡指标

## 可以把上面介绍的技术合到一张图上
chartSeries(AAPL,up.col='red', dn.col='green',theme="white"，TA=c(addBBands(),addMACD(),addADX(),addVo()))

## 计算简单收益率
close <- AAPL[,4]
close1 <- lag(close,1)
cal.close <- merge(close,close1)

s.return <- (close-close1)/close1
names(s.return)="s.return"
cal.return=merge(cal.close,s.return)
head(cal.return)

# 对数收益率
library(PerformanceAnalytics)

d.return<-periodReturn(close,period="daily",type="log")  # 对数日收益率 or dailyReturn(close)
head(d.return)

w.return <- periodReturn(close,period="weekly",type="log") # 对数周收益率 or weeklyReturn(close)
m.return <- periodReturn(close,period="monthly",type="log") # 对数月收益率 or monthlyReturn(close)
q.return <- periodReturn(close,period="quarterly",type="log") # 对数季收益率quarterlyReturn（close）
y.return <- periodReturn(close,period="yearly",type="log") # 对数年收益率 or yearlyReturn(close)
a.return <- annualReturn(close)                            # calculate annual returns,same to yearlyReturn

periodReturn(close,period='monthly',subset='2010::')   # 计算2010年至现在的月收益率
periodReturn(close,period='monthly',subset='2010')     # 计算2010年的月收益率

## 抓取四家公司的全部股票行情数据

new.environment <- new.env()
getSymbols(c("AAPL", "ORCL", "MSFT", "GOOG"), src = "yahoo", env = new.environment)

str(get("AAPL", env = new.environment))
str(get("ORCL", env = new.environment))
str(get("MSFT", env = new.environment))
str(get("GOOG", env = new.environment))

## 查看 AAPL 涨跌幅超过 2% 的情况
AAPL <- Delt(Cl(get("AAPL", env = new.environment)))
length(AAPL[which(AAPL > 0.02), ])
plot(AAPL[which(AAPL > 0.02), ])

## 查看 MSFT 涨跌幅超过 2% 的情况
MSFT <- Delt(Cl(get("MSFT", env = new.environment)))
length(MSFT[which(MSFT > 0.02), ])
plot(MSFT[which(MSFT > 0.02), ])

periodicity(get("GOOG", env = new.environment))

# 将四家公司股票的每天调整价格整理在一个数据框中
m <- cbind(Ad(get("AAPL", env = new.environment)), Ad(get("ORCL", env = new.environment)), Ad(get("MSFT", env = new.environment)), Ad(get("GOOG", env = new.environment)))

## 分析相关性并绘图
library(psych)
corr.test(as.data.frame(m))














