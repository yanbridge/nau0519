#######################Download data of 20 stocks #####################

install.packages('quantmod')

library(quantmod) # for getting stock price
library(copula)   # for copula functions
library(MASS)
par(mfrow=c(4,5)) # for plots layout

stocklist = c('AMZN',  'COH',  'EXPE',  'MCD'   ,'NKE'  ,'RL'  ,'CCE', 'PEP',  'AXP',
'AIG',  'BLK','COF','GS','JPM','MCO', 'WFC',  'FDX'  ,'MMM','AAPL','FB')

for (i in stocklist) {
    x = get(getSymbols(i, src = 'yahoo', from = '2015-01-01', adjust=TRUE))
    xr = dailyReturn(x, type = 'log')
    assign(i, xr)
}

dat <- cbind(AMZN,  COH,  EXPE, MCD ,NKE ,RL ,CCE, PEP, AXP,
AIG, BLK,COF,GS,JPM,MCO, WFC, FDX ,MMM,AAPL,FB)
dat <- as.data.frame(dat)
names(dat) <- stocklist


##################### Raw data histogram ############################


for (i in 1:20)
{
    hist(dat[,i], main = stocklist[i])
}

###################### Test of Normality - QQplot #############################

for (i in 1:20){
    qqnorm(dat[,i])
}

####################### Fit Normal###########################################
est_mean <- apply(dat,2, function(x){as.numeric(fitdistr(x,"normal")$estimate)[1]})
est_sd <-  apply(dat,2, function(x){as.numeric(fitdistr(x,"normal")$estimate)[2]})

data1 <- matrix(0,dim(dat)[1],20)

for (i in 1:20){
    data1[,i] <- pnorm(dat[,i], mean=est_mean[i], sd=est_sd[i])
}

########################## Uniform Distribution ###############
for (i in 1:20)
{
    hist(data1[,i], main = paste('Transfomed', stocklist[i], sep= ''))
}


############################ Fit Copula ########################

### Fit normalCopula

fnorm_itau = fitCopula(data=data1,

method = "itau", optim.method="BFGS",

copula=normalCopula(dim=20, dispstr="un"))


########## Construct Copula ########################

mean1 <- apply(dat,2, mean)

sd1 <- apply(dat,2,sd)

mvdc_norm <- mvdc(copula = normalCopula(coef(fnorm_itau),dim=20,dispstr="un"), rep("norm", 20), list(list(mean = mean1[1],sd = sd1[1]),

list(mean = mean1[2], sd=sd1[2]),

list(mean= mean1[3], sd=sd1[3]),

list(mean= mean1[4], sd=sd1[4]),

list(mean= mean1[5], sd=sd1[5]),

list(mean= mean1[6], sd=sd1[6]),

list(mean= mean1[7], sd=sd1[7]),

list(mean= mean1[8], sd=sd1[8]),

list(mean= mean1[9], sd=sd1[9]),

list(mean= mean1[10], sd=sd1[10]),

list(mean= mean1[11], sd=sd1[11]),

list(mean= mean1[12], sd=sd1[12]),

list(mean= mean1[13], sd=sd1[13]),

list(mean= mean1[14], sd=sd1[14]),

list(mean= mean1[15], sd=sd1[15]),

list(mean= mean1[16], sd=sd1[16]),

list(mean= mean1[17], sd=sd1[17]),

list(mean= mean1[18], sd=sd1[18]),

list(mean= mean1[19], sd=sd1[19]),

list(mean= mean1[20], sd=sd1[20])))
######################################## Simulation ################################
set.seed(2015)
rand_mvdc <- rMvdc(n=10000, mvdc=mvdc_norm)

par(mfrow=c(4,5))
for (i in 1:20){
    hist(rand_mvdc[,i])
}

##################################### simulated VaR and ES ##########################
meanRet <- apply(rand_mvdc, 1, mean)
x <- meanRet[order(meanRet)]
# assuem we invest $1000,000
invest = 1000000
VaR_5perc_simulation <- -invest*x[number*0.05]
ES_5prc_simulation <- -invest*(mean(x[1:number*0.05]))

####################### Parametric Estimation of VaR and ES ########################
meanRet <- mean(apply(dat, 2, mean))
sdRet <- sqrt(sum(cov(dat)/400))
invest = 1000000
VaR_para <- invest*(-meanRet+1.645*sdRet)
ES5_para <- invest*(-meanRet+0.103*sdRet/0.05)
