library(quantmod)
library(tseries)

pfyh<-get.hist.quote(instrument='600000.ss',start='2016-01-01',end='2017-09-29',quote="Adjusted")
colnames(pfyh)<-c("pfyh")
rpfyh<-mean(diff(log(pfyh)))*sqrt(252)
spfyh<-sd(diff(log(pfyh)))*sqrt(252)
dpfyh<-c(rpfyh,spfyh)
dpfyh

zxzq<-get.hist.quote(instrument='600030.ss',start='2016-01-01',end='2017-09-29',quote="Adjusted")
colnames(zxzq)<-c("pfyh")
rzxzq<-mean(diff(log(zxzq)))*sqrt(252)
szxzq<-sd(diff(log(zxzq)))*sqrt(252)
dzxzq<-c(rzxzq,szxzq)
dzxzq

gsyh<-get.hist.quote(instrument='601398.ss',start='2016-01-01',end='2017-09-29',quote="Adjusted")
colnames(gsyh)<-c("gsyh")
rgsyh<-mean(diff(log(gsyh)))*sqrt(252)
sgsyh<-sd(diff(log(gsyh)))*sqrt(252)
dgsyh<-c(rgsyh,sgsyh)
dgsyh

gzmt<-get.hist.quote(instrument='600519.ss',start='2016-01-01',end='2017-09-29',quote="Adjusted")
colnames(gzmt)<-c("gzmt")
rgzmt<-mean(diff(log(gzmt)))*sqrt(252)
sgzmt<-sd(diff(log(gzmt)))*sqrt(252)
dgzmt<-c(rgzmt,sgzmt)
dgzmt

zsyh<-get.hist.quote(instrument='600036.ss',start='2016-01-01',end='2017-09-29',quote="Adjusted")
colnames(zsyh)<-c("zsyh")
rzsyh<-mean(diff(log(zsyh)))*sqrt(252)
szsyh<-sd(diff(log(zsyh)))*sqrt(252)
dzsyh<-c(rzsyh,szsyh)
dzsyh

drjt<-get.hist.quote(instrument='600718.ss',start='2016-01-01',end='2017-09-29',quote="Adjusted")
colnames(drjt)<-c("drjt")
rdrjt<-mean(diff(log(drjt)))*sqrt(252)
sdrjt<-sd(diff(log(drjt)))*sqrt(252)
ddrjt<-c(rdrjt,sdrjt)
ddrjt

bgjt<-get.hist.quote(instrument='600019.ss',start='2016-01-01',end='2017-09-29',quote="Adjusted")
colnames(bgjt)<-c("bgjt")
rbgjt<-mean(diff(log(na.omit(bgjt))))*sqrt(252)
sbgjt<-sd(diff(log(na.omit(bgjt))))*sqrt(252)
dbgjt<-c(rbgjt,sbgjt)
dbgjt

wk<-get.hist.quote(instrument='000002.sz',start='2016-01-01',end='2017-09-29',quote="Adjusted")
colnames(wk)<-c("wk")
rwk<-mean(diff(log(wk)))*sqrt(252)
swk<-sd(diff(log(wk)))*sqrt(252)
dwk<-c(rwk,swk)
dwk

hxdq<-get.hist.quote(instrument='600060.ss',start='2016-01-01',end='2017-09-29',quote="Adjusted")
colnames(hxdq)<-c("hxdq")
rhxdq<-mean(diff(log(na.omit(hxdq))))*sqrt(252)
shxdq<-sd(diff(log(na.omit(hxdq))))*sqrt(252)
dhxdq<-c(rhxdq,shxdq)
dhxdq

gldq<-get.hist.quote(instrument='000651.sz',start='2016-01-01',end='2017-09-29',quote="Adjusted")
colnames(gldq)<-c("gldq")
rgldq<-mean(diff(log(na.omit(gldq))))*sqrt(252)
sgldq<-sd(diff(log(na.omit(gldq))))*sqrt(252)
dgldq<-c(rgldq,sgldq)
dgldq

szty<-get.hist.quote(instrument='300002.sz',start='2016-01-01',end='2017-09-29',quote="Adjusted")
colnames(szty)<-c("szty")
rszty<-mean(diff(log(szty)))*sqrt(252)
sszty<-sd(diff(log(szty)))*sqrt(252)
dszty<-c(rszty,sszty)
dszty

xzb<-get.hist.quote(instrument='300037.sz',start='2016-01-01',end='2017-09-29',quote="Adjusted")
colnames(xzb)<-c("xzb")
rxzb<-mean(diff(log(xzb)))*sqrt(252)
sxzb<-sd(diff(log(xzb)))*sqrt(252)
dxzb<-c(rxzb,sxzb)
dxzb

drzb<-get.hist.quote(instrument='300183.sz',start='2016-01-01',end='2017-09-29',quote="Adjusted")
colnames(drzb)<-c("drzb")
rdrzb<-mean(diff(log(drzb)))*sqrt(252)
sdrzb<-sd(diff(log(drzb)))*sqrt(252)
ddrzb<-c(rdrzb,sdrzb)
ddrzb

bjjz<-get.hist.quote(instrument='300223.sz',start='2016-01-01',end='2017-09-29',quote="Adjusted")
colnames(bjjz)<-c("bjjz")
rbjjz<-mean(diff(log(bjjz)))*sqrt(252)
sbjjz<-sd(diff(log(bjjz)))*sqrt(252)
dbjjz<-c(rbjjz,sbjjz)
dbjjz

dfyh<-get.hist.quote(instrument='002271.sz',start='2016-01-01',end='2017-09-29',quote="Adjusted")
colnames(dfyh)<-c("dfyh")
rdfyh<-mean(diff(log(na.omit(dfyh))))*sqrt(252)
sdfyh<-sd(diff(log(na.omit(dfyh))))*sqrt(252)
ddfyh<-c(rdfyh,sdfyh)
ddfyh

par(family="STKaiti")
plot(sdfyh,rdfyh,ylim=c(-0.05,0.1),xlim=c(0,1.5),xlab="risk",ylab="return")
text(0.75,0.05,"东方雨虹")

points(sbjjz,rbjjz,col=2,pch=2)
text(0.75,-0.045,"北京君正",col=2)

points(sdrzb,rdrzb,col=3,pch=3)
text(0.55,-0.005,"东软载波",col=2)

points(sxzb,rxzb,col=4,pch=4)
text(0.65,0.005,"新宙邦",col=4)

points(sszty,rszty,col=5,pch=5)
text(0.5,-0.025,"神州泰岳",col=5)

points(sgldq,rgldq,col=6,pch=6)
text(0.35,0.035,"格力电器",col=6)

points(shxdq,rhxdq,col=7,pch=7)
text(0.25,0.05,"海信电器",col=7)

points(swk,rwk,col=8,pch=8)
text(0.45,0.02,"万科",col=8)
#
points(sbgjt,rbgjt,col=9,pch=9)
text(0.45,0.03,"宝钢集团",col=9)

points(szsyh,rzsyh,col=10,pch=10)
text(0.25,0.012,"招商银行",col=10)

points(sdrjt,rdrjt,col=11,pch=11)
text(0.5,-0.015,"东软集团",col=7)

points(sgsyh,rgsyh,col=12,pch=12)
text(0.2,0.02,"工商银行",col=12)

points(szxzq,rzxzq,col=13,pch=13)
text(0.35,0.015,"中信证券",col=13)

points(spfyh,rpfyh,col=14,pch=14)
text(0.25,-0.001,"浦发银行",col=14)

points(sgzmt,rgzmt,col=15,pch=15)
text(0.25,0.04,"贵州茅台",col=15)
