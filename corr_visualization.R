set.seed(955) 
vvar <- 1:20 + rnorm(20,sd=3) 
wvar <- 1:20 + rnorm(20,sd=5) 
xvar <- 20:1 + rnorm(20,sd=3) 
yvar <- (1:20)/2 + rnorm(20, sd=10) 
zvar <- rnorm(20, sd=6) 

# 一个包含多组变量的数据框 
data <- data.frame(vvar, wvar, xvar, yvar, zvar) 
head(data) 

library(ellipse) 

# 构建相关系数表 
ctab <- cor(data) 
round(ctab, 2)

# 绘制图像，减少画布的边际留白 
plotcorr(ctab, mar = c(0.1, 0.1, 0.1, 0.1)) 

# 在上面代码的基础上加入用于反应相关系数大小的颜色 
colorfun <- colorRamp(c("#CC0000","white","#3366CC"), space="Lab") 
plotcorr(ctab, col=rgb(colorfun((ctab+1)/2), maxColorValue=255), 
mar = c(0.1, 0.1, 0.1, 0.1)) 
