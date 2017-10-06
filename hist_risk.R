set.seed(123)
hr<-rnorm(100)
hist(hr,col="lightblue",prob=T)
lines(density(hr), col="red", lwd=3)
curve(dnorm(x),col='blue', lwd=3, lty=2, add=T)

##change number
hr1<-sort(hr)
hr1[1:3]<-c(-4.5,-3.8,-4.3)
hist(hr1,col="lightblue",prob=T)
lines(density(hr1), col="red", lwd=3)
curve(dnorm(x),col='blue', lwd=3, lty=2, add=T)
