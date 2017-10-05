S0 <- 50;          # initial price
mu <- 0.04;        # mean
sigma <- 0.3;      # volatility
T <- 1;            # 1 year
n <- 10000;          # step
N <-252;            # the number of sample
price = matrix(NA,n,N+1);
price[,1] = S0;
dt = T/n
nudt = (mu-0.5*sigma^2)*dt
sidt = sigma*sqrt(dt)
for (i in 1:n){
	for (j in 1:N){
		price[i,j+1] = price[i,j]*exp(nudt + sidt*rnorm(1))
	}
}
matplot(t(price),type="l",col=1:9)