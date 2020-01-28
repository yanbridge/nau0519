outdir = "XXXX"

library(xts)
library(copula)
library(qrmtools)
library(Data)
library(scrAndFun)
library(ggplot2)

#### Data manupulation ####
coinList.xts <- list(
  BitCoinYahoo.xts = xts::xts(BitCoinYahoo[,2:7], order.by = BitCoinYahoo[,1]),
  BitCashYahoo.xts = xts::xts(BitCashYahoo[,2:7], order.by = BitCashYahoo[,1]),
  EthereumYahoo.xts = xts::xts(EthereumYahoo[,2:7], order.by = EthereumYahoo[,1]),
  RippleYahoo.xts = xts::xts(RippleYahoo[,2:7], order.by = RippleYahoo[,1])
)

MSCI.xts <- list(
  days = xts::xts(MSCI_DP[,2], order.by = MSCI_DP[,1]),
  weeks = xts::xts(MSCI_WP[,2], order.by = MSCI_WP[,1]),
  months = xts::xts(MSCI_MP[,2], order.by = MSCI_MP[,1])
)

dailyExcess <- Log_Period_Vol(listCoins = coinList.xts, listIndex = MSCI.xts,
                              period = "days",
                              delta = 60/61, annu = 365.25,
                              dateRange = list(
                                BitCoin = "2013/",
                                BitCash = "2017-08-01/",
                                Ethereum = "2016-02-01/",
                                Ripple = "2014-01-01/",
                                Cum = "2013/"
                              ))

#### Datamanipulation ####
SX <- returns(stocks.xts[,-4])
IX <- returns(indx.xts)
FX <- returns(cur.xts)
COMX <- returns(com.xts)
ZCB <- ZCB.xts[,c("THREEFY1","THREEFY3", "THREEFY10")]
colnames(ZCB) <- c("Y1", "Y3", "Y10")
ZCBX <- returns(ZCB)
crypto <- do.call(cbind, lapply(dailyExcess, function(x) {x$excessReturn}))[,1:4]
colnames(crypto) <- names(dailyExcess)[1:4]
crypto <- crypto[, c("BitCoin", "Ethereum", "Ripple")]
cryptoX <- crypto[-which(is.na(crypto), arr.ind = TRUE)[,1],]

periods <- lapply(list(SX, IX, FX, COMX, ZCBX, cryptoX), xts::periodicity)
names(periods) <- c("SX", "IX", "FX", "COMX", "ZCBX", "cryptoX"); periods

SX_sub <- SX[paste0(range(zoo::index(cryptoX)), collapse = "/")]
IX_sub <- IX[paste0(range(zoo::index(cryptoX)), collapse = "/")]
FX_sub <- FX[paste0(range(zoo::index(cryptoX)), collapse = "/")]
COMX_sub <- COMX[paste0(range(zoo::index(cryptoX)), collapse = "/")]
ZCBX_sub <- ZCBX[paste0(range(zoo::index(cryptoX)), collapse = "/")]

# Subset of same periodicity as CC
periods_sub <- lapply(list(SX_sub, IX_sub, FX_sub, COMX_sub, ZCBX_sub, cryptoX), xts::periodicity)
names(periods_sub) <- c("SX_sub", "IX_sub", "FX_sub", "COMX_sub", "ZCBX_sub", "cryptoX"); periods_sub

### Fitting statis copulas ####
foo <- list(crypto = cryptoX, Stocks = SX_sub, Index = IX_sub,
            Forex = FX_sub, Com = COMX_sub, ZCB = ZCBX_sub)
system.time(
  log_like_est_un <- get_log_like(prices = foo, type = "un")
)
system.time(
  log_like_est_ex <- get_log_like(prices = foo, type = "ex")
)
xtable::xtable(log_like_est_ex$log_like)
xtable::xtable(log_like_est_un$log_like)
xtable::xtable(log_like_est_ex$rho)
xtable::xtable(
  cbind(log_like_est_ex$log_like, log_like_est_un$log_like[,c("N", "t")]))

get_pairs_plot(foo, outdir)

#### Timevarying copula ####
crypto <- do.call(cbind, lapply(dailyExcess, function(x) {x$excessReturn}))[,1:4]
colnames(crypto) <- names(dailyExcess)[1:4]

crypto <- crypto[, c("BitCoin", "Ethereum", "Ripple")]
cryptoX <- crypto[-which(is.na(crypto), arr.ind = TRUE)[,1],]

# Index portfolios without NA's
ind_BTC_ETH <- which(is.na(crypto[,c("BitCoin","Ethereum")]), arr.ind = TRUE)[,1]
ind_BTC_XRP <- which(is.na(crypto[,c("BitCoin","Ripple")]), arr.ind = TRUE)[,1]
# Portfolio selection
crypto_BTC_ETH <- crypto[-ind_BTC_ETH,c("BitCoin","Ethereum")]
crypto_BTC_XRP <- crypto[-ind_BTC_XRP,c("BitCoin","Ripple")]
# Pseudo observations
U_BTC_ETH <- as.matrix(pobs(crypto_BTC_ETH))
U_BTC_XRP <- as.matrix(pobs(crypto_BTC_XRP))

# Fit of BTC_ETH
fit_N_St_BTC_ETH <- fitCopula(normalCopula(dim = dim(U_BTC_ETH)[2], dispstr = "un"),  data = U_BTC_ETH)
fit_C_St_BTC_ETH <- fitCopula(claytonCopula(dim = dim(U_BTC_ETH)[2]), data = U_BTC_ETH)
fit_Crot_St_BTC_ETH <- fitCopula(claytonCopula(dim = dim(U_BTC_ETH)[2]),  data = 1-U_BTC_ETH)

(maxLL_gaus_TV_BTC_ETH <- stats::nlminb(start = c(w = 0.5, alpha = 0.5, beta = 0.5),
                                        function(par) {
                                          U_foo = U_BTC_ETH
                                          LL_gaus_cpp(par, U_foo, 10)},
                                        lower = c(rep(-Inf,3)), upper = c(rep(Inf, 3))))
(maxLL_clayton_TV_BTC_ETH <- stats::nlminb(start = c(w = 0.5, alpha = 0.5, beta = 0.5),
                                           function(par) {
                                             U_foo = U_BTC_ETH
                                             LL_clayton_cpp(par, U_foo)},
                                           lower = c(rep(-Inf,3)), upper = c(rep(Inf, 3))))
(maxLL_clayton_surv_TV_BTC_ETH <- stats::nlminb(start = c(w = 0.1, alpha = 0.1, beta = 0.1),
                                                function(par) {
                                                  U_foo = U_BTC_ETH
                                                  LL_clayton_surv_cpp(par, U_foo)},
                                                lower = c(rep(-Inf,3)), upper = c(rep(Inf, 3))))
# Fit of BTC_XRP
fit_N_St_BTC_XRP <- fitCopula(normalCopula(dim = dim(U_BTC_XRP)[2], dispstr = "un"),  data = U_BTC_XRP)
(maxLL_clayton_surv_BTC_XRP <- stats::nlminb(start = c(th = 0.5),
                                             function(th) {
                                               U_foo = 1-U_BTC_XRP
                                               LL_clayton_static_cpp(th, U_foo)},
                                             lower = c(-1), upper = c(Inf)))
(maxLL_clayton_BTC_XRP <- stats::nlminb(start = c(th = 0.5),
                                        function(th) {
                                          U_foo = U_BTC_XRP
                                          LL_clayton_static_cpp(th, U_foo)},
                                        lower = c(-1), upper = c(Inf)))
(maxLL_gaus_TV_BTC_XRP <- stats::nlminb(start = c(w = 0.5, alpha = 0.5, beta = 0.5),
                                        function(par) {
                                          U_foo = U_BTC_XRP
                                          LL_gaus_cpp(par, U_foo, 10)},
                                        lower = c(rep(-Inf,3)), upper = c(rep(Inf, 3))))
(maxLL_clayton_TV_BTC_XRP <- stats::nlminb(start = c(w = 0.5, alpha = 0.5, beta = 0.5),
                                           function(par) {
                                             U_foo = U_BTC_XRP
                                             LL_clayton_cpp(par, U_foo)},
                                           lower = c(rep(-Inf,3)), upper = c(rep(Inf, 3))))
(maxLL_clayton_surv_TV_BTC_XRP <- stats::nlminb(start = c(w = 0.5, alpha = 0.5, beta = 0.5),
                                                function(par) {
                                                  U_foo = U_BTC_XRP
                                                  LL_clayton_surv_cpp(par, U_foo)},
                                                lower = c(rep(-Inf,3)), upper = c(rep(Inf, 3))))
# Log-likelihood
loglike_TV <- (rbind(c(N = fit_N_St_BTC_ETH@loglik,
                       C = fit_C_St_BTC_ETH@loglik,
                       C_rot = fit_Crot_St_BTC_ETH@loglik,
                       N_TV = -maxLL_gaus_TV_BTC_ETH$objective,
                       C_TV = -maxLL_clayton_TV_BTC_ETH$objective,
                       C_TV_rot = -maxLL_clayton_surv_TV_BTC_ETH$objective),
                     c(N = fit_N_St_BTC_XRP@loglik,
                       C = -maxLL_clayton_BTC_XRP$objective,
                       C_rot = -maxLL_clayton_surv_BTC_XRP$objective,
                       N_TV = -maxLL_gaus_TV_BTC_XRP$objective,
                       C_TV = -maxLL_clayton_TV_BTC_XRP$objective,
                       C_TV_rot = -maxLL_clayton_surv_TV_BTC_XRP$objective)))
rownames(loglike_TV) <- c("BTC-ETH", "BTC-XRP")
xtable::xtable(loglike_TV)
# Plot of pseudo-observations
pdf(file = paste0(outdir, "pseudoplot_bivar_cops.pdf"),
    width = 8, height = 6)
par(mfrow = c(1,2), mar = c(4,5,2,1) + 0.1)
plot(U_BTC_ETH, xlab = expression(U[1]^"BTC"), ylab = expression(U[2]^"ETH"),
     main = "Pseudo-observation of BTC-ETH")
plot(U_BTC_XRP[,1],U_BTC_XRP[,2], xlab = expression(U[1]^"BTC"), ylab = expression(U[2]^"XRP"),
     main = "Pseudo-observation of BTC-XRP")
par(mfrow = c(1,1))
dev.off()
# Parameter estimates
parameter_table <- cbind(N_BTC_ETH = maxLL_gaus_TV_BTC_ETH$par,
                         C_BTC_ETH = maxLL_clayton_TV_BTC_ETH$par,
                         C_BTC_ETH = maxLL_clayton_surv_TV_BTC_ETH$par,
                         N_BTC_XRP = maxLL_gaus_TV_BTC_XRP$par,
                         C_BTC_XRP = maxLL_clayton_TV_BTC_XRP$par,
                         C_BTC_XRP = maxLL_clayton_surv_TV_BTC_XRP$par)
xtable::xtable(parameter_table, digits = 3)
# Plot of time-varying parameter
par_BTC_ETH <- cbind(Gaussian = LL_gaus_sim_cpp(maxLL_gaus_TV_BTC_ETH$par, U_BTC_ETH, 10),
                     Clayton = LL_clayton_sim_cpp(maxLL_clayton_TV_BTC_ETH$par, U_BTC_ETH),
                     `Clayton rotated` = LL_clayton_sim_cpp(maxLL_clayton_surv_TV_BTC_ETH$par, U_BTC_ETH))
par_BTC_XRP <- cbind(Gaussian = LL_gaus_sim_cpp(maxLL_gaus_TV_BTC_XRP$par, U_BTC_XRP, 10),
                     Clayton = LL_clayton_sim_cpp(maxLL_clayton_TV_BTC_XRP$par, U_BTC_XRP),
                     `Clayton rotated` = LL_clayton_sim_cpp(maxLL_clayton_surv_TV_BTC_XRP$par, U_BTC_XRP))

param_BTC_ETH <- par_BTC_ETH %>%
  tibble::as_tibble() %>%
  dplyr::mutate(Date = zoo::index(crypto_BTC_ETH))
rho_BTC_ETH_tidy <- param_BTC_ETH[-c(1:10),] %>%
  dplyr::transmute(Gaussian = (2/pi)*asin(Gaussian),
                   Clayton = Clayton/(Clayton + 2),
                   `Clayton rotated` = `Clayton rotated`/(`Clayton rotated` + 2),
                   Date = Date) %>%
  tidyr::gather(key = "Model", value = "rho_t", - Date)
param_BTC_XRP <- par_BTC_XRP %>%
  tibble::as_tibble() %>%
  dplyr::mutate(Date = zoo::index(crypto_BTC_XRP))
rho_BTC_XRP_tidy <- param_BTC_XRP[-c(1:10),] %>%
  dplyr::transmute(Gaussian = (2/pi)*asin(Gaussian),
                   Clayton = Clayton/(Clayton + 2),
                   `Clayton rotated` = `Clayton rotated`/(`Clayton rotated` + 2),
                   Date = Date) %>%
  tidyr::gather(key = "Model", value = "rho_t", - Date)

pdf(file = paste0(outdir, "btc_ETH_dyn_tau.pdf"),
    width = 8, height = 6)
ggplot(rho_BTC_ETH_tidy, aes(x = Date, y = rho_t, col = Model)) +
  geom_line(size = 0.7) +
  theme_classic() +
  labs(title = "Dynamics of time-varying parameter",
       subtitle = "BTC-ETH",
       caption = expression(hat(rho)[t]~" transformed to Kendall's "~tau),
       y = expression(hat(rho)[t]^tau)) +
  ggplot2::scale_color_manual(values = c(RColorBrewer::brewer.pal(3,"Set1"))) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(size=12))
dev.off()
pdf(file = paste0(outdir, "btc_XRP_dyn_tau.pdf"),
    width = 8, height = 6)
ggplot(rho_BTC_XRP_tidy, aes(x = Date, y = rho_t, col = Model)) +
  geom_line(size = 0.7) +
  theme_classic() +
  labs(title = "Dynamics of time-varying parameter",
       subtitle = "BTC-XRP",
       caption = expression(hat(rho)[t]~" transformed to Kendall's "~tau),
       y = expression(hat(rho)[t]^tau)) +
  ggplot2::scale_color_manual(values = c(RColorBrewer::brewer.pal(3,"Set1"))) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(size=12))
dev.off()
