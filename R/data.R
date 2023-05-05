# get data (0P0001GYGN.SW = Siwsscanto Bond)
assets1 <- c("^SSMI", "USDCHF=X", "^GSPC", "GC=F", "BTC-USD", "SREN.SW", "^TNX")
dat_raw <- popSwissr::convert_currencies(symbols=assets1)
dat <- timeSeries::returns(dat_raw, na.rm=TRUE)[-1,]
#dat <- popSwissr::return_matrix1(dat_raw)
dat <- as.matrix(dat)
dat <- dat[, -2]
dat <- unname(dat)
dat


# definitions
yearly_returns <- apply(X=dat*260, MARGIN=2, FUN=mean)
Sigma <- cov(dat)


# compute mvp & tp weight, returns and volatiolites
# mvp
mvp_n <- mvp(assets=dat); mvp_n <- c(mvp_n$weights, mvp_n$return, mvp_n$volatility); mvp_n <- unname(mvp_n); mvp_n
mvp_os <- mvp_opt(assets=dat, shorting=TRUE); mvp_os <- c(mvp_os$mvp_weights, mvp_os$mvp_return, mvp_os$mvp_vola); mvp_os
mvp_ows <- mvp_opt(assets=dat, shorting=FALSE); mvp_ows <- c(mvp_ows$mvp_weights, mvp_ows$mvp_return, mvp_ows$mvp_vola); mvp_ows
# tp
tp_n <- tp(assets=dat); tp_n <- c(tp_n$weights, tp_n$return, tp_n$volatility); tp_n <- unname(tp_n); tp_n
tp_os <- tp_opt(assets=dat, shorting=TRUE); tp_os <- c(tp_os$tp_weights, tp_os$tp_return, tp_os$tp_vola); tp_os
tp_ows <- tp_opt(assets=dat, shorting=FALSE); tp_ows <- c(tp_ows$tp_weights, tp_ows$tp_return, tp_ows$tp_vola); tp_ows
# generate data frame
dat_cl <- data.frame(MVP_normal_function_short=mvp_n,
                     MVP_opt_short=mvp_os,
                     MVP_opt_not_short=mvp_ows,
                     TP_normal_function_short=tp_n,
                     TP_opt_short=tp_os,
                     TP_opt_not_short=tp_ows)
row.names(dat_cl) <- c(names(dat_raw[,-2]), "return", "vola"); dat_cl

abs((dat_cl$MVP_normal_function_short - dat_cl$MVP_opt_not_short)/dat_cl$MVP_normal_function_short)


# plot efficient frontier
# define an alpha sequence
alpha <- seq(-1, 3, 0.01)
# calculate portfolio weights with different alphas (therefore the %o% = outter product is needed)
w_pf <- alpha %o% mvp(assets=dat)$weights + (1 - alpha) %o% tp(assets=dat)$weights
# define the portfolio returns
return_pf <- w_pf %*% yearly_returns

# define the portfolio volatilities
volatility_pf <- c()
for(i in 1:length(alpha)){volatility_pf[i] <- sqrt(t(w_pf[i,]) %*% Sigma %*% w_pf[i,]) * sqrt(p_year)}

# plot of the efficient frontier
plot(x=100*volatility_pf, y=100*return_pf, type="l", col="red", xlab="volatilities [%]",
     ylab="returns [%]", main="efficient frontier", xlim=c(0, 30), ylim=c(-25, 25))

# mvp point with shorting
points(x=100*dat_cl$MVP_normal_function_short[nrow(dat_cl)],
       y=100*dat_cl$MVP_normal_function_short[nrow(dat_cl)-1],
       col="red", pch=16, cex=2)
text(x=100*dat_cl$MVP_normal_function_short[nrow(dat_cl)],
     y=dat_cl$MVP_normal_function_short[nrow(dat_cl)-1],
     labels=c("MVP_Normal_Short"), pos=4, offset=1)

# mvp point with optimizer with shorting
points(x=100*dat_cl$MVP_opt_short[nrow(dat_cl)], y=100*dat_cl$MVP_opt_short[nrow(dat_cl)-1],
       col="blue", pch=16, cex=1)
text(x=100*dat_cl$MVP_opt_short[nrow(dat_cl)], y=100*dat_cl$MVP_opt_short[nrow(dat_cl)-1],
     labels=c("MVP_Opt_Short"), pos=1, offset=1)

# mvp point with optimizer without shorting
points(x=100*dat_cl$MVP_opt_not_short[nrow(dat_cl)], y=100*dat_cl$MVP_opt_not_short[nrow(dat_cl)-1],
       col="orange", pch=16, cex=0.5)
text(x=100*dat_cl$MVP_opt_not_short[nrow(dat_cl)], y=100*dat_cl$MVP_opt_not_short[nrow(dat_cl)-1],
     labels=c("MVP_Opt_N_Short"), pos=3, offset=1)

# tp point with shorting
points(100*dat_cl$TP_normal_function_short[nrow(dat_cl)],
       y=100*dat_cl$TP_normal_function_short[nrow(dat_cl)-1],
       col="green", pch=16, cex=2)
text(x=100*dat_cl$TP_normal_function_short[nrow(dat_cl)],
     y=100*dat_cl$TP_normal_function_short[nrow(dat_cl)-1],
     labels=c("TP_Normal_Short"), pos=3, offset=1)

# tp point with optimizer with shorting
points(100*dat_cl$TP_opt_short[nrow(dat_cl)],
       y=100*dat_cl$TP_opt_short[nrow(dat_cl)-1],
       col="pink", pch=16, cex=2)
text(x=100*dat_cl$TP_opt_short[nrow(dat_cl)],
     y=100*dat_cl$TP_opt_short[nrow(dat_cl)-1],
     labels=c("TP_Opt_Short"), pos=3, offset=1)

# tp point with optimizer without shorting
points(100*dat_cl$TP_opt_not_short[nrow(dat_cl)],
       y=100*dat_cl$TP_opt_short[nrow(dat_cl)-1],
       col="lightblue", pch=16, cex=1)
text(x=100*dat_cl$TP_opt_not_short[nrow(dat_cl)],
     y=100*dat_cl$TP_opt_not_short[nrow(dat_cl)-1],
     labels=c("TP_Opt_N_Short"), pos=4, offset=1)

# risk free return
points(x=0, y=0.01, color="grey", pch=16)
abline(v=0, color="grey")

# draw cpital marekt line
lines(x=c(0, 100*dat_cl$TP_normal_function_short[nrow(dat_cl)]),
      y=c(0.01, 100*dat_cl$TP_normal_function_short[nrow(dat_cl)-1]),
      lty=2)
