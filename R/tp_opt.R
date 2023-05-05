tp_opt <- function(assets, shorting=TRUE, rf=NA, p_year=260){
  
  # import library
  library(nloptr)
  
  # yearly returns
  r <- apply(X=dat*p_year, MARGIN=2, FUN=mean)
  # covariance amtrix
  Sigma <- cov(dat)
  # number of assets
  n <- ncol(Sigma)
  # risk free return
  r_rf <- 0.01
  
  # objective funciton
  eval_f <- function(x, Sigma, r, r_rf){
    # !important!: x = weights
    return(-1*((t(x) %*% r)/(t(x) %*% Sigma %*% x)))
  }
  
  # equality constraint
  eval_g_eq <- function(x, Sigma, r, r_rf){
    # !important!: x = weights
    return(sum(x) - 1)
  }
  
  # lower and upper bounds, with distinction whether shorting is allowed or not
  if(shorting == TRUE){
    lb <- rep(-20, ncol(Sigma))
    ub <- rep(20, ncol(Sigma))
  }else{
    lb <- rep(0, ncol(Sigma))
    ub <- rep(1, ncol(Sigma))
  }
  

  # initial value
  x0 <- rep(1/n, n)
  
  # set optimization options
  opts <- list("algorithm"="NLOPT_GN_ISRES",
               "xtol_rel"=1.0e-15,
               "maxeval"=160000,
               "print_level"=0,
               "local_opts"=list("algorithm"="NLOPT_LD_MMA",
                                 "xtol_rel"=1.0e-15 ))
  
  # optimization
  res <- nloptr(x0=x0,
                eval_f=eval_f,
                lb=lb,
                ub=ub,
                eval_g_eq=eval_g_eq,
                opts=opts, 
                Sigma=Sigma,
                r=r,
                r_rf=r_rf)
  
  # compute mvp return and volatility and weights
  
  weights <- Sigma_inv %*% (yearly_return - rf*rep(1, dim(Sigma)[1]))
  
  weights_scal <- as.vector(res$solution)
  tp_return <- as.vector(abs(t(weights_scal) %*% r))
  tp_vola <- as.vector(sqrt(t(weights_scal) %*% Sigma %*% weights_scal)*sqrt(p_year))
  
  # return
  return(list(tp_return=tp_return, tp_vola=tp_vola, tp_weights=weights_scal))
}

tp_opt(assets=dat, shorting=TRUE)
tp(assets=dat)
