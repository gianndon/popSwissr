r_opt <- function(assets, r_pf, shorting=TRUE, p_year=260){
  # import library
  library(nloptr)
  
  # yearly returns, volatility and covariance
  r <- apply(X=assets*p_year, MARGIN=2, FUN=mean)
  # yearly_volatility <- apply(X=assets*sqrt(p_year), MARGIN=2, FUN=sd)
  # compute covariance matrix
  Sigma <- p_year*cov(assets)
  
  # minimize risk
  # objective function
  eval_f <- function(x, Sigma, r, r_pf){
    # !important!: x = weights
    return(as.numeric(t(x) %*% Sigma %*% x))
  }
  
  # lower and upper bounds, with distinction whether shorting is allowed or not
  if(shorting == TRUE){
    lb <- rep(-3, ncol(Sigma))
    ub <- rep(3, ncol(Sigma))
  }else{
    lb <- rep(0, ncol(Sigma))
    ub <- rep(1, ncol(Sigma))
  }
  
  # Constraints
  eval_g_eq <- function(x, Sigma, r, r_pf){
    # !important!: x = weights
    constr <- c(sum(x) - 1,
                x %*% r - r_pf)
    return(constr)
  }
  
  # Initial weights
  x0 <- rep(1/3, ncol(Sigma))
  
  # set optimization options
  opts <- list("algorithm"="NLOPT_GN_ISRES",
               "xtol_rel"=1.0e-15,
               "maxeval"=160000,
               "local_opts"=list("algorithm"="NLOPT_LD_MMA",
                                "xtol_rel"=1.0e-15 ),
               "print_level"=0)
  
  # optimization
  res <- nloptr(x0=x0,
                eval_f = eval_f,
                lb = lb,
                ub = ub,
                # eval_g_ineq = eval_g_ineq,
                eval_g_eq = eval_g_eq,
                opts = opts, 
                Sigma = Sigma,
                r = r,
                r_pf = r_pf
  )
  
  
  # results
  var_pf <- res$objective
  sd_pf <- sqrt(res$objective)
  weights_pf <- res$solution
  # return
  return(list(var_pf=var_pf, sd_pf=sd_pf, weights_pf=weights_pf))
}
  