tp <- function(assets, rf=NA, p_year=260){
  # default rf value
  rf <- 0.01
  # yearly returns, volatility and covariance
  yearly_return <- apply(X=assets*p_year, MARGIN=2, FUN=mean)
  yearly_volatility <- apply(X=assets*sqrt(p_year), MARGIN=2, FUN=sd)
  Sigma <- cov(assets); Sigma_inv <- solve(Sigma)
  # compute weights
  weights <- Sigma_inv %*% (yearly_return - rf*rep(1, dim(Sigma)[1]))
  weights_scal <- weights/sum(weights); weights_scal <- as.vector(weights_scal); names(weights_scal) <- colnames(assets)
  # compute return and volatility
  return_TP <- t(weights_scal) %*% yearly_return; return_TP <- as.vector(return_TP); names(return_TP) <- "Return Portfolio"
  volatility_TP <- sqrt(t(weights_scal) %*% Sigma %*% weights_scal)*sqrt(260); volatility_TP <- as.vector(volatility_TP); names(volatility_TP) <- "Volatility Portfolio"
  # return
  return(list(weights=weights_scal, return=abs(return_TP), volatility=volatility_TP))
}

tp(assets=dat)
