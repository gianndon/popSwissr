return_matrix1 <- function(dat){
  return((matrixStats::colDiffs(dat)/dat[-1,]))
}

return_matrix2 <- function(dat){
  return(timeSeries::returns(dat, na.rm=TRUE)[-1,])
}


dat_ret1 <- return_matrix1(dat=dat)
any(dat_ret1 > 1)

dat_ret2 <- return_matrix2(dat=dat)
any(dat_ret2 > 1)

tp(assets=dat_ret1[, -which(names(dat_ret1) == "USDCHF.X.Adjusted")])
