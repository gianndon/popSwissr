get_data <- function(symbols, starting_point=Sys.Date()-10*365, end_point=Sys.Date()){
  library(quantmod)
  dat <- do.call(cbind, sapply(X=symbols, FUN=function(x){
    getSymbols(Symbols=x,
               from=starting_point,
               to=end_point,
               auto.assign=FALSE)[, 6]
  })) %>%
    `colnames<-`(symbols)
  dat_bereinigt <- na.omit(dat)
  return(dat_bereinigt)
}
