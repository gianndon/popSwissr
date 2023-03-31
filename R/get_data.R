get_data <- function(symbols, starting_point=Sys.Date()-10*365, end_point=Sys.Date()){
  # get data from yahoo finance
  dat <- do.call(cbind, sapply(X=symbols, FUN=function(x){
    quantmod::getSymbols(Symbols=x,
                         from=starting_point,
                         to=end_point,
                         auto.assign=FALSE)[, 6]
  })) %>%
    `colnames<-`(symbols)
  # drop na's and return data
  return(na.omit(dat))
}
