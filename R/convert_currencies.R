convert_currencies <- function(symbols){
  data <- get_data(symbols=symbols)
  data$GSPC.Adjusted <- data$USDCHF.X.Adjusted * data$GSPC.Adjusted
  data$BTC.USD.Adjusted <- data$USDCHF.X.Adjusted * data$BTC.USD.Adjusted
  data$TNX.Adjusted <- data$USDCHF.X.Adjusted * data$TNX.Adjusted
  return(data)
}
