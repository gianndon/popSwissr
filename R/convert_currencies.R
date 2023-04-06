convert_currencies <- function(symbols, starting_point=Sys.Date() - 30*365, endpoint=Sys.Date()){
    data <- get_data(symbols=symbols, starting_point=starting_point, endpoint=endpoint)
    data$GSPC.Adjusted <- data$USDCHF.X.Adjusted * data$GSPC.Adjusted
    data$BTC.USD.Adjusted <- data$USDCHF.X.Adjusted * data$BTC.USD.Adjusted
    data$TNX.Adjusted <- data$USDCHF.X.Adjusted * data$TNX.Adjusted
    return(data)
}
