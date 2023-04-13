get_rf_ym <- function(){
  # URL <- "https://www.snb.ch/de/iabout/stat/statrep/id/current_interest_exchange_rates#t2"  # SNB URL
  URL <- "https://www.yourmoney.ch/ym/details/4961368%2C1526%2C1#Tab0"  # yourmoney URL
  pagecode <- xml2::read_html(x=URL)
  pagecode <- rvest::html_nodes(pagecode, css=".detailPrice")  # yourmoney SARON
  # pagecode <- rvest::html_nodes(pagecode, css=".col-md-6:nth-child(3) .heading+ span")  # SNB SARON
  pagecode <- rvest::html_text(pagecode)
  # rf <- as.numeric(substr(x=pagecode[1], start=1, stop=nchar(pagecode[1])-1))  # rf return SNB
  pagecode_clean <- gsub(" ", "", pagecode, fixed = TRUE)
  rf <- as.numeric(substr(x=pagecode_clean[1], start=1, stop=nchar(pagecode_clean[1])-1))  # rf return yourmoney
  return(rf)
}
