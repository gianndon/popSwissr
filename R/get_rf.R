get_rf <- function(){
  URL <- "https://www.snb.ch/de/iabout/stat/statrep/id/current_interest_exchange_rates#t2"
  pagecode <- xml2::read_html(x=URL)
  pagecode <- rvest::html_nodes(pagecode, css=".col-md-6:nth-child(4) .heading+ span")
  pagecode <- rvest::html_text(pagecode)
  rf <- as.numeric(substr(x=pagecode[1], start=1, stop=nchar(pagecode[1])-1))
  return(rf)
}


