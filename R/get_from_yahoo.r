#' retrieve historical prices for a vector of stocks
#' 
#' get_from_yahoo retrieves historical prices for a vector of stocks from yahoo 
#' finance's historical price API \url{http://ichart.yahoo.com/}.
#' 
#' @param symbols A character vector of stock or index symbols. Each symbol must 
#' be reocgnized by \url{http://ichart.yahoo.com/}, which means all symbols 
#' must conform to URL encoding standards. A good way to find the appropriate 
#' symbol for a stock is to manually search for the stock at 
#' \url{http://finance.yahoo.com/} and then observe how the stock is referred to 
#' in the web address.
#' 
#' @param start A date or character string that marks the beginning of the 
#' investment period.
#' 
#' @param end A date or character string that marks the end of the investment 
#' period. end must occur after start. Defaults to "2012-12-01".
#' 
#' @return get_from_yahoo returns a data frame arranged in the tidy format. 
#' The data frame shows the daily closing price of stocks, adjusted for splits 
#' and dividends. 
#' @export
get_from_yahoo <- function(symbols, start, end = "2012-12-01") {
  
  if (length(start) != 1 | length(end) != 1) {
    stop("start and end must have a length of 1.")
  }
  
  # extract date components
  start <- as.Date(start)
  end <- as.Date(end)
  
  start.day <- day(start)
  start.month <- month(start) - 1
  start.year <- year(start)
  
  end.day <- day(end)
  end.month <- month(end) - 1
  end.year <- year(end)
  
  # build urls for yahoo's API
  urls <- paste("http://ichart.yahoo.com/table.csv?s=", symbols, 
                "&a=", start.month,         
                "&b=", start.day,
                "&c=", start.year,
                "&d=", end.month,           
                "&e=", end.day,
                "&f=", end.year,
                "&g=d&ignore=.csv",             
                sep = "")
  
  returns <- lapply(as.list(urls), read.csv)
  names(returns) <- symbols
  returns
}
