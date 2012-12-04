#' Get historical cumulative returns for a set of stocks or indexes
#'
#' get_stocks retrieves historical cumulative returns data for a group of stocks 
#' from the yahoo finance API. Daily closing prices are retrieved for each stock 
#' for the investment period that begins on start and ends on end. Prices are 
#' converted into returns as a proportion of the stock's value on start.
#' 
#' @seealso \code{\link{get_portfolio}}, \code{\link{get_returns}}
#' 
#' @param tickers A vector of stock ticker symbols. Ticker symbols should comply
#' with URL encoding standards for special characters. Common tickers include 
#' %5EGSPC (the S&P 500), %5EOEX (the S&P 100), DIA (Dow Jones Industrial 
#' Average), and DOD (Dogs of the dow).
#' 
#' @param start A date or character string that marks the beginning of the 
#' investment period. Defaults to "2007-12-01".
#' 
#' @param end A date or character string that marks the end of the investment 
#' period. end must occur after start. Defaults to "2012-12-01".
#' 
#' @return get_stocks returns a data frame arranged in the tidy format. 
#' The data frame shows the daily values of stocks as a proportion of the 
#' initial investment.
#' @export
get_stocks <- function(tickers, start = "2007-12-01", end = "2012-12-01") {
  
  returns <- get_from_yahoo(tickers, start = start, end = end)
  
  # Return a tidy, minimal data frame. Convert prices to percentages
  tickers <- clean_tickers(tickers)
  names(returns) <- tickers
  ldply(returns, summarise, date = as.Date(Date), 
        return = Adj.Close / Adj.Close[which.min(Date)])
}

#' Replace url tickers of the S&P 500 and S&P 100 indexes with human readable 
#' labels.
#' 
#' @param tickers a vector of character strings.
#' 
#' @return A vector of character strings where all instances of "%5EGSPC"
#' and "%5EOEX" have been replaced.
clean_tickers <- function(tickers) {
  url.case <- c("%5EGSPC", "%5EOEX")
  common <- c("%5EGSPC" = "S&P_500", "%5EOEX" = "S&P_100")
  cleanable <- tickers %in% url.case
  tickers[cleanable] <- common[tickers[cleanable]]
  tickers
}