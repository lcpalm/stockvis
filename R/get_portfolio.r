#' Get historical daily returns for a portfolio of stocks or indexes
#'
#' get_portfolio retrieves historical returns data from the yahoo finance API 
#' for an equal weighted portfolio of stocks. Daily closing prices are retrieved
#' for the portfolio for the investment period that begins on start and ends on 
#' end. Prices are converted into returns as a proportion of the portfolio's 
#' value on start.
#' 
#' get_portfolio differs from \code{\link{get_stock}} in that get_portfolio 
#' provides a single set of returns: the returns on a portfolio made by 
#' investing equally in each stock within the portfolio on start and never 
#' rebalancing. get_stocks provides an individual set of returns for each stock.
#' 
#' @seealso \code{\link{get_stock}}, \code{\link{get_returns}}
#' 
#' @param tickers A vector of stock ticker symbols. get_portfolio will generate 
#' a portfolio that invests equal amounts of money in each of these stocks on 
#' start and never rebalances. Ticker symbols should comply with URL encoding 
#' standards for special characters.
#' 
#' @param start A date or character string that marks the beginning of the 
#' investment period. Defaults to "2007-12-01".
#' 
#' @param end A date or character string that marks the end of the investment 
#' period. end must occur after start. Defaults to "2012-12-01".
#' 
#' @param name An optional name to give the portfolio in the results. Must be 
#' a character string. Defaults to "portfolio."
#' 
#' @return get_portfolio returns a data frame arranged in the tidy format. 
#' The data frame shows the daily values of the portfolio as a proportion of the 
#' initial investment.
#' @export
get_portfolio <- function(tickers, start = "2007-12-01", end = "2012-12-01", 
                          name = "portfolio"){
  
  returns <- get_stocks(tickers = tickers, start = start, end = end)
  returns <- ddply(returns, "date", summarise, return = mean(return))
  returns$.id <- name
  returns
}
