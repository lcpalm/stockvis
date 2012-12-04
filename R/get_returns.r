#' Get something something something described here
#' 
#' @export
get_returns <- function(portfolio, start = "2012-01-01", end = "2012-12-01", 
                        name = "portfolio", benchmark = "%5EGSPC", 
                        adjust = FALSE) {
  
  portfolio <- get_portfolio(portfolio, start = start, end = end, name = name)
  benchmark <- get_stocks(benchmark, start = start, end = end)
  
  returns <- rbind(portfolio, benchmark)
  
  if (adjust) {
    returns <- adj_returns(returns)
  }
  
  returns
}
