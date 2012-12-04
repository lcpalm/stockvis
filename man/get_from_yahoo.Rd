\name{get_from_yahoo}
\alias{get_from_yahoo}
\title{retrieve historical prices for a vector of stocks}
\usage{
  get_from_yahoo(symbols, start, end = "2012-12-01")
}
\arguments{
  \item{symbols}{A character vector of stock or index
  symbols. Each symbol must be reocgnized by
  \url{http://ichart.yahoo.com/}, which means all symbols
  must conform to URL encoding standards. A good way to
  find the appropriate symbol for a stock is to manually
  search for the stock at \url{http://finance.yahoo.com/}
  and then observe how the stock is referred to in the web
  address.}

  \item{start}{A date or character string that marks the
  beginning of the investment period.}

  \item{end}{A date or character string that marks the end
  of the investment period. end must occur after start.
  Defaults to "2012-12-01".}
}
\value{
  get_from_yahoo returns a data frame arranged in the tidy
  format. The data frame shows the daily closing price of
  stocks, adjusted for splits and dividends.
}
\description{
  get_from_yahoo retrieves historical prices for a vector
  of stocks from yahoo finance's historical price API
  \url{http://ichart.yahoo.com/}.
}

