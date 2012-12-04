\name{get_stocks}
\alias{get_stocks}
\title{Get historical cumulative returns for a set of stocks or indexes}
\usage{
  get_stocks(tickers, start = "2007-12-01",
    end = "2012-12-01")
}
\arguments{
  \item{tickers}{A vector of stock ticker symbols. Ticker
  symbols should comply with URL encoding standards for
  special characters. Common tickers include %5EGSPC (the
  S&P 500), %5EOEX (the S&P 100), DIA (Dow Jones Industrial
  Average), and DOD (Dogs of the dow).}

  \item{start}{A date or character string that marks the
  beginning of the investment period. Defaults to
  "2007-12-01".}

  \item{end}{A date or character string that marks the end
  of the investment period. end must occur after start.
  Defaults to "2012-12-01".}
}
\value{
  get_stocks returns a data frame arranged in the tidy
  format. The data frame shows the daily values of stocks
  as a proportion of the initial investment.
}
\description{
  get_stocks retrieves historical cumulative returns data
  for a group of stocks from the yahoo finance API. Daily
  closing prices are retrieved for each stock for the
  investment period that begins on start and ends on end.
  Prices are converted into returns as a proportion of the
  stock's value on start.
}
\seealso{
  \code{\link{get_portfolio}}, \code{\link{get_returns}}
}

