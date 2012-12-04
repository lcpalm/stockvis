\name{clean_tickers}
\alias{clean_tickers}
\title{Replace url tickers of the S&P 500 and S&P 100 indexes with human readable
labels.}
\usage{
  clean_tickers(tickers)
}
\arguments{
  \item{tickers}{a vector of character strings.}
}
\value{
  A vector of character strings where all instances of
  "%5EGSPC" and "%5EOEX" have been replaced.
}
\description{
  Replace url tickers of the S&P 500 and S&P 100 indexes
  with human readable labels.
}

