stats <- function(returns.df) {
  results <- function(df) {
    DATE.A=which.min(df$date)
    DATE.B=which.max(df$date) 
    AMT.1=df$return[DATE.A]
    AMT.2=df$return[DATE.B]
    Days=as.numeric(difftime(df$date[DATE.B], df$date[DATE.A]))
    cagr=round(((AMT.2/AMT.1)^(365/Days)-1)*100,2)
    if (!("adj.return" %in% names(df))) return(data.frame(return = AMT.2, cagr = cagr))
    AAMT.1=df$adj.return[DATE.A]
    AAMT.2=df$adj.return[DATE.B]
    CAGR=round(((AAMT.2/AAMT.1) ^ (365/Days) - 1) * 100, 2)
    data.frame(return = AMT.2, cagr = cagr, adj.return = AAMT.2, adj.cagr = CAGR)
    } 
ddply(returns.df, ".id", results)
}

# Modified in class:
stats <- function(returns.df) {
  
  results <- function(df) {
  #
    DATE.A <- which.min(df$date)
    DATE.B <- which.max(df$date) 
    AMT.1 <- df$return[DATE.A]
    AMT.2 <- df$return[DATE.B]
    Days <- as.numeric(difftime(df$date[DATE.B], df$date[DATE.A]))
    cagr <- round(((AMT.2/AMT.1)^(365/Days)-1)*100,2)
    if (!("adj.return" %in% names(df))) return(data.frame(return = AMT.2, cagr = cagr))
    AAMT.1 <- df$adj.return[DATE.A]
    AAMT.2 <- df$adj.return[DATE.B]
    CAGR <- round(((AAMT.2/AAMT.1) ^ (365/Days) - 1) * 100, 2)
    data.frame(return = AMT.2, cagr = cagr, adj.return = AAMT.2, adj.cagr = CAGR)
  } 
  ddply(returns.df, ".id", results)
}
