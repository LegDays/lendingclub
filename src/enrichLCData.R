backOutRate <- function(aPrincipal, aTotalPayment, aTerm) {
  mySecondsToYearsFactor <- 1 / (60 * 60 * 24 * 365.25)
  myTermInYears <- aTerm * mySecondsToYearsFactor
  theRate <- ((aTotalPayment / aPrincipal) ^ (1 / myTermInYears)) - 1
  return(theRate)
}

enrichWithRealizedRatesColumn <- function(aLoans) {
  aLoans$realizedRates <- mapply(function(p, total, t) backOutRate(p, total, t),
                                 aLoans$funded_amnt, aLoans$total_pymnt, aLoans$term)
  return(aLoans)
}

enrichLoans <- function(aLoans) {
  aLoans <- enrichWithRealizedRatesColumn(aLoans)
  return(aLoans)
}