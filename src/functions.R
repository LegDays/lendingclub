backOutRate <- function(aPrincipal, aTotalPayment, aTerm) {
  theRate <- ((aTotalPayment / aPrincipal) ^ (1 / aTerm)) - 1
  return(theRate)
}