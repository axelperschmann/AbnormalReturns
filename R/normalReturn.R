compute_normalReturn.marketmodel <- function(estimationWindow.prices_stock, estimationWindow.prices_market, actualReturn.price_market) {
  # Ordinary Least Squares (OLS)
  M = lm(estimationWindow.prices_stock ~ estimationWindow.prices_market)

  nd = data.frame(estimationWindow.prices_market = actualReturn.price_market)
  normalReturn = predict(M, newdata = nd)

  return(normalReturn)
}


compute_normalReturn.constantmeanmodel <- function(estimationWindow.prices_stock) {

  normalReturn = mean(estimationWindow.prices_stock)

  return(normalReturn)
}
