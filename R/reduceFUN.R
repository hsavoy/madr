### params as a list? of functions?

#' ## Matern Function
#' Useful for signals that approach a steady value but
#' have varied slopes at x=0, such as drawdown.
matern <- function(x, params){
  sigma <- params[1]
  lambda <- params[2]
  kappa <- params[3]   #0.5
  t <- sqrt(2*kappa)*x/lambda
  cov <-  ((sigma*(t^kappa)/gamma(kappa))*2^(1-kappa))*besselK(t,kappa)
  return(sigma-cov)
}

init.matern <- function(x){
  params<- c()
  params[1] <- min(x)
  params[2] <- 5 #tail(which(x > 0.99*min(x)),1)
  params[3] <- 0.5
  return(params)
}
