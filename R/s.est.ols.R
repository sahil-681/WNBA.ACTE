#' Function to estimate CATE
#' @param xmodel x-learner model
#' @param x_data data containing covariate features
#' @return CATE estimates
s.est.ols <- function(xmodel, x_data){
  x_data['w'] <- 0
  mu0 <- predict(xmodel$m1, x_data)
  x_data['w'] <- 1
  mu1 <- predict(xmodel$m1, x_data)
  tau <- mu1 - mu0
  return(tau)
}