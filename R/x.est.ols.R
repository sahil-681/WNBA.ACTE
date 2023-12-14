#' Function to estimate CATE
#' @param xmodel x-learner model
#' @param x_data data containing covariate features
#' @return CATE estimates
#' @export
x.est.ols <- function(xmodel, x_data){
  tau1 <- predict(xmodel$m3, x_data)
  tau0 <- predict(xmodel$m4, x_data)
  pscore <- predict(xmodel$g, x_data)
  tau <- pscore * tau0 + (1-pscore) * tau1
  return(tau)
}