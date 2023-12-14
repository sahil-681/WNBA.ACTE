#' Function to estimate CATE
#' @param xmodel x-learner model
#' @param data.x data containing covariate features
#' @return CATE estimates
#' @export
t.est.rf <- function(xmodel, data.x){
  mu0 <- predict(xmodel$m1, data.x)
  mu1 <- predict(xmodel$m2, data.x)
  tau <- mu1 - mu0
  return(tau)
}