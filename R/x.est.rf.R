#' Function to estimate CATE
#' @param xmodel x-learner model
#' @param data.x data containing covariate features
#' @return CATE estimates
x.est.rf <- function(xmodel, data.x){
  tau1 <- predict(xmodel$m3, data.x)
  tau0 <- predict(xmodel$m4, data.x)
  pscore <- predict(xmodel$g, data.x)
  tau <- pscore * tau0 + (1-pscore) * tau1
  return(tau)
}