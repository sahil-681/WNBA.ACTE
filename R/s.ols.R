# S-learner for OLS
#' @param x_data Matrix of covariates for the observations in the training set.
#' @param w_train The vector indicating treatment assignment for each observation 
#' in the training dataset, where 1 represents treated and 0 represents control.
#' @param y_train The vector of observed outcomes for the training dataset.
#' @return a list containing the fitted OLS model object
s.ols <- function(x_data, w_train, y_train){
  s.data <- x_data
  s.data['w'] <- w_train
  m1 <- lm(y_train ~ ., as.data.frame(s.data))
  return(list(m1 = m1))
}