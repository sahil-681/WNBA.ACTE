# T-learner for OLS
#' @param x_data Matrix of covariates for the observations in the training set.
#' @param w_train The vector indicating treatment assignment for each observation 
#'                in the training dataset, where 1 represents treated and 0 represents control.
#' @param y_train The vector of observed outcomes for the training dataset.
#' @return a list containing the fitted OLS model objects
t.ols <- function(x_data, w_train, y_train){
  xt <- x_data[w_data == 1, ]
  xc <- x_data[w_data == 0, ]
  yt <- y_data[w_data == 1]
  yc <- y_data[w_data == 0]
  m1 <- lm(yc ~ ., as.data.frame(xc))
  m2 <- lm(yt ~ ., as.data.frame(xt))
  return(list(m1 = m1, m2 = m2))
}