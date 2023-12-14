# X-learner for OLS
#' @param x_data The matrix of covariates for the observations in the training dataset.
#' @param w_train The vector indicating treatment assignment for each observation in the training dataset,
#'                where 1 represents treated and 0 represents control.
#' @param y_train The vector of observed outcomes for the training dataset.
#' @return A list containing the fitted OLS model object.
#' @export
x.ols <- function(x_data, w_train, y_train){
  xt <- x_data[w_data == 1, ]
  xc <- x_data[w_data == 0, ]
  yt <- y_data[w_data == 1]
  yc <- y_data[w_data == 0]
  m1 <- lm(yc ~ ., as.data.frame(xc))
  m2 <- lm(yt ~ ., as.data.frame(xt))
  # If new level in test set add to the model m1 and m2
  
  d1 <- yt - predict(m1, xt)
  d0 <- predict(m2, xc) - yc
  
  m3 <- lm(d1 ~ ., as.data.frame(xt))
  m4 <- lm(d0 ~ ., as.data.frame(xc))
  
  g <- glm(w_data ~ ., data = x_data)
  return(list(m3 = m3, m4 = m4, g = g))
}