#############################################
# RF
#############################################
# S-learner for RF
s.rf <- function(data.x, data.t, data.y){
  s.data <- data.x
  s.data['w'] <- data.t
  m1 <- forestry(x = s.data, y = data.y)
  return(list(m1 = m1))
}

#' Function to estimate CATE
#' @param xmodel x-learner model
#' @param data.x data containing covariate features
#' @param data.t vector of treatment indicators
#' @return CATE estimates
s.est.rf <- function(xmodel, data.x){
  data.x['w'] <- 0
  mu0 <- predict(xmodel$m1, data.x)
  data.x['w'] <- 1
  mu1 <- predict(xmodel$m1, data.x)
  tau <- mu1 - mu0
  return(tau)
}

# T-learner for RF
t.rf <- function(data.x, data.t, data.y){
  x.t <- data.x[data.t == 1, ]
  x.c <- data.x[data.t == 0, ]
  
  m1 <- forestry(x = x.c, y = data.y[data.t == 0])
  m2 <- forestry(x = x.t, y = data.y[data.t == 1])
  return(list(m1 = m1, m2 = m2))
}

#' Function to estimate CATE
#' @param xmodel x-learner model
#' @param data.x data containing covariate features
#' @param data.t vector of treatment indicators
#' @return CATE estimates
t.est.rf <- function(xmodel, data.x){
  mu0 <- predict(xmodel$m1, data.x)
  mu1 <- predict(xmodel$m2, data.x)
  tau <- mu1 - mu0
  return(tau)
}

# X-learner for RF
x.rf <- function(data.x, data.t, data.y){
  x.t <- data.x[data.t == 1, ]
  x.c <- data.x[data.t == 0, ]
  
  m1 <- forestry(x = x.c, y = data.y[data.t == 0])
  m2 <- forestry(x = x.t, y = data.y[data.t == 1])
  
  d1 <- data.y[data.t == 1] - predict(m1, x.t)
  d0 <- predict(m2, x.c) - data.y[data.t == 0]
  
  m3 <- forestry(x = x.t, y = d1)
  m4 <- forestry(x = x.c, y = d0)
  
  g <- forestry(x = data.x, y = data.t)
  return(list(m3 = m3, m4 = m4, g = g))
}

#' Function to estimate CATE
#' @param xmodel x-learner model
#' @param data.x data containing covariate features
#' @param data.t vector of treatment indicators
#' @return CATE estimates
x.est.rf <- function(xmodel, data.x){
  tau1 <- predict(xmodel$m3, data.x)
  tau0 <- predict(xmodel$m4, data.x)
  pscore <- predict(xmodel$g, data.x)
  tau <- pscore * tau0 + (1-pscore) * tau1
  return(tau)
}

#############################################
# OLS
#############################################
# S-learner for OLS
s.ols <- function(x_data, w_train, y_train){
  s.data <- x_data
  s.data['w'] <- w_train
  m1 <- lm(y_train ~ ., as.data.frame(s.data))
  return(list(m1 = m1))
}

#' Function to estimate CATE
#' @param xmodel x-learner model
#' @param data.x data containing covariate features
#' @param data.t vector of treatment indicators
#' @return CATE estimates
s.est.ols <- function(xmodel, x_data){
  x_data['w'] <- 0
  mu0 <- predict(xmodel$m1, x_data)
  x_data['w'] <- 1
  mu1 <- predict(xmodel$m1, x_data)
  tau <- mu1 - mu0
  return(tau)
}

# T-learner for OLS
t.ols <- function(x_data, w_train, y_train){
  xt <- x_data[w_data == 1, ]
  xc <- x_data[w_data == 0, ]
  yt <- y_data[w_data == 1]
  yc <- y_data[w_data == 0]
  m1 <- lm(yc ~ ., as.data.frame(xc))
  m2 <- lm(yt ~ ., as.data.frame(xt))
  return(list(m1 = m1, m2 = m2))
}

#' Function to estimate CATE
#' @param xmodel x-learner model
#' @param data.x data containing covariate features
#' @param data.t vector of treatment indicators
#' @return CATE estimates
t.est.ols <- function(xmodel, x_data){
  mu0 <- predict(xmodel$m1, x_data)
  mu1 <- predict(xmodel$m2, x_data)
  tau <- mu1 - mu0
  return(tau)
}

# X-learner for OLS
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

#' Function to estimate CATE
#' @param xmodel x-learner model
#' @param data.x data containing covariate features
#' @param data.t vector of treatment indicators
#' @return CATE estimates
x.est.ols <- function(xmodel, x_data){
  tau1 <- predict(xmodel$m3, x_data)
  tau0 <- predict(xmodel$m4, x_data)
  pscore <- predict(xmodel$g, x_data)
  tau <- pscore * tau0 + (1-pscore) * tau1
  return(tau)
}


