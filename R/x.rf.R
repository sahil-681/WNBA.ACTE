# X-learner for RF
#' @param data.x covariate matrix for athletes in the dataset
#' @param data.t treatment assignment vector where each entry indicates
#' whether the athlete has received treatment (1) or not (0)
#' @param data.y outcome vector for athletes in dataset
#' @return a list containing the models that estimate the treatment effects
#' for the treated groups, control groups, and propensity score estimation
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