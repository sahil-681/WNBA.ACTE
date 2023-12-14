#' Function to get T-learner using forestry package
#' @param data.x covariate matrix for athletes in the dataset
#' @param data.t treatment assignment vector where each entry indicates
#'               whether the athlete has received treatment (1) or not (0)
#' @param data.y outcome vector for athletes in dataset
#' @return a list containing the trained T-learner model
t.rf <- function(data.x, data.t, data.y){
  x.t <- data.x[data.t == 1, ]
  x.c <- data.x[data.t == 0, ]
  
  m1 <- forestry(x = x.c, y = data.y[data.t == 0])
  m2 <- forestry(x = x.t, y = data.y[data.t == 1])
  return(list(m1 = m1, m2 = m2))
}