#' Function to get S-learner using forestry package
#' @param data.x covariate matrix for athletes in the dataset
#' @param data.t treatment assignment vector where each entry indicates
#'               whether the athlete has received treatment (1) or not (0)
#' @param data.y outcome vector for athletes in dataset
#' @return a list containing the trained S-learner model
#' @export
s.rf <- function(data.x, data.t, data.y){
  s.data <- data.x
  s.data['w'] <- data.t
  m1 <- forestry(x = s.data, y = data.y)
  return(list(m1 = m1))
}