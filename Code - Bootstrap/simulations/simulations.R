library(ggplot2)
library(pubtheme)
library(Rforestry)
library(tidyr)
source("Code/X_RF_OLS.R")
# Realize simulations
# Simulation from Schukers
#' Fixed treatment effect
################################################################################
# Simulation 1
################################################################################
# Treatment effect is constant
g1 <- function(t, w){
  return(omega + b1*(t - tm)^2 + b2*(t - tm)^2*(t > tm) + b3*(t - tm)^3*(t > tm) + 2*w)
}

f1 <- function(x, t, w){
  return(rnorm(1, 0 , s_gamma) + bi*(t - tm)^2*(t > tm))
}

getY1 <- function(x, t){
  wi <- rbinom(N, 1, 0.133)
  Yi <- g1(t, wi) + f1(x, t, wi) + rnorm(N, 0, sig_e)
  return(list(Y = Yi, W = wi))
}

tm <- 25; b1 <- -1/9; b2 <- -6/1000; b3 <- 45/10000; sig_b <- 0.02; sig_e <- 1
N <- 1000; omega <- 0; s_gamma <- 0.4;
bi <- rnorm(N, 0, sig_b)
Y <- getY1(x, 30)
lt <- seq(18, 40)
lall <- list()
i <- 1
for (t in lt){
  Y1 <- getY1(x, t)
  dft <- data.frame(player = factor(seq(1, N)), age = t, w = Y1$W,y = Y1$Y)
  lall[[i]] <- dft
  print(dim(dft))
  i <- i + 1
}

dfF <- do.call(rbind, lall)
# Remove players without overlap
dfF <- ungroup(dfF %>% group_by(player) %>% filter(any(w == 0) && any(w==1)))
dfF <- data.frame(dfF)
# Create a plot
ggplot(dfF, aes(x = age, y = y, group = player, color = player)) +
  geom_line(alpha = 0.7) +  # Adjust width for spacing
  scale_color_manual(values = c(rep("gray", length(unique(dfF$player)) - 1), "red")) + 
  labs(x = "Age", y = "Value") +
  theme_minimal() +
  theme(legend.position = "none")  # Remove the legend

dfg <- dfF %>% group_by(age, w) %>% reframe(average = g1(age, w), sd = sd(y), 
                                              .groups = "drop")
# Create a ribbon plot
gg <- ggplot(dfg, aes(x = age, y = average, fill = as.factor(w))) +
  geom_ribbon(aes(ymin = average - sd, ymax = average + sd), alpha = 0.5) +
  geom_line(aes(color = as.factor(w)), size = 0.5) +
  scale_color_manual(values = c("maroon", "royalblue4"), name = "") +  # Set line colors
  scale_fill_manual(values = c("brown1", "turquoise"), name = "") +
  labs(x = "Age", y = "Value", fill = NULL, linetype = "W") +
  theme_pub(type='line', base_size=36/3, colors = 'cb14')
ggsave(filename = paste0("output/Image/Simulation1.pdf"), plot = gg,
       device = 'pdf')
gg
saveRDS(dfF, "data/simulations1.rds")

################################################################################
# Simulation 2
################################################################################
#' Treatment effect depends on age
g1 <- function(t, w){
  return(omega + b1*(t - tm)^2 + b2*(t - tm)^2*(t > tm) + b3*(t - tm)^3*(t > tm) + 1/10*(t - 18)*w)
}

f1 <- function(x, t, w){
  return(rnorm(1, 0 , s_gamma) + bi*(t - tm)^2*(t > tm))
}

getY1 <- function(x, t){
  wi <- rbinom(N, 1, 0.133)
  Yi <- g1(t, wi) + f1(x, t, wi) + rnorm(N, 0, sig_e)
  return(list(Y = Yi, W = wi))
}

bi <- rnorm(N, 0, sig_b)
Y <- getY1(x, 30)
lt <- seq(18, 40)
lall <- list()
i <- 1
for (t in lt){
  Y1 <- getY1(x, t)
  dft <- data.frame(player = factor(seq(1, N)), age = t, w = Y1$W,y = Y1$Y)
  lall[[i]] <- dft
  i <- i + 1
}

dfF <- do.call(rbind, lall)
# Remove players without overlap
dfF <- ungroup(dfF %>% group_by(player) %>% filter(any(w == 0) && any(w==1)))
dfF <- data.frame(dfF)
# Create a plot
ggplot(dfF, aes(x = age, y = y, group = player, color = player)) +
  geom_line(alpha = 0.7) +  # Adjust width for spacing
  scale_color_manual(values = c(rep("gray", length(unique(dfF$player)) - 1), "red")) + 
  labs(x = "Age", y = "Value") +
  theme_minimal() +
  theme(legend.position = "none")  # Remove the legend

dfg <- dfF %>% group_by(age, w) %>% reframe(average = g1(age, w), sd = sd(y), 
                                            .groups = "drop") 
gg <- ggplot(dfg, aes(x = age, y = average, fill = as.factor(w))) +
  geom_ribbon(aes(ymin = average - sd, ymax = average + sd), alpha = 0.5) +
  geom_line(aes(color = as.factor(w)), size = 0.5) +
  scale_color_manual(values = c("maroon", "royalblue4"), name = "") +  # Set line colors
  scale_fill_manual(values = c("brown1", "turquoise"), name = "") +
  labs(x = "Age", y = "Value", fill = NULL, linetype = "W") +
  theme_pub(type='line', base_size=36/3, colors = 'cb14')
gg
ggsave(filename = paste0("output/Image/Simulation2.pdf"), plot = gg,
       device = 'pdf')
saveRDS(dfF, "data/simulations2.rds")

################################################################################
# Simulation 3
################################################################################
#' Treatment effect depends on age and there is confounding and high imbalance
g1 <- function(t, w){
  return(omega + b1*(t - tm)^2 + b2*(t - tm)^2*(t > tm) + b3*(t - tm)^3*(t > tm) + 
           (2*(t - 16) + 0.0005*(t > 20)*(t - tm)^3 - 0.0005*(t > tm)*(t - tm)^4)*w)
}

f1 <- function(x, t, w){
  return(rnorm(1, 0 , s_gamma) + bi*(t - tm)^2*(t > tm) - w*x*5/(1+exp(-(t - 25))))
}

getY1 <- function(x, t){
  wi <- rbinom(N, 1, 0.01)
  xi <- runif(N, -1, 1)
  Yi <- g1(t, wi) + f1(xi, t, wi) + rnorm(N, 0, sig_e)
  return(list(Y = Yi, W = wi, D = xi))
}

bi <- rnorm(N, 0, sig_b)
Y <- getY1(x, 30)
lt <- seq(18, 40)
lall <- list()
i <- 1
for (t in lt){
  Y1 <- getY1(x, t)
  dft <- data.frame(player = factor(seq(1, N)), age = t, w = Y1$W,y = Y1$Y, defence = Y1$D)
  lall[[i]] <- dft
  i <- i + 1
}

dfF <- do.call(rbind, lall)
# Remove players without overlap
dfF <- ungroup(dfF %>% group_by(player) %>% filter(any(w == 0) && any(w==1)))
dfF <- data.frame(dfF)
# Create a plot
ggplot(dfF, aes(x = age, y = y, group = player, color = player)) +
  geom_line(alpha = 0.7) +  # Adjust width for spacing
  scale_color_manual(values = c(rep("gray", length(unique(dfF$player)) - 1), "red")) + 
  labs(x = "Age", y = "Value") +
  theme_minimal() +
  theme(legend.position = "none")  # Remove the legend

dfg <- dfF %>% group_by(age, w) %>% reframe(average = g1(age, w), sd = sd(y), 
                                            .groups = "drop")
gg <- ggplot(dfg, aes(x = age, y = average, fill = as.factor(w))) +
  geom_ribbon(aes(ymin = average - sd, ymax = average + sd), alpha = 0.5) +
  geom_line(aes(color = as.factor(w)), size = 0.5) +
  scale_color_manual(values = c("maroon", "royalblue4"), name = "") +  # Set line colors
  scale_fill_manual(values = c("brown1", "turquoise"), name = "") +
  labs(x = "Age", y = "Value", fill = NULL, linetype = "W") +
  theme_pub(type='line', base_size=36/3, colors = 'cb14')
gg
ggsave(filename = paste0("output/Image/Simulation3.pdf"), plot = gg,
       device = 'pdf')
saveRDS(dfF, "data/simulations3.rds")

