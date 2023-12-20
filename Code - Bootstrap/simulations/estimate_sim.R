library(ggplot2)
library(splines)
library(Rforestry)
source("Code/X_RF_OLS.R")
library(dplyr)
library(tidyr)
sim <- 1
dfF <- readRDS(paste0("data/simulations", as.character(sim), ".rds"))

# Add splines
nspline <- 5
spline <- ns(dfF$age, df = nspline)
dfns <- data.frame(ns(dfF$age, df = nspline))
spcols <- paste0("NS", seq(1,nspline))
colnames(dfns) <- paste0("NS", seq(1,nspline))
dfF <- cbind(dfF, dfns)

# Estimation
po <- "y"; trt <- "w"
x_data <- dfF[, !(names(dfF) %in% c(po, trt, "age"))]; y_data <- dfF[, po]; 
w_data <- dfF[, trt]

smodel.ols <- s.ols(x_data, w_data, y_data)
s.cates.ols <- s.est.ols(smodel.ols, x_data)

tmodel.ols <- t.ols(x_data, w_data, y_data)
t.cates.ols <- t.est.ols(tmodel.ols, x_data)

xmodel.ols <- x.ols(x_data, w_data, y_data)
x.cates.ols <- x.est.ols(xmodel.ols, x_data)

smodel.rf <- s.rf(x_data, w_data, y_data)
s.cates.rf <- s.est.rf(smodel.rf, x_data)

tmodel.rf <- t.rf(x_data, w_data, y_data)
t.cates.rf <- t.est.rf(tmodel.rf, x_data)

xmodel.rf <- x.rf(x_data, w_data, y_data)
x.cates.rf <- x.est.rf(xmodel.rf, x_data)

cates <- cbind(s.cates.ols, t.cates.ols, x.cates.ols, s.cates.rf, t.cates.rf, x.cates.rf)
colnames(cates) <- c('s.cates.ols', 't.cates.ols', 'x.cates.ols', 's.cates.rf', 
                     't.cates.rf', 'x.cates.rf')

dfF <- cbind(dfF,cates)
if (sim == 1){
  dfF$cate.oracle <- 2
} else if (sim == 2){
  dfF$cate.oracle <- 1/10*(dfF$age - 18)
} else if (sim == 3){
  # dfF$cate.oracle <- 1/3*(dfF$age - 25)
  t <- dfF$age; tm <- 25
  dfF$cate.oracle <- (2*(t - 16) + 0.0005*(t > 20)*(t - tm)^3 - 0.0005*(t > tm)*(t - tm)^4)
}

# Reshape the data to long format
df_long <- dfF %>%
  pivot_longer(cols = contains("cate"), names_to = "variable", values_to = "value")

df.cate <- df_long %>%
  group_by(age, variable) %>%
  summarise(value = mean(value), .groups = "drop") 

# Create a plot
ggplot(df.cate, aes(x = age, y = value, group = variable, color = variable)) +
  geom_line(alpha = 0.7) +  # Adjust width for spacing
  labs(x = "Age", y = "Value") +
  theme_minimal() +
  scale_color_manual(values = c("purple", "brown1", "dodgerblue", "brown2", "steelblue", "brown3", "mediumblue"))

# Calculate MSE
df_diff <- dfF %>%
  select(matches("cate")) %>%
  transmute(across(everything(), ~(. - dfF$cate.oracle)^2, .names = "mse.{.col}"))
df_diff$age <- dfF$age

df_long <- df_diff %>%
  pivot_longer(cols = contains("mse."), names_to = "variable", values_to = "value")
df.mse <- df_long %>%
  group_by(age, variable) %>%
  summarise(value = mean(value), .groups = "drop") 

df.mse <- df.mse %>%
  filter(variable != "mse.cate.oracle")

gg <- ggplot(df.mse, aes(x = age, y = value, group = variable, color = variable)) +
  geom_line(alpha = 0.7) +  # Adjust width for spacing
  labs(x = "Age", y = "Value") +
  theme_pub(type='line', base_size=36/3, colors = 'cb14') + 
  scale_color_manual(values = c("brown1", "dodgerblue", "maroon", 
                                "steelblue", "brown3", "mediumblue"))
ggsave(filename = paste0("output/Image/mse_simulation", as.character(sim), ".pdf"), plot = gg,
       device = 'pdf')
gg
df.mse.agg <- df_long %>%
  group_by(variable) %>%
  summarise(value = mean(value), .groups = "drop") 
write.csv(df.mse.agg, paste0("output/mse_simulation", as.character(sim), ".csv"))
df.mse.agg[order(df.mse.agg$value),]
