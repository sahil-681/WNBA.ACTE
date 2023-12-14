library(Rforestry)
library(ggplot2)
library(dplyr)
library(splines)
library(pubtheme)
library(reshape2)
source("R/x.ols.R")
source("R/x.est.ols.R")

df <- readRDS("data/1_Cleaned_df.rds")

# Make factor
lfactor <- c("starter", "ejected", "did_not_play", "active", 
             "teamag", "season", "season.type", "pos", "country",
             "player")
df[, lfactor] <- lapply(df[, lfactor], as.factor)

lfeatures <- c("mins", "offensive_rebounds", "defensive_rebounds", "rebounds", 
               "assists", "steals", "blocks", "turnovers", "fouls", 
               "plus_minus", "points", "starter", "ejected", "did_not_play", 
               "active", "teamag", "season", "season.type", "ht", "wt", "pos", 
               "country", "rookie", "field_goals_made", "field_goals_attempted", 
               "three_point_field_goals_made", "three_point_field_goals_attempted", 
               "free_throws_made", "free_throws_attemped", "field_goals_pct", 
               "three_point_field_goals_pct", "free_throws_made_pct","player")
#all we can use
# Add splines and interaction term
df[, "trt"] <- as.numeric(df$rest_days > 1 | df$last_game_minutes < 20) 
lknots <- c(23, 25, 26, 28, 31)
spline <- ns(df$age, df = 6)
spline['knots']

dfns <- data.frame(ns(df$age, df = 6))
colnames(dfns) <- c("NS1", "NS2", "NS3", "NS4", "NS5", "NS6")

df <- cbind(df, dfns)
lfeatures <- c("NS1", "NS2", "NS3", "NS4", "NS5", "NS6", "player") 

trt <- "trt"
# If we want NN we should normalize the data. RF do not require normalizing
po <- "p40"

# Athlete with most games for consistency
lp <- as.character(unique(df$player))
lpn <- lp[table(as.character(df$player)) > 25]
df <- df[as.character(df$player) %in% lpn, ]
dim(df)

# Keep data only for players playing b2b
dfb2b0 <- df[df$trt == 0, ]
lb2b <- df$trt == 0
dfb2b1 <- df[c(lb2b[-1], FALSE), ]
dfb2b <- rbind(dfb2b0, dfb2b1)
dfb2b <- dfb2b[order(dfb2b$player, dfb2b$date), ]

n <- nrow(df)

luse <- sample(seq(1, n), n * 1)

x_train <- df[luse, lfeatures]
x_test <- df[-luse, lfeatures]
y_train <- df[luse, po]
y_test <- df[-luse, po]
w_train <- df[luse, trt]
w_test <- df[-luse, trt]

x_data <- x_train
y_data <- y_train
w_data <- w_train

# X_OLS
xmodel <- x.ols(x_data, w_train, y_train)
xres <- x.est.ols(xmodel, x_data)
df[, "cate"] <- xres

acte <- aggregate(cate ~ age + pos, df, mean)
acte %>% ggplot( aes(x = age, y = cate, group = pos, color = pos)) +
  geom_line() +
  theme_pub(type = "line", base_size = 36/3)


# Construct the curve
lplayer <- c("Diana Taurasi", "Sue Bird", "Erika de Souza")
lage <- seq(21, 42, 0.1)
lt <- as.factor(c(0, 1))
dfLBJ <- expand.grid(player = lplayer, age = lage, trt = lt)
dfns <- data.frame(ns(dfLBJ$age, knots = lknots))
colnames(dfns) <- c("NS1", "NS2", "NS3", "NS4", "NS5", "NS6")
dfLBJ <- cbind(dfLBJ, dfns)
dfLBJ[, "Pred"] <- x.est.ols(xmodel, dfLBJ)
#dfLBJ[, "Pred"] <- EstimateCate(xl_rf, dfLBJ[, lfeatures])
dfLBJ %>% 
  ggplot(aes(x=age, y = Pred, group = player, color = player)) +
  geom_line() +
  theme_pub(type = "line", base_size = 36/3)

# Linear models
# Try df=5, the final version should be more than 100 games
lmod <- lm(p40 ~ trt * ns(age, df = 6) + trt * player, df)
summary(lmod)

lplayer <- c("Diana Taurasi")
lage <- seq(20, 40, 0.1)
lt <- c(0, 1)
lpos <- c("Center", "Center-Forward", "Forward", "Forward-Center", 
          "Forward-Guard", "Guard", "Guard-Forward")

dfLBJ <- expand.grid(player = unique(df$player), 
                     age = lage, 
                     trt = lt)

dfLBJ[, "Pred"] <- predict(lmod, dfLBJ)
dfLBJ <- aggregate(Pred ~ trt + age, data = dfLBJ, FUN = mean)
dfLBJ$trt <- as.factor(dfLBJ$trt)
dfLBJ$trtstr <- "Back-to-back"
dfLBJ[dfLBJ$trt == 1, "trtstr"] <- "1+ Days Rest"
dfLBJ %>% 
  ggplot(aes(x=age, y = Pred, group = trtstr, color = trtstr)) +
  geom_line() +
  labs(title = "Conditional Expectation Function",
       subtitle = 'CEF for Diana Taurasi',
       x = 'Age', 
       y = 'Points per 40 minutes') +  
  scale_x_continuous() + 
  scale_y_continuous() + 
  coord_cartesian(clip='off', expand=FALSE) +
  theme_pub(type='line', base_size=36/3) 

# Same with dfb2b
lmod <- lm(p40 ~ trt * ns(age, df = 6) + trt * player, dfb2b)
summary(lmod)

lplayer <- c("Diana Taurasi")
lage <- seq(22, 40, 0.1)
lt <- c(0, 1)
lpos <- c("Center", "Center-Forward", "Forward", "Forward-Center", 
          "Forward-Guard", "Guard", "Guard-Forward")

dfLBJ <- expand.grid(player = unique(dfb2b$player), age = lage, trt = lt)

dfLBJ[, "Pred"] <- predict(lmod, dfLBJ)
dfLBJ <- aggregate(Pred ~ trt + age, data = dfLBJ, FUN = mean)
dfLBJ$trt <- as.factor(dfLBJ$trt)
dfLBJ$trtstr <- "Back-to-back"
dfLBJ[dfLBJ$trt == 1, "trtstr"] <- "1+ Days Rest"
dfLBJ %>% 
  ggplot(aes(x=age, y = Pred, group = trtstr, color = trtstr)) +
  geom_line() +
  labs(title = " Conditional Expectation Function for Points",
       subtitle = 'Averaged over all players with 100 games played',
       x = 'Age', 
       y = 'Points per 40 minutes') +  
  scale_x_continuous() + 
  scale_y_continuous() + 
  coord_cartesian(clip='off', expand=FALSE) +
  theme_pub(type='line', base_size=36/3) 

# Use XGboost much faster
dfLBJw <- dcast(dfLBJ[, c("age", "trtstr", "Pred")], age ~ trtstr)
dfLBJw$Diff <- dfLBJw$`1+ Days Rest` - dfLBJw$`Back-to-back`
title = "Age Conditioned Treatment Effect" 
dfLBJw %>% 
  ggplot(aes(x = age, y = Diff)) +
  geom_line() +
  labs(title = title,
       subtitle = 'Average ACTE',
       x = 'Age', 
       y = 'ACTE') +  
  scale_x_continuous() + 
  scale_y_continuous() + 
  coord_cartesian(clip='off', expand=FALSE) +
  theme_pub(type='line', base_size=36/3) 

