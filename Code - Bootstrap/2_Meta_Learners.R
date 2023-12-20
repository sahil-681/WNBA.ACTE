library(Rforestry)
library(causalToolbox)
library(ggplot2)
library(dplyr)
library(splines)
library(orgthemes)
library(reshape2)
setwd("~/GitHub/ACTE/Code")
source("X_RF_OLS.R")
packageVersion("causalToolbox")
setwd("~/GitHub/ACTE/Data")
df <- readRDS("1_Cleaned_df.rds")
df <- df[complete.cases(df), ]
# Make factor
lfactor <- c("starter", "ejected", "did_not_play", "active", 
             "team_abbreviation", "season", "season_type", "pos", "country",
             "athlete_display_name")
df[, lfactor] <- lapply(df[, lfactor], as.factor)

lfeatures <- c("min", "oreb", "dreb", "reb", "ast", "stl", "blk", "to", "pf", 
               "plus_minus", "pts", "starter", "ejected", "did_not_play", 
               "active", "team_abbreviation",
               "season", "season_type", "ht", "wt", "pos", "country", "rookie",
               "fgM", "fgA", "fg3M", "fg3A", "ftM", "ftA", "fgp", "fg3p", "ftp",
               "athlete_display_name")
#all we can use
# Add splines and interaction term
df[, "trt"] <- as.numeric(df$RDays > 1 | df$lagmin < 20) 
lknots <- c(23, 25, 26, 28, 31)
spline <- ns(df$age, df = 6)
spline['knots']

dfns <- data.frame(ns(df$age, df = 6))
colnames(dfns) <- c("NS1", "NS2", "NS3", "NS4", "NS5", "NS6")

df <- cbind(df, dfns)
lfeatures <- c("NS1", "NS2", "NS3", "NS4", "NS5", "NS6", 
               "athlete_display_name") 


trt <- "trt"
# If we want NN we should normalize the data. RF do not require normalizing
po <- "p48"





# Athlete with most games for consistency
lp <- as.character(unique(df$athlete_display_name))
lpn <- lp[table(as.character(df$athlete_display_name)) > 25]
df <- df[as.character(df$athlete_display_name) %in% lpn, ]
#df <- df[df$athlete_display_name != "LeBron James", ]
dim(df)

# Keep data only for players playing b2b
dfb2b0 <- df[df$trt == 0, ]
lb2b <- df$trt == 0
dfb2b1 <- df[c(lb2b[-1], FALSE), ]
dfb2b <- rbind(dfb2b0, dfb2b1)
dfb2b <- dfb2b[order(dfb2b$athlete_display_name, dfb2b$game_date), ]
a


n <- nrow(df)

luse <- sample(seq(1, n), n * 1)

x_train <- df[luse, lfeatures]
x_test <- df[-luse, lfeatures]
y_train <- df[luse, po]
y_test <- df[-luse, po]
w_train <- df[luse, trt]
w_test <- df[-luse, trt]
x_data <- x_train; y_data <- y_train; w_data <- w_train

# X_RF
# xl_rf <- X_RF(feat = x_train, tr = w_train, yobs = y_train)
# setwd("~/GitHub/ACTE/Output")
# saveRDS(xl_rf, "xl_rf2010-2021.rds")
# #xl_rf <- readRDS("xl_rf2016-2021.rds")
# cate_esti_rf <- EstimateCate(xl_rf, df[, lfeatures])
# df[, "cate"] <- cate_esti_rf

# X_OLS
a
xmodel <- x.ols(x_data, w_train, y_train)
xres <- x.est.ols(xmodel, x_data)
df[, "cate"] <- xres$cate

acte <- aggregate(cate ~ age + pos, df, mean)
acte %>% ggplot( aes(x=age, y = cate, group = pos, color = pos)) +
  geom_line()


# Construct the curve
lplayer <- c("James Harden", "DeMar DeRozan", 
             "Giannis Antetokounmpo")
lage <- seq(21, 42, 0.1)
lt <- as.factor(c(0, 1))
dfLBJ <- expand.grid(athlete_display_name = lplayer, age = lage, trt = lt)
dfns <- data.frame(ns(dfLBJ$age, knots = lknots))
colnames(dfns) <- c("NS1", "NS2", "NS3", "NS4", "NS5", "NS6")
dfLBJ <- cbind(dfLBJ, dfns)
dfLBJ[, "Pred"] <- x.est.ols(xmodel, dfLBJ)$cate
#dfLBJ[, "Pred"] <- EstimateCate(xl_rf, dfLBJ[, lfeatures])
dfLBJ %>% ggplot(aes(x=age, y = Pred, 
                      group = athlete_display_name, 
                      color = athlete_display_name)) +
  geom_line()

a
# Linear models
# Try df=5, the final version should be more than 100 games
lmod <- lm(p48 ~ trt*ns(age, df = 6) + trt*athlete_display_name, df)
summary(lmod)

lplayer <- c("LeBron James")
lage <- seq(20, 40, 0.1)
lt <- c(0, 1)
lpos <- c("Center", "Center-Forward", "Forward", "Forward-Center", 
          "Forward-Guard", "Guard", "Guard-Forward")

dfLBJ <- expand.grid(athlete_display_name = unique(df$athlete_display_name),
                     age = lage, trt = lt)

dfLBJ[, "Pred"] <- predict(lmod, dfLBJ)
dfLBJ <- aggregate(Pred ~ trt + age, data = dfLBJ, FUN = mean)
dfLBJ$trt <- as.factor(dfLBJ$trt)
dfLBJ$trtstr <- "Back-to-back"
dfLBJ[dfLBJ$trt == 1, "trtstr"] <- "1+ Days Rest"
dfLBJ %>% ggplot(aes(x=age, y = Pred, 
                      group = trtstr, 
                      color = trtstr)) +
  geom_line() +
  labs(title    = "Conditional Expectation Function",
       subtitle = 'CEF for LeBron James',
       x = 'Age', 
       y = 'Points per 48 minutes') +  
  scale_x_continuous() + 
  scale_y_continuous() + 
  coord_cartesian(clip='off', expand=FALSE) +
  theme_org(type='line', base_size=36/3) 

# Same with dfb2b
lmod <- lm(p48 ~ trt*ns(age, df = 6) + trt*athlete_display_name, dfb2b)
summary(lmod)

lplayer <- c("LeBron James")
lage <- seq(22, 40, 0.1)
lt <- c(0, 1)
lpos <- c("Center", "Center-Forward", "Forward", "Forward-Center", 
          "Forward-Guard", "Guard", "Guard-Forward")

dfLBJ <- expand.grid(athlete_display_name = unique(dfb2b$athlete_display_name),
                     age = lage, trt = lt)

dfLBJ[, "Pred"] <- predict(lmod, dfLBJ)
dfLBJ <- aggregate(Pred ~ trt + age, data = dfLBJ, FUN = mean)
dfLBJ$trt <- as.factor(dfLBJ$trt)
dfLBJ$trtstr <- "Back-to-back"
dfLBJ[dfLBJ$trt == 1, "trtstr"] <- "1+ Days Rest"
dfLBJ %>% ggplot(aes(x=age, y = Pred, 
                     group = trtstr, 
                     color = trtstr)) +
  geom_line() +
  labs(title    = " Conditional Expectation Function for Points",
       subtitle = 'Averaged over all players with 100 games played',
       x = 'Age', 
       y = 'Points per 48 minutes') +  
  scale_x_continuous() + 
  scale_y_continuous() + 
  coord_cartesian(clip='off', expand=FALSE) +
  theme_org(type='line', base_size=36/3) 

# Use xgboost much faster
dfLBJw <- dcast(dfLBJ[, c("age", "trtstr", "Pred")], age ~ trtstr)
dfLBJw$Diff <- dfLBJw$`1+ Days Rest` - dfLBJw$`Back-to-back`
title = "Age Conditioned Treatment Effect" 
dfLBJw %>% ggplot(aes(x=age, y = Diff)) +
  geom_line() +
  labs(title    = title,
       subtitle = 'Average ACTE',
       x = 'Age', 
       y = 'ACTE') +  
  scale_x_continuous(labels=comma) + 
  scale_y_continuous(labels=comma) + 
  coord_cartesian(clip='off', expand=FALSE) +
  theme_org(type='line', base_size=36/3) 

