library(Rforestry)
#library(causalToolbox)
library(ggplot2)
library(dplyr)
library(splines)
library(pubtheme)
library(reshape2)
source("Code/X_RF_OLS.R")
# packageVersion("causalToolbox")
# 100 possessions
cb.pal =  c("#004949","#009292","#ff6db6","#ffb6db",
            "#490092","#006ddb","#b66dff","#6db6ff","#b6dbff",
            "#920000","#924900","#db6d00","#24ff24","#ffff6d")

po <- "net.rat"; b2bdata <- "nob2b"
filename <- paste0(po, b2bdata)
yvars <- c("net.rat", "off.rat", "def.rat", "p100", "a100", "r100", "s100", 
           "b100", "to100", "fgp", "ftp", "oreb.p", "dreb.p", "ts.p",
           "or100", "dr100")
yvars <- c("net.rat")
if (b2bdata == "b2b"){
  df <- readRDS("Data/1_Cleaned_dfb2b.rds")
} else{
  df <- readRDS("Data/1_Cleaned_df.rds")
}

# Add splines
nspline <- 6
spline <- ns(df$age, df = nspline)
dfns <- data.frame(ns(df$age, df = nspline))
spcols <- paste0("NS", seq(1,nspline))
colnames(dfns) <- paste0("NS", seq(1,nspline))
df <- cbind(df, dfns)

# Get consistent players
df <- df[df$age < 39 & df$age > 18, ]
lp <- sort(as.character(unique(df$player)))
lplayer <- lp[table(as.character(df$player)) > 500]
lplayer

df <- df[df$player %in% lplayer, ]

# Change teamnames
df[df$team == "NJN", "team"] <- "BKN"
df[df$teamag == "NJN", "teamag"] <- "BKN"

# Define features
lfeatures <- c(spcols, "team", "player", "teamag", "homeind", "season") 
# Make factor
lfactor <- c("player", "team", "season.type", "country", "away", "home",
             "teamag", "homeind", "season")
df[, lfactor] <- lapply(df[, lfactor], as.factor)
n <- nrow(df)
trt <- "trt"; 

# Create the grid
lage <- seq(21, 38, 1); lt <- as.factor(c(1)); 
lteam <- unique(df[df$season == "2020", "teamag"])
lteamag <- unique(df[df$season == "2020", "teamag"])
lhomeind <- c("TRUE", "FALSE"); lseason <- sort(unique(df$season))

for (po in yvars){
  # Start for loop here
  luse <- sample(seq(1, n), n * 1)
  x_data <- df[luse, lfeatures]; y_data <- df[luse, po]; w_data <- df[luse, trt]
  
  # X_OLS
  xmodel <- x.ols(x_data, w_train, y_train)
  #saveRDS(xmodel, file = paste0("Output/Model/", po, "GridXOLS1003.rds"))
  #print(po)
  for (s in lseason){
    df.grid <- expand.grid(player = lplayer, age = lage, trt = lt, team = lteam,
                           teamag = lteamag, homeind = lhomeind,
                           season = as.character(s))
    dfns <- data.frame(ns(df.grid$age, df = nspline))
    colnames(dfns) <- spcols
    df.grid <- cbind(df.grid, dfns)
    cates <- x.est.ols(xmodel, df.grid)
    df.grid[, paste0("Pred.", po)] <- cates$cate
    print(s, po)
    saveRDS(df.grid, file = paste0("Output/Model/", po, s, b2bdata, "GridXOLS102.rds"))
  }
}



df.grid <- readRDS(paste0("Output/Model/", po, s, b2bdata, "GridXOLS102.rds"))
head(df.grid)
# df.grid[, paste0("Pred.", yvars)] <- scale(df.grid[, paste0("Pred.", yvars)])

lvars <- c("net.rat")
lvars <- c("age", paste0("Pred.", lvars))
dft <- df.grid[, lvars]
dft <- aggregate(. ~ age, dft, FUN = mean)
lfvar <- c("age", "Net Rating")
colnames(dft) <- lfvar
dfp <- melt(dft[, lfvar], id.vars = "age")
xolsp <- dfp %>% ggplot(aes(x=age, y = value, group = variable,
                            color = variable)) +
  geom_line() + 
  labs(title    = "X-learner (Spline Regression)",
       subtitle = 'Average Rate Statistics ACTE', x = 'Age', 
       y = "Outcome") +  
  
  scale_x_continuous() + 
  scale_y_continuous() + 
  coord_cartesian(clip='off', expand=FALSE)
xolsp

lvars <- c("a100", "r100", "s100", "b100", "to100")
lvars <- c("age", paste0("Pred.", lvars))
dft <- df.grid[, lvars]
dft <- aggregate(. ~ age, dft, FUN = mean)
lfvar <- c("age", "AST100", "REB100", "STL100", "BLT100", "TO100")
colnames(dft) <- lfvar
dfp <- melt(dft[, lfvar], id.vars = "age")
xolsp <- dfp %>% ggplot(aes(x=age, y = value, group = variable,
                            color = variable)) +
  geom_line() + 
  labs(title    = "X-learner (Spline Regression)",
       subtitle = 'Average Box Statistics ACTE', x = 'Age', 
       y = "Outcome") +  
  
  scale_x_continuous() + 
  scale_y_continuous() + 
  coord_cartesian(clip='off', expand=FALSE) +
  theme_org(type='line', base_size=36/3, colors = 'cb14') 
xolsp


lvars <- c( "fgp", "ftp", "ts.p")
lvars <- c("age", paste0("Pred.", lvars))
dft <- df.grid[, lvars]
dft <- aggregate(. ~ age, dft, FUN = mean)
dfp <- melt(dft[, c(lvars)], id.vars = "age")
xolsp <- dfp %>% ggplot(aes(x=age, y = value, group = variable,
                            color = variable)) +
  geom_line() + labs(title    = "X-learner (Spline Regression)",
                     subtitle = 'Standardized ACTE', x = 'Age', 
                     y = "Standardized Outcome") +  
  scale_x_continuous() + 
  scale_y_continuous() + 
  coord_cartesian(clip='off', expand=FALSE) +
  theme_org(type='line', base_size=36/3, colors = 'cb14') 
xolsp



lvars <- c("oreb.p", "dreb.p", "or100", "dr100")
lvars <- c("age", paste0("Pred.", lvars))
dft <- df.grid[, lvars]
dfp <- melt(dft[, c(lvars)], id.vars = "age")
xolsp <- dfp %>% ggplot(aes(x=age, y = value, group = variable,
                            color = variable)) +
  geom_line() + labs(title    = "X-learner (Spline Regression)",
                     subtitle = 'Standardized ACTE', x = 'Age', 
                     y = "Standardized Outcome") +  
  scale_x_continuous() + 
  scale_y_continuous() + 
  coord_cartesian(clip='off', expand=FALSE) +
  theme_org(type='line', base_size=36/3, colors = 'cb14') 
xolsp


## or alpha=0.3

