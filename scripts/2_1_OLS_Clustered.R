library(Rforestry)
library(ggplot2)
library(dplyr)
library(splines)
library(pubtheme)
library(reshape2)
source("scripts/X_RF_OLS.R")
source("helper/2_1_biometric_clustering.R")

# 40 minute metrics
cb.pal =  c("#004949","#009292","#ff6db6","#ffb6db",
            "#490092","#006ddb","#b66dff","#6db6ff","#b6dbff",
            "#920000","#924900","#db6d00","#24ff24","#ffff6d")

po <- "p40"
b2bdata <- "nob2b"

filename <- paste0(po, b2bdata)
yvars <- c("p40", "a40", "r40", "s40", "b40", "field_goals_pct",
           "three_point_field_goals_pct", "free_throws_made_pct")

if (b2bdata == "b2b"){
  d <- readRDS("data/1_Cleaned_dfB2B.rds")
} else{
  d <- readRDS("data/1_Cleaned_df.rds")
}

dd <- biometric_clustering(d)

########### Choosing cluster
i = 3 # set i to the cluster you want to run
cluster_df <- dd[dd$cluster == i, ]
cluster_df <- cluster_df[, !names(cluster_df) %in% "cluster", drop = FALSE]
df <- cluster_df
##########

# drop HOU and 2021 season for cluster 2
if (i == 2){
  df <- df[df$home != "HOU", ]
  df <- df[df$away != "HOU", ]
  df <- df[df$season != "2021", ]
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
lplayer <- lp[table(as.character(df$player)) > 50]
lplayer

df <- df[df$player %in% lplayer, ]

# Reapply all original 'team' levels to the filtered dataset
#df$team <- factor(df$team, levels = all_teams)

# Define features
lfeatures <- c(spcols, "team", "player", "teamag", "homeind", "season")

# Make factor
lfactor <- c("player", "team", "season.type", "country", "away", "home",
             "teamag", "homeind", "season")
df[, lfactor] <- lapply(df[, lfactor], as.factor)
n <- nrow(df)
trt <- "trt"

# Create the grid
lage <- seq(21, 38, 1)
lt <- as.factor(c(1))
lteam <- unique(df[df$season == "2020", "teamag"])
lteamag <- unique(df[df$season == "2020", "teamag"])
lhomeind <- c("TRUE", "FALSE")
lseason <- sort(unique(df$season))

for (po in yvars){
  # Start for loop here
  luse <- sample(seq(1, n), n * 1)
  x_data <- df[luse, lfeatures]
  y_data <- df[luse, po]
  w_data <- df[luse, trt]

  # X_OLS
  xmodel <- x.ols(x_data, w_train, y_train)
  #saveRDS(xmodel, file = paste0("output/model/", po, "GridXOLS1003.rds"))
  saveRDS(xmodel, file = paste0("output/clusteredmodel/cluster", i, "/", po, "GridXOLS1003_cluster", i, ".rds"))
  print(po)
  for (s in lseason){
    df.grid <- expand.grid(player = lplayer,
                           age = lage,
                           trt = lt,
                           team = lteam,
                           teamag = lteamag,
                           homeind = lhomeind,
                           season = as.character(s))
    dfns <- data.frame(ns(df.grid$age, df = nspline))
    colnames(dfns) <- spcols
    df.grid <- cbind(df.grid, dfns)
    cates <- x.est.ols(xmodel, df.grid)
    df.grid[, paste0("Pred.", po)] <- cates
    #print(s, po)
    #saveRDS(df.grid, file = paste0("output/model/", po, s, b2bdata, "GridXOLS102.rds"))
    saveRDS(df.grid, file = paste0("output/clusteredmodel/cluster", i, "/", po, s, b2bdata, "GridXOLS102_cluster", i, ".rds"))
  }
}


# Change metric and season according to what you want
po <- "a40"
s <- "2012"

df.grid <- readRDS(paste0("output/clusteredmodel/cluster", i, "/", po, s, b2bdata, "GridXOLS102_cluster", i, ".rds"))
head(df.grid)
# df.grid[, paste0("Pred.", yvars)] <- scale(df.grid[, paste0("Pred.", yvars)])

lvars <- c(po)
lvars <- c("age", paste0("Pred.", lvars))
dft <- df.grid[, lvars]
dft <- aggregate(. ~ age, dft, FUN = mean)
lfvar <- c("age", po)
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
