library(Rforestry)
library(causalToolbox)
library(ggplot2)
library(dplyr)
library(splines)
library(orgthemes)
library(reshape2)
setwd("~/GitHub/ACTE")
source("Code/X_RF_OLS.R")
packageVersion("causalToolbox")

po <- "net.rat"; b2bdata <- "b2b"
filename <- paste0(po, b2bdata)
yvars <- c("net.rat", "off.rat", "def.rat", "p48", "a48", "r48", "s48", "b48", 
           "to")
# yvars <- c("net.rat", "p48")
# yvars <- c("net.rat", "p48")
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

lfeatures <- c("age", "team", "player", "teamag", "homeind", "season") 

# Make factor
lfactor <- c("player", "team", "season.type", "country", "away", "home",
             "teamag", "homeind", "season", "pos")
df[, lfactor] <- lapply(df[, lfactor], as.factor)
n <- nrow(df)
trt <- "trt"; 

# Create the grid
lp <- as.character(unique(df$player))
ngames <- 100
lplayer <- lp[table(as.character(df$player)) > ngames]


lage <- seq(21, 40, 1); lt <- as.factor(c(1)); 
lteam <- "LAL"; lteamag <- "SAS"; lhomeind <- "TRUE"; 
dfs <- expand.grid(player = lplayer, age = lage, team = lteam, 
                       teamag = lteamag, homeind = lhomeind, season = "2020")

#, team = "SAS", teamag = "LAL", homeind = "TRUE", season = "2020"

# Activate for splines
dfns <- data.frame(ns(dfs$age, df = nspline))
colnames(dfns) <- spcols
dfs <- cbind(dfs, dfns)

# X_RF
# Start for loop here

lp <- as.character(unique(df$player))
lplayer <- lp[table(as.character(df$player)) > ngames]
df <- df[df$player %in% lplayer, ]
rownames(df) <- NULL
n <- nrow(df)
luse <- sample(seq(1, n), n * 1)

for (po in yvars){
  x_data <- df[luse, lfeatures]; y_data <- df[luse, po]; w_data <- df[luse, trt]
  xl_rf <- x.rf(x_data, w_data, y_data)
  
  dfs[, paste0("Pred.", po)] <- x.est(xl_rf, dfs[, lfeatures])
  print(po)
}

saveRDS(dfs, file = "Output/Model/GridXRF200.rds")
dfs <- readRDS(file = "Output/Model/GridXRF200.rds")
dfs <- aggregate(. ~ age, dfs[, c("age", paste0("Pred.", yvars))], mean)
dfs[, paste0("Pred,", yvars)] <- scale(dfs[, paste0("Pred.", yvars)])
dfp <- melt(dfs[, c("age", paste0("Pred.", yvars))], id.vars = "age")

xrfp <- dfp %>% ggplot(aes(x=age, y = value, group = variable,
                            color = variable)) +
  geom_line() + labs(title    = "X-learner (RF)",
                     subtitle = 'ACTE', x = 'Age', 
                     y = "Standardized Outcome") +  
  scale_x_continuous() + 
  scale_y_continuous() + 
  coord_cartesian(clip='off', expand=FALSE) +
  theme_org(type='line', base_size=36/3) 
xrfp
ggsave("Output/Image/X_RF_Plot200.png", plot = xrfp, width = 7, height = 5)
a

#####################################################
# Appendix
############################################
dfs <- df[, ]
dfs <- aggregate(net.rat ~ age + trt, data = dfs, mean)
dfs$trtstr <- "Back-to-back"
dfs[dfs$trt == 1, "trtstr"] <- "1+ Days Rest"
dfs %>% ggplot(aes(x=age, y = net.rat, group = trtstr, 
                               color = trtstr)) +
  geom_line() + labs(x = 'Age', 
                     y = "Point per 48 minutes") +  
  #title    = "Conditional Expectation Function",
  #subtitle = 'Average CEF'
  scale_x_continuous() + 
  scale_y_continuous() + 
  coord_cartesian(clip='off', expand=FALSE) +
  theme_org(type='line', base_size=36/3) 

#ACTE difference
df.gridw <- dcast(dfs[, c("age", "trtstr", "net.rat")], age ~ trtstr)
df.gridw$Diff <- df.gridw$`1+ Days Rest` - df.gridw$`Back-to-back`
title = "Age Conditioned Treatment Effect"
CEF_diff <- df.gridw %>% ggplot(aes(x=age, y = Diff)) +
  geom_line() +
  labs(title    = title,
       subtitle = 'CEF difference',
       x = 'Age',
       y = 'difference of CEF') +  ylim(0, 3) +
  scale_x_continuous(labels=comma) +
  scale_y_continuous(labels=comma) +
  theme_org(type='line', base_size=36/3)
#coord_cartesian(clip='off', expand=FALSE) +
CEF_diff
# Save plot
ggsave(paste0("Output/Image/", filename, "CEF_diffPlot.png"), plot = CEF_diff,
       width = 7, height = 5)
