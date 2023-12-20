library(Rforestry)
library(causalToolbox)
library(ggplot2)
library(dplyr)
library(splines)
library(orgthemes)
library(reshape2)
library(lme4)
setwd("~/GitHub/ACTE")

# b2bdata <- "nob2b"
# 
# if (b2bdata == "b2b"){
#   df <- readRDS("Data/1_Cleaned_dfb2b.rds")
# } else{
#   df <- readRDS("Data/1_Cleaned_df.rds")
# }
# 
# # Get consistent players
# lp <- as.character(unique(df$player))
# lplayer <- lp[table(as.character(df$player)) > 0]
# df <- df[df$player %in% lplayer, ]
# dim(df)
# 
# # Get weights
# df <- data.frame(df %>% group_by(age) %>% mutate(w = n()))
# # df[df$age >34, "age"] <- 35
# # Fitting a regression
# yvars <- c("p100", "a100")
# 
# df[df$pos == "Center-Forward", "pos"] <- "Center"
# df[df$pos == "Forward-Center", "pos"] <- "Forward"
# df[df$pos == "Forward-Guard", "pos"] <- "Forward"
# df[df$pos == "Guard-Forward", "pos"] <- "Guard"
# table(df$pos)/nrow(df)
# lpos <- unique(df$pos)
# 
# lprop <- glm(trt ~ player + age + teamag + season
#              + homeind + team, data = df, family = binomial)
# df[, "pscore"] <- lprop$fitted
# saveRDS(df, file = "Data/processed_data/Pscoredf.rds")
df <- readRDS("Data/processed_data/Pscoredf.rds")
df$pw <- ifelse(df$trt == 1, 1/df$pscore, 1/(1-df$pscore))

# df[df$age >34, "age"] <- 35
# Fitting a regression
yvars <- c("net.rat", "off.rat", "def.rat", "p100", "a100", "r100", "s100", 
           "b100", "to100", "fgp", "ftp", "oreb.p", "dreb.p", "ts.p",
           "or100", "dr100")
# yvars <- c("a100")
# yvars <- c("net.rat")

# Create grid
# Create grid to estimate the curve
lage <- seq(20, 40, 0.5)
lt <- c(0, 1)
df.grid <- expand.grid(player = unique(df$player), age = lage, trt = lt, 
                       teamag = "LAL", team = "SAS", season = 2013, 
                       homeind = T)


for (yname in yvars){
  filename <- paste0(yname)
  y <- df[, yname]
  yname <- paste0("Pred.", yname)
  
  lmod <- lm(y ~ trt*player + trt*ns(age, df = 6) + teamag + season
             + homeind + team, data = df)
  # summary(lmod)
  saveRDS(lmod, file = paste0("Output/Model/CEF/CEF", filename, "1003.rds"))
  # readRDS(paste0("Output/Model/CEF/", filename, "1003.rds"))
  
  
  # Predict the value for the grid using the model and aggregate
  # df.grid[, yname] <- predict(lmod, df.grid)
  print(yname)
  
}
aaaaaa
saveRDS(df.grid, file = paste0("Output/Model/GridCEF.rds"))
df.grid <- readRDS(paste0("Output/Model/GridCEF.rds"))
require(gridExtra)
df.grid <- aggregate(. ~ trt + age, 
                     data = df.grid[, c(paste0("Pred.", yvars), "trt", "age")],
                     FUN = mean)
df.grid$trt <- as.factor(df.grid$trt)
df.grid$trtstr <- "Back-to-back"
df.grid[df.grid$trt == 1, "trtstr"] <- "1+ Days Rest"

for (var in yvars){
  var <- paste0("Pred.", var)
  cef <- df.grid %>% ggplot(aes(x=age, y = get(var), group = trtstr, 
                                 color = trtstr)) +
    geom_line() + labs( x = 'Age', 
                        y = var) +  
    scale_x_continuous() + 
    scale_y_continuous() + 
    coord_cartesian(clip='off', expand=FALSE) +
    theme_org(type='line', base_size=36/3) 
  cef
  ggsave(paste0("Output/Image/", var, "CEFPlot.png"), plot = cef, 
         width = 7, height = 5)
}

var <- "a100"
var <- paste0("Pred.", var)
dg = df[sample(1:nrow(df), 1000),] ## randomly select 1000 rows, or 10000, or something
dg$trtstr <- "Back-to-back"
dg[dg$trt == 1, "trtstr"] <- "1+ Days Rest"

cef <- df.grid %>% ggplot(aes(x=age, y = get(var), group = trtstr, 
                              color = trtstr)) + geom_line() +
  labs( x = 'Age', y = "Pred.p100") +
  scale_x_continuous() +
  scale_y_continuous() +
  coord_cartesian(clip='off', expand=FALSE) +
  theme_org(type='line', base_size=36/3)
cef
# In case we want to show few of them
# cef2 <- df.grid %>% ggplot(aes(x=age, y = Predp48, group = trtstr, 
#                                color = trtstr)) +
#   geom_line() + labs(x = 'Age', 
#                      y = "Point per 48 minutes") +  
#   #title    = "Conditional Expectation Function",
#   #subtitle = 'Average CEF'
#   scale_x_continuous() + 
#   scale_y_continuous() + 
#   coord_cartesian(clip='off', expand=FALSE) +
#   theme_org(type='line', base_size=36/3) 
# 
# grid.arrange(cef1, cef2, ncol=2, top = textGrob(
#   "Spline S-learner",
#   gp = gpar(fontface = 3, fontsize = 30)))


# ggsave("Output/Image/CEF_Plot.png", plot = xolsp, width = 7, height = 5)
df[df$pos == "Center-Forward", "pos"] <- "Center"
df[df$pos == "Forward-Center", "pos"] <- "Forward"
df[df$pos == "Forward-Guard", "pos"] <- "Forward"
df[df$pos == "Guard-Forward", "pos"] <- "Guard"
dg = df[sample(1:nrow(df), nrow(df)),]
dg <- dg[dg$pos == "Center", ]
dg <- dg[dg$age > 35, ]
# dg[dg$age >34, "age"] <- 35
dg$age.f <- as.factor(dg$age)
dg$trt.f <- as.factor(dg$trt)
ggplot(dg, aes(x=age.f,y=p100,col=trt.f)) + geom_jitter() +
  geom_boxplot(alpha=0.2) + facet_wrap(~trt.f) + 
  scale_y_continuous()

# plot(df$a100, lmod$residuals)
