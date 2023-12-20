library(Rforestry)
library(causalToolbox)
library(ggplot2)
library(dplyr)
library(splines)
library(orgthemes)
library(reshape2)
library(lme4)
setwd("~/GitHub/ACTE")

# Oreb vs dreb, assist
yname <- ".net.rat"; b2bdata <- "nob2b"

if (b2bdata == "b2b"){
  df <- readRDS("Data/1_Cleaned_dfb2b.rds")
} else{
  df <- readRDS("Data/1_Cleaned_df.rds")
}

# Get consistent players
lp <- as.character(unique(df$player))
lplayer <- lp[table(as.character(df$player)) > 0]
df <- df[df$player %in% lplayer, ]
dim(df)

# Get weights
df <- data.frame(df %>% group_by(age) %>% mutate(w = n()))
# df[df$age >34, "age"] <- 35
# Fitting a regression
yvars <- c("net.rat")



ls <- unique(df$season)
yname <- "net.rat"
n <- 6
for (s in ls[1:(length(ls) - (n - 1))]){
  filename <- paste0("CEF", yname, b2bdata)
  dfs <-df[df$season >= s & df$season < s+n, ] 
  # Create grid
  # Create grid to estimate the curve
  lage <- seq(20, 40, 0.5)
  lt <- c(0, 1)
  df.grid <- expand.grid(player = unique(dfs$player), age = lage, trt = lt, 
                         teamag = "LAL", team = "SAS", season = s, 
                         homeind = T)
  
  y <- dfs[, yname]
  
  lmod <- lm(y ~ player + trt*ns(age, df = 6) + teamag + season
             + homeind + team, data = dfs)
  varname <- paste0("net.rat", s, "-", s + n - 1)
  df.grid[, varname] <- predict(lmod, df.grid)
  
  # Plotting
  require(gridExtra)
  df.grid <- aggregate(. ~ trt + age, 
                       data = df.grid[, c(varname, "trt", "age")],
                       FUN = mean)
  df.grid$trt <- as.factor(df.grid$trt)
  df.grid$trtstr <- "Back-to-back"
  df.grid[df.grid$trt == 1, "trtstr"] <- "1+ Days Rest"
  cef <- df.grid %>% ggplot(aes(x=age, y = get(varname), group = trtstr, 
                                color = trtstr)) +
    geom_line() + labs( x = 'Age', 
                        y = varname) +  
    scale_x_continuous() + 
    scale_y_continuous() + 
    coord_cartesian(clip='off', expand=FALSE) +
    theme_org(type='line', base_size=36/3) 
  cef
  ggsave(paste0("Output/Image/", varname, "CEFMAPlot.png"), plot = cef, 
         width = 7, height = 5)
  print(s)
}
