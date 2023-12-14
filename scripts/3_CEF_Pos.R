library(Rforestry)
library(ggplot2)
library(dplyr)
library(splines)
library(pubtheme)
library(reshape2)
library(lme4)

b2bdata <- "nob2b"

if (b2bdata == "b2b"){
  df <- readRDS("data/1_Cleaned_dfB2B.rds")
} else{
  df <- readRDS("data/1_Cleaned_df.rds")
}

# Get consistent players
lp <- as.character(unique(df$player))
lplayer <- lp[table(as.character(df$player)) > 0]
df <- df[df$player %in% lplayer, ]
dim(df)

# Get weights
df <- data.frame(df %>% group_by(age) %>% mutate(w = n()))
df[df$age >34, "age"] <- 35
# Fitting a regression
yvars <- c("p40", "a40")

df[df$pos == "Center-Forward", "pos"] <- "Center"
df[df$pos == "Forward-Center", "pos"] <- "Forward"
df[df$pos == "Forward-Guard", "pos"] <- "Forward"
df[df$pos == "Guard-Forward", "pos"] <- "Guard"
table(df$pos)/nrow(df)


lprop <- glm(trt ~ player + age + teamag + season
             + homeind + team, data = df, family = binomial)
df[, "pscore"] <- lprop$fitted
#saveRDS(df, file = "data/pscoredf_wnba.rds")
df <- readRDS("data/pscoredf_wnba.rds")
df$pw <- ifelse(df$trt == 1, 1/df$pscore, 1/(1-df$pscore))
lpos <- unique(df$pos)
df[df$age >37, "age"] <- 38
# Fitting a regression
yvars <- c("p40", "a40", "r40", "s40", "b40", 
           "field_goals_pct", "free_throws_made_pct")
#yvars <- c("a40", "p40")
# yvars <- c("net.rat")

# Create grid
# Create grid to estimate the curve
lage <- seq(20, 40, 0.5)
lt <- c(0, 1)
df.grid <- expand.grid(player = unique(df$player), 
                       age = lage, 
                       trt = lt, 
                       teamag = "ATL", 
                       team = "WAS", 
                       season = 2015, 
                       homeind = T)

for (yname in yvars){
  for (pos in lpos){
    filename <- paste0("CEF", yname, b2bdata)
    dfs <- df[df$pos == pos, ] 
    # Create grid
    # Create grid to estimate the curve
    lage <- seq(20, 40, 0.5)
    lt <- c(0, 1)
    df.grid <- expand.grid(player = unique(dfs$player), 
                           age = lage, 
                           trt = lt, 
                           teamag = "ATL", 
                           team = "WAS", 
                           season = 2015, 
                           homeind = T)
    
    y <- dfs[, yname]
    lmod <- lm(y ~ player + trt*ns(age, df = 6) + teamag + season
               + homeind + team, data = dfs, weights = pw)
    varname <- paste0(yname, "-", pos)
    df.grid[, varname] <- predict(lmod, df.grid)
    
    # Plotting
    require(gridExtra)
    df.grid <- aggregate(. ~ trt + age, 
                         data = df.grid[, c(varname, "trt", "age")],
                         FUN = mean)
    df.grid$trt <- as.factor(df.grid$trt)
    df.grid$trtstr <- "Back-to-back"
    df.grid[df.grid$trt == 1, "trtstr"] <- "1+ Days Rest"
    cef <- df.grid %>% 
      ggplot(aes(x=age, y = get(varname), group = trtstr, color = trtstr)) +
      geom_line() + 
      labs( x = 'Age', y = varname) +  
      scale_x_continuous() + 
      scale_y_continuous() + 
      coord_cartesian(clip='off', expand=FALSE) +
      theme_pub(type='line', base_size=36/3) 
    cef
    ggsave(paste0("output/image/", varname, "CEFPos.png"), plot = cef, 
           width = 7, height = 5)
    print(varname)
  }
}


# See what's happening
#ggsave("output/image/CEF_Plot.png", plot = xolsp, width = 7, height = 5)
dg = df
dg <- dg[dg$pos == "Center", ]
dg <- dg[dg$age > 30, ]
dg = dg[sample(1:nrow(dg), 10000),]
dg[dg$age >34, "age"] <- 35
dg$age.f <- as.factor(dg$age)
dg$trt.f <- "Back-to-back"
dg[dg$trt == 1, "trt.f"] <- "1+ Days Rest"
ggplot(dg, aes(x=age.f,y=b40,col=trt.f)) + 
  geom_jitter() +
  geom_boxplot(alpha=0.2) + 
  facet_wrap(~trt.f) +
  labs( x = 'Age', y = "Ast. per 40 minutes") +  
  scale_y_continuous() + 
  theme_pub(type='line', base_size=36/3) 

plot(sapply(19:37, function(x) cor(df[df$trt == 0 & df$age == x, "a40"], 
                              df[df$trt == 0 & df$age == x, "p40"])))
