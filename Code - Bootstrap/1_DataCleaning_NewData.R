library(dplyr)
library(tidyr)
# setwd("C:/Users/shinpei/Documents/GitHub/ACTE")
dd <- readRDS("Data/processed_data/nba_data.rds")

# We have 17% of missing in mins and 18% on value
# However, I checked by hand and these NAs is DNP-Coach's decision so seems
# reasonable to remove.
round(sapply(dd, function(x) sum(is.na(x)))/nrow(dd), 2)
dd_na <- dd[is.na(dd$value), ]
# The remaining 1% is all for fg2p and it's when the player did not shoot once
# We might be safe to impute 0 in this case.
dd_na2 <- dd[is.na(dd$value) & !is.na(dd$mins), ]

dd[is.na(dd$value) & !is.na(dd$mins), "value"] <- 0
dd <- dd[complete.cases(dd), ]
df <- dd %>% pivot_wider()


# Compute player's age
df[, "age"] <- as.numeric(floor(difftime(df$date, df$dob, units = "weeks")
                                /52.25))

# Compute rest days between games
df <- df[df$mins > 25, ]
df <- df[order(df$pid, df$date), ] 
df <- df %>% group_by(pid) %>% 
  mutate(lgdate = lag(date, n = 1, default = NA)) %>% as.data.frame()
df <- df %>% group_by(pid) %>% 
  mutate(lagmin = lag(mins, n = 1, default = NA)) %>% as.data.frame()
df[, "RDays"] <- as.numeric(df$date - df$lgdate)
# 15% is b2b and 50% is 1 day rest
round(table(df$RDays)/nrow(df), 2)

# Na created by lag set to be 100 days rest
df[is.na(df$RDays), "RDays"] <- 100

# Compute points per 48 min
# Use other performance metric
df[, "p48"] <- df[, "pts"]/df[, "mins"]*48
df[, "r48"] <- df[, "reb"]/df[, "mins"]*48
df[, "a48"] <- df[, "ast"]/df[, "mins"]*48
df[, "s48"] <- df[, "stl"]/df[, "mins"]*48
df[, "b48"] <- df[, "blk"]/df[, "mins"]*48

# Compute points per 100 pos
# Use other performance metric
df[, "p100"] <- df[, "pts"]/df[, "poss"]*100
df[, "r100"] <- df[, "reb"]/df[, "poss"]*100
df[, "a100"] <- df[, "ast"]/df[, "poss"]*100
df[, "s100"] <- df[, "stl"]/df[, "poss"]*100
df[, "b100"] <- df[, "blk"]/df[, "poss"]*100
df[, "to100"] <- df[, "to"]/df[, "poss"]*100
df[, "or100"] <- df[, "oreb"]/df[, "poss"]*100
df[, "dr100"] <- df[, "dreb"]/df[, "poss"]*100

# Separate team away vs home
df$homeind <- df$team == df$home
df$teamag <- ifelse(df$homeind, df$away, df$home)

# Create treatment, 1 indicates non b2b and 0 is b2b
df[, "trt"] <- as.numeric(df$RDays > 1) 

# Delete players that never plays b2b
ltrt <- unique(df[df$trt == 1, "player"])
lctr <- unique(df[df$trt == 0, "player"])
lplayer <- unique(df$player)
lpuse <- lplayer[lplayer %in% ltrt & lplayer %in% lctr]
df <- df[df$player %in% lpuse, ]

# Delete the playoffs for now as there are no b2b
df <- df[df$season.type != "post", ]

# Keep data only for players playing b2b
df <- df[order(df$pid, df$date), ] 
rownames(df) <- NULL
dfb2b0 <- df[df$trt == 0, ]
lb2b <- df$trt == 0
dfb2b1 <- df[c(lb2b[-1], FALSE), ]
dfb2b <- rbind(dfb2b0, dfb2b1)
dfb2b <- dfb2b[order(dfb2b$player, dfb2b$date), ]
rownames(dfb2b) <- NULL 
n <- nrow(dfb2b)
dfb2b[is.na(dfb2b$lagmin), "lagmin"] <- 0

# Save output
saveRDS(dfb2b, "Data/1_Cleaned_dfB2B.rds")
saveRDS(df, "Data/1_Cleaned_df.rds")

