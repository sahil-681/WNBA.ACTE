dd <- readRDS("data/temp_cleaned_wnba_data.rds")

# Pivot field goals made back to wide format
ddw <- pivot_wider(dd, names_from = name, values_from = value)

# Make numeric
lnumeric <- c("field_goals_made",
              "three_point_field_goals_made",
              "free_throws_made",
              "field_goals_attempted", 
              "three_point_field_goals_attempted",
              "free_throws_attempted",
              "offensive_rebounds",
              "defensive_rebounds",
              "rebounds",
              "assists",
              "steals",
              "blocks",
              "turnovers",
              "fouls",
              "plus_minus",
              "points")

ddw[, lnumeric] <- lapply(ddw[, lnumeric], as.numeric)

# Compute field_goals_made, three_point_field_goals_made, 
# free_throws_made percentage
ddw[, c("field_goals_pct", "three_point_field_goals_pct", "free_throws_made_pct")] <- 0
ddw[ddw$field_goals_made != 0, "field_goals_pct"] <- ddw[ddw$field_goals_made != 0, "field_goals_made"] / ddw[ddw$field_goals_made != 0, "field_goals_attempted"]
ddw[ddw$three_point_field_goals_made != 0, "three_point_field_goals_pct"] <- ddw[ddw$three_point_field_goals_made != 0, "three_point_field_goals_made"] / ddw[ddw$three_point_field_goals_made != 0, "three_point_field_goals_made"]
ddw[ddw$free_throws_made != 0, "free_throws_made_pct"] <- ddw[ddw$free_throws_made != 0, "free_throws_made"] / ddw[ddw$free_throws_made != 0, "free_throws_attempted"]


# Compute rest days between games
ddw <- ddw[ddw$mins > 20, ]
ddw <- ddw[order(ddw$player, ddw$date), ] 
ddw <- ddw %>% group_by(player) %>% 
  mutate(last_game_date = lag(date, n = 1, default = NA)) %>% as.data.frame()
ddw <- ddw %>% group_by(player) %>% 
  mutate(last_game_minutes = lag(mins, n = 1, default = NA)) %>% as.data.frame()
ddw[, "rest_days"] <- as.numeric(ddw$date - ddw$last_game_date)

ddw <- ddw[ddw$rest_days > 0 & !is.na(ddw$rest_days), ]

# Compute points per 40 min
# Use other performance metric
ddw[, "p40"] <- ddw[, "points"] / ddw[, "mins"] * 40
ddw[, "r40"] <- ddw[, "rebounds"] / ddw[, "mins"] * 40
ddw[, "a40"] <- ddw[, "assists"] / ddw[, "mins"] * 40
ddw[, "s40"] <- ddw[, "steals"] / ddw[, "mins"] * 40
ddw[, "b40"] <- ddw[, "blocks"] / ddw[, "mins"] * 40


# Separate team away vs home
ddw$homeind <- ddw$team == ddw$home
ddw$teamag <- ifelse(ddw$homeind, ddw$away, ddw$home)

# Create treatment, 1 indicates non b2b and 0 is b2b
ddw[, "trt"] <- as.numeric(ddw$rest_days > 1)

# Delete players that never plays b2b
ltrt <- unique(ddw[ddw$trt == 1, "player"])
lctr <- unique(ddw[ddw$trt == 0, "player"])
lplayer <- unique(ddw$player)
lpuse <- lplayer[lplayer %in% ltrt & lplayer %in% lctr]
ddw <- ddw[ddw$player %in% lpuse, ]

# Delete the playoffs for now as there are no b2b
ddw <- ddw[ddw$season.type != "post", ]

# Keep data only for players playing b2b
ddw <- ddw[order(ddw$pid.x, ddw$date), ] 
rownames(ddw) <- NULL
dfb2b0 <- ddw[ddw$trt == 0, ]
lb2b <- ddw$trt == 0
dfb2b1 <- ddw[c(lb2b[-1], FALSE), ]
dfb2b <- rbind(dfb2b0, dfb2b1)
dfb2b <- dfb2b[order(dfb2b$player, dfb2b$date), ]
rownames(dfb2b) <- NULL 
n <- nrow(dfb2b)
dfb2b[is.na(dfb2b$last_game_minutes), "last_game_minutes"] <- 0

#saveRDS(dfb2b, "data/1_Cleaned_dfB2B.rds")
#saveRDS(ddw, "data/1_Cleaned_df.rds")


