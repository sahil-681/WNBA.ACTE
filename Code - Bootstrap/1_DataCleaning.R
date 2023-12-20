library(dplyr)
setwd("~/GitHub/ACTE/Data/hoopR")
df <- readRDS("2002-2021GameData.rds")
setwd("~/GitHub/ACTE/Data/PlayerData")
dp <- readRDS("nba.players.rds")

################################################################################
# Use last n years
################################################################################

df <- df[df$season>2011, ]

################################################################################
# Merge Player data (NOTE: Some player data have NAs b/c merging by player name)
################################################################################
# Brian is pulling the data from nba.com to solve this 
df <- data.frame(merge(df, dp, by.x = "athlete_display_name", by.y = "player", 
                       all.x = TRUE))

################################################################################
# Drop columns we won't use
################################################################################

lnouse <- c("athlete_jersey", "athlete_headshot_href", "athlete_position_name",
            "team_name", "team_logo", "team_short_display_name", "team_color")
luse <- colnames(df)[!names(df) %in% lnouse]
df <- df[, luse]

################################################################################
# Clean columns
################################################################################

# Separate fg, fg3, ft
df <- df[!df$fg == "-----", ]
fg <- data.frame(do.call('rbind', strsplit(as.character(df$fg), '-')))
fg3 <- data.frame(do.call('rbind', strsplit(as.character(df$fg3), '-')))
ft <- data.frame(do.call('rbind', strsplit(as.character(df$ft), '-')))
dfshoot <- cbind(fg, fg3, ft)
colnames(dfshoot) <- c("fgM", "fgA", "fg3M", "fg3A", "ftM", "ftA")
df <- cbind(df, dfshoot)

# Clean plus minus
df$plus_minus <- as.numeric(df$plus_minus)

# Make numeric
lnumeric <- c("min", "fgM", "fgA", "fg3M", "fg3A", "ftM", "ftA", "oreb", "dreb",
              "reb", "ast", "stl", "blk", "to", "pf", "ht", "wt", "pts")
df[, lnumeric] <- lapply(df[, lnumeric], as.numeric)
head(df)

# Compute fg, fg3, ft percentage
df[, c("fgp", "fg3p", "ftp")] <- 0
df[df$fgM != 0, "fgp"] <- df[df$fgM != 0, "fgM"]/df[df$fgM != 0, "fgA"]
df[df$fg3M != 0, "fg3p"] <- df[df$fg3M != 0, "fg3M"]/df[df$fg3M != 0, "fg3A"]
df[df$ftM != 0, "ftp"] <- df[df$ftM != 0, "ftM"]/df[df$ftM != 0, "ftA"]

# Compute player's age
df[, "age"] <- as.numeric(floor(difftime(df$game_date, df$dob, units = "weeks")
                                /52.25))

# Compute rest days between games
df <- df[df$min > 20, ]
df <- df[order(df$athlete_display_name, df$game_date), ] 
df <- df %>% group_by(athlete_display_name) %>% 
  mutate(lgdate = lag(game_date, n = 1, default = NA)) %>% as.data.frame()
df <- df %>% group_by(athlete_display_name) %>% 
  mutate(lagmin = lag(min, n = 1, default = NA)) %>% as.data.frame()
df[, "RDays"] <- as.numeric(df$game_date - df$lgdate)

# hoopR has duplicated game so we will remove for now
# This might not happen if we use the new data
df <- df[df$RDays > 0 & !is.na(df$RDays), ]

# Compute points per 48 min
# Use other performance metric
df[, "p48"] <- df[, "pts"]/df[, "min"]*48

# Save output
setwd("~/GitHub/ACTE/Data")
saveRDS(df, "1_Cleaned_df.rds")











