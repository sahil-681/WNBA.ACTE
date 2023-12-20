library(dplyr)
library(tidyr)
source("scripts/X_RF_OLS.R")

# Read in the clean pbg and games RDS files and combine the datasets
pbg_files <- list.files(path = "data/cleandata/pbg",
                    pattern = "\\.rds$",
                    full.names = TRUE)
pbg_list <- lapply(pbg_files, readRDS)
pbg_data <- do.call(rbind, pbg_list)
#pbg_data$player_name_key <- tolower(gsub(" ", "", pbg_data$player))

games_list <- list.files(path = "data/cleandata/games",
                         pattern = "\\.rds$",
                         full.names = TRUE)
games_list <- lapply(games_list, readRDS)
games_data <- do.call(rbind, games_list)

# Read in players data
players_data <- readRDS("./data/cleandata/players/wnba.players.rds")
#players_data$player_name_key <- tolower(gsub(" ", "", players_data$player))

# Apply the middle name removal function to players_data
players_data$player_name_key <- sapply(players_data$player, remove_middle_name)
pbg_data$player_name_key <- sapply(pbg_data$player, remove_middle_name)

# Apply cleaning function to player_name_key in both datasets
pbg_data$player_name_key <- sapply(pbg_data$player_name_key, clean_player_name)
players_data$player_name_key <- sapply(players_data$player_name_key, clean_player_name)

# Merge players data to combined data
df <- pbg_data %>%
  left_join(players_data, by = "player_name_key") %>%
  left_join(games_data, by = "gid")

# Identify names with NA values in specific columns (e.g., pid.y, player.y)
na_players <- unique(df[is.na(df$pid.y),]$player.x)
na_players

players_data$player = gsub("Sun Min Jung", "Jung Sun-Min", players_data$player)
players_data$player = gsub("Iziane Castro Marques", "Iziane Castro", players_data$player)
players_data$player = gsub("Tonya Massaline", "LaTonya Massaline", players_data$player)
players_data$player = gsub("Nikki McCray", "Nikki McCray-Penson", players_data$player)
players_data <- players_data[!players_data$player == "Penny McCray, Nikki (", ]
players_data$player = gsub("Vanessa Hayden", "Vanessa Hayden-Johnson", players_data$player)
players_data$player = gsub("Miao Li jie", "Miao Lijie", players_data$player)
players_data$player = gsub("Natalia Vodopyanova", "Natalia Vodopyonova", players_data$player)
players_data$player = gsub("Suzy Batkovic-Brown", "Suzy Batkovic", players_data$player)
players_data$player = gsub("Susan King-Borchardt", "Susan Borchardt", players_data$player)
players_data$player = gsub("Sui Fei Fei", "Sui Feifei", players_data$player)
players_data$player = gsub("Emilie Gomis", "Emile Gomis", players_data$player)
players_data$player = gsub("Tiffany Jackson", "Tiffany Jackson-Jones", players_data$player)
players_data$player = gsub("Jayne Appel Marinelli", "Jayne Appel-Marinelli", players_data$player)
players_data$player = gsub("Felicia Chester-Wootton", "Felicia Chester", players_data$player)
players_data$player = gsub("Aneika Morello", "Aneika Henry-Morello", players_data$player)
#players_data$player = gsub("Erika  Desouza", "Erika de Souza", players_data$player)
pbg_data$player = gsub("Erika  Desouza", "Erika de Souza", pbg_data$player)
players_data$player = gsub("Azurá Stevens", "Azura Stevens", players_data$player)
players_data$player = gsub("Marie Gülich", "Marie Gulich", players_data$player)
players_data$player = gsub(".*\\((.*)\\).*", "\\1 Durr", players_data$player)
players_data$player = gsub("Marine Johannès", "Marine Johannes", players_data$player)
players_data$player = gsub("Catrina Frierson", "Trina Frierson", players_data$player)
players_data$player = gsub("Tenae Davis-Cain", "Tanae Davis-Cain", players_data$player)
players_data$player = gsub("Val Berezhynska", "Valeriya Berezhynska", players_data$player)


# Apply the middle name removal function to players_data
players_data$player_name_key <- sapply(players_data$player, remove_middle_name)
pbg_data$player_name_key <- sapply(pbg_data$player, remove_middle_name)

# Apply cleaning function to player_name_key in both datasets
pbg_data$player_name_key <- sapply(pbg_data$player_name_key, clean_player_name)
players_data$player_name_key <- sapply(players_data$player_name_key, clean_player_name)

df2 <- pbg_data %>%
  left_join(players_data, by = "player_name_key") %>%
  left_join(games_data, by = "gid")

# Identify names with NA values in specific columns (e.g., pid.y, player.y)
na_players <- unique(df2[is.na(df2$pid.y),]$player.x)
na_players

df2 <- df2 %>%
  select(-player.y,
         -pos.y,
         -date.y,
         -lg.y,
         -season.y,
         -season.type.y) %>%
  rename(date = date.x,
         player = player.x,
         pos = pos.x,
         lg = lg.x,
         season = season.x,
         season.type = season.type.x)

df2$age <- as.numeric(floor(difftime(df2$date, df2$dob, units = "weeks") / 52.25))

# Only keep complete cases
df2 <- df2[complete.cases(df2), ]

# Map team names to the three letter abbreviations
team_abbreviations <- setNames(c("CON", "ATL", "CHI", "SAS", "LAS", "HOU", "IND", "WAS",
                                 "SAS", "NYL", "DET", "PHX", "CHA", "MIN", "SEA", "CLE",
                                 "TUL", "SAC", "LVA", "DAL"),
                               c("Connecticut Sun", "Atlanta Dream", "Chicago Sky", "San Antonio Stars",
                                 "Los Angeles Sparks", "Houston Comets", "Indiana Fever", "Washington Mystics",
                                 "San Antonio Silver Stars", "New York Liberty", "Detroit Shock", "Phoenix Mercury",
                                 "Charlotte Sting", "Minnesota Lynx", "Seattle Storm", "Cleveland Rockers",
                                 "Tulsa Shock", "Sacramento Monarchs", "Las Vegas Aces", "Dallas Wings"))


# Apply to team and opp columns
df3 <- df2 %>%
  mutate(team = team_abbreviations[team],
         opp = team_abbreviations[opp])

# Change team codes with inconsistencies
dd <- df3 %>%
  mutate(team = case_when(team =='PHX'  ~ 'PHO', # Phoenix Mercury
                          team =='SAS'  ~ 'SAN', # San Antonio Stars
                          TRUE ~ team),

         opp = case_when(opp == 'PHX'  ~ 'PHO', # Phoenix Mercury
                         opp =='SAS'  ~ 'SAN', # San Antonio Stars
                         TRUE ~ opp))

# saveRDS(dd, "data/temp_cleaned_wnba_data.rds")
# dd <- readRDS("data/temp_cleaned_wnba_data.rds")

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




