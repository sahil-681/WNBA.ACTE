library(dplyr)
library(tidyr)
source("R/remove_middle_name.R")
source("R/clean_player_name.R")

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
df4 <- df3 %>%
  mutate(team = case_when(team =='PHX'  ~ 'PHO', # Phoenix Mercury
                          team =='SAS'  ~ 'SAN', # San Antonio Stars
                          TRUE ~ team),
         
         opp = case_when(opp == 'PHX'  ~ 'PHO', # Phoenix Mercury
                         opp =='SAS'  ~ 'SAN', # San Antonio Stars
                         TRUE ~ opp))


#saveRDS(df4, "data/temp_cleaned_wnba_data.rds")

