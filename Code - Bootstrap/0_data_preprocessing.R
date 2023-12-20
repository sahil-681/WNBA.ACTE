library(dplyr)
# setwd("C:/Users/shinpei/Documents/GitHub/ACTE")
## load the two data sets
d = readRDS('Data/pbg_data/nba.pbg2.rds')
p = readRDS('Data/pbg_data/nba.players.rds')
g = readRDS('Data/pbg_data/nba.games.rds')

## keep just pid and dob columns
pp = p %>% select(pid, dob, ht, wt, pos, country, rookie)
gg <- g %>% select(away, home, ascore, hscore, gid)

## join the two data sets
dd = d %>%
  left_join(pp, by='pid') %>% left_join(gg, by = "gid")

## check that it joined as expected
head(dd)
tail(dd)
dd %>% 
  filter(player=='LeBron James')

## check that we have dob for most/all players
## only 396 rows out of 2 million missing a dob
length(which(is.na(dd$dob)))


## This code shows that most of the missing dob are for players who played 0 minutes. 
## Looks like there are only two players with minutes that don't have a birthday
## after joining with the players data set
## we can probably ignore them for now, but I'll work on getting them into the
## players data
dd %>%
  select(pid, player, dob, season, season.type, mins) %>%
  filter(is.na(dob)) %>%
  unique()

saveRDS(dd, file = "Data/processed_data/nba_data.rds")
