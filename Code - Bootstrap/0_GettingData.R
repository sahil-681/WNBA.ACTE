library(ggplot2)
library(hoopR)
setwd("~/GitHub/ACTE/Data/hoopR")
yeari <- 2002; yearf <- 2021
lyear <- seq(yeari, yearf)
lall <- list()
for (year in lyear){
  dy <- load_nba_player_box(year)
  saveRDS(dy, file = paste0(year, "GameData.rds"))
  lall[[year - yeari + 1]] <- dy
}
dfAll <- do.call(rbind, lall)
saveRDS(dfAll, file = "2002-2021GameData.rds")

