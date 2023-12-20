library(lubridate)
library(hoopR)
library(wehoop)
library(dplyr)

source("helper/0_helpers.R")

leagues = 'wnba'
types = c('players', 'games', 'pbg')
update.raw   = T
lgs.games   = 'wnba'
lgs.players = 'wnba'
lgs.pbg     = 'wnba'

excluded_games <- data.frame(gid = numeric(),
                             season = numeric(),
                             lg = character())

incomplete_team_names <- data.frame(gid = numeric(),
                                    season = numeric(),
                                    lg = character())

gids_to_exclude <- vector()

for (lg in leagues){

  cat('\n', lg,'\n')

  ## update players ----
  praw.backup.fn = paste0('data/rawdata/players/', lg, '.players.', Sys.Date(), '.rds')
  praw.fn        = paste0('data/rawdata/players/', lg, '.players.rds')
  p.fn           = gsub('raw', 'clean', praw.fn)
  update.players = 'players' %in% types & lg %in% lgs.players

  if(update.players==T){

    if(update.raw==T){
      praw = get.players(lg=lg);
      saveRDS(praw, praw.backup.fn);
      saveRDS(praw, praw.fn)
    }
    if(update.raw==F){praw = readRDS(praw.fn)}

    p = clean.players(praw)
    saveRDS(p, p.fn)
  }
  if(update.players==F){p = readRDS(p.fn)}
  seasons = 2022:2002

  for(season in seasons){
    cat('\n', season,'season\n')

    ## Create initial filename to be edited
    ## Then create filenames for each data type
    fn = paste0('data/rawdata/type/', lg, '.type.', season, '.rds')
    raw.g.fn     = gsub('type', 'games', fn)
    raw.tbs.fn   = gsub('type', 'tbs'  , fn)
    clean.g.fn   = gsub('raw' , 'clean', raw.g.fn  )
    clean.tbs.fn = gsub('raw' , 'clean', raw.tbs.fn)

    for(type in types){

      update.games = 'games' %in% types & lg %in% lgs.games & update.raw==T
      update.pbg   = 'pbg'   %in% types & lg %in% lgs.pbg   & update.raw==T
      ## update raw data ----
      if(update.games==T){  graw = get.games(lg, season); saveRDS(graw  , raw.g.fn  )}

      if(update.games==F){graw   = readRDS(raw.g.fn  )}

      ## clean data ----
      g   = clean.games(  graw)

      ## save data ----
      saveRDS(g  , clean.g.fn  )

      ## player box game ----
      if(update.pbg==T){
        gids = sort(g$gid)
        for(gid in gids){
          cat(as.character(gid),'')
          raw.pbg.fn   = paste0('data/rawdata/pbg/', lg, '.pbg.', season, '.', gid, '.rds')
          raw.tbg.fn   = paste0('data/rawdata/tbg/', lg, '.tbg.', season, '.', gid, '.rds')
          clean.pbg.fn = gsub('raw' , 'clean', raw.pbg.fn)
          clean.tbg.fn = gsub('raw' , 'clean', raw.tbg.fn)

          if(update.pbg==T){pbgraw = get.pbg(lg=lg, gid = gid, season = season); saveRDS(pbgraw, raw.pbg.fn)}
          if(update.pbg==F){pbgraw = readRDS(raw.pbg.fn)}

          if (lg == "wnba" &
              setequal(colnames(pbgraw),
                       c("athlete_display_name", "team_short_display_name",
                         "min", "fg", "fg3", "ft", "oreb", "dreb", "reb", "ast",
                         "stl", "blk", "to", "pf", "plus_minus", "pts", "starter",
                         "ejected", "did_not_play", "active", "athlete_jersey",
                         "athlete_id", "athlete_short_name", "athlete_headshot_href",
                         "athlete_position_name", "athlete_position_abbreviation",
                         "team_name", "team_logo", "team_id", "team_abbreviation",
                         "team_color", "game_id", "season", "season_type", "game_date", "lg"
                       ))) {
            incomplete_team_names <- rbind(incomplete_team_names,
                                           data.frame(gid = gid, season = season, lg = lg))
          }

          if (length(pbgraw$lg) == 0) {
            gids_to_exclude <- c(gids_to_exclude, gid)
            cat('(Excluded)\n')
            temp <- data.frame(gid = gid, season = season, lg = lg)
            excluded_games <- rbind(excluded_games, temp)
            next
          }

          pbg    = clean.pbg(pbgraw)

          ## save cleaned data
          saveRDS(pbg, file=clean.pbg.fn)

        } # end loop over gid

        ## bind all games for this season together and
        ## save them all to one file for the whole season
        cat('Bind games','')
        if (exists("gids_to_exclude")) {
          remaining_gids <- setdiff(gids, gids_to_exclude) #remove skipped gids
        } else {
          remaining_gids <- gids
        }
        d = bind.games(lg=lg, gid = remaining_gids, data.type='pbg')

        saveRDS(d, file=paste0('data/', lg, '.pbg.', season, '.rds'))
        head(d,2)
        tail(d,2)
      }
    }
  }
  cat('bind seasons ')
  d = bind.seasons(lg=lg, seasons=seasons, data.type='games')
  saveRDS(d, file=paste0('data/cleandata/', lg, '.games.rds'))
}

excluded_games <- unique(excluded_games)
incomplete_team_names <- unique(incomplete_team_names)

saveRDS(excluded_games, "data/excluded_games_wnba.rds")
saveRDS(incomplete_team_names, "data/incomplete_team_names_wnba.rds")

## bind leagues and save
cat('bind leagues and save...')
g   = bind.leagues(lgs = lgs.games, data.type = 'games')


saveRDS(g  , file='data/games.rds')

