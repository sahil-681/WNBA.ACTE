# Data preprocessing helper functions

get.players=function(lg=NULL){

  if(lg == 'wnba'){p = get.wnba.players()}
  else{print('not yet implemented')}

  return(p)

}

get.wnba.players = function(){

  p = wnba_commonallplayers(
    is_only_current_season = 0,
    league_id = '10')
  p=p$CommonAllPlayers %>% as.data.frame() %>%
    mutate()
  head(p,2)

  if (file.exists('rawdata/players/wnba.players.rds') == T) {
    existing.p = readRDS('rawdata/players/wnba.players.rds')
    # existing.p = read.csv('rawdata/wnba.players.csv')
    # existing.p = existing.p[, colnames(existing.p)!='X']
    existing.p$lg='wnba'
    existing.p$PERSON_ID = as.character(existing.p$PERSON_ID)
    p = p %>% filter(!PERSON_ID %in% existing.p$PERSON_ID)
    dd = existing.p
  } else {
    # Create an empty dataframe
    dd <- data.frame(
      PERSON_ID = character(0),
      FIRST_NAME = character(0),
      LAST_NAME = character(0),
      DISPLAY_FIRST_LAST = character(0),
      DISPLAY_LAST_COMMA_FIRST = character(0),
      DISPLAY_FI_LAST = character(0),
      PLAYER_SLUG = character(0),
      BIRTHDATE = character(0),
      SCHOOL = character(0),
      COUNTRY = character(0),
      LAST_AFFILIATION = character(0),
      HEIGHT = character(0),
      WEIGHT = numeric(0),
      SEASON_EXP = numeric(0),
      JERSEY = numeric(0),
      POSITION = character(0),
      ROSTERSTATUS = character(0),
      GAMES_PLAYED_CURRENT_SEASON_FLAG = character(0),
      TEAM_ID = numeric(0),
      TEAM_NAME = character(0),
      TEAM_ABBREVIATION = character(0),
      TEAM_CODE = character(0),
      TEAM_CITY = character(0),
      PLAYERCODE = character(0),
      FROM_YEAR = numeric(0),
      TO_YEAR = numeric(0),
      DLEAGUE_FLAG = character(0),
      NBA_FLAG = character(0),
      GAMES_PLAYED_FLAG = character(0),
      DRAFT_YEAR = character(0),
      DRAFT_ROUND = character(0),
      DRAFT_NUMBER = character(0),
      GREATEST_75_FLAG = character(0),
      lg = character(0)
    )
  }

  ## only scrape players that don't already exist

  dim(p)

  cat('getting', nrow(p), 'players\n')

  if(nrow(p)>=1){
    for (j in 1:nrow(p)){
      #if(j %% 10 ==0 | j==nrow(p)){cat(j, '')}
      cat(j,'')

      try(d <- wnba_commonplayerinfo(league='10',
                                     player_id = p$PERSON_ID[j]),
          silent=TRUE)
      varlist = ls()
      varlistmatch = which(varlist=="d")
      if (length(varlistmatch)>0){
        d = d$CommonPlayerInfo
        d = d %>%
          mutate(WEIGHT = as.numeric(WEIGHT),
                 SEASON_EXP = as.numeric(SEASON_EXP),
                 JERSEY = as.numeric(JERSEY),
                 TEAM_ID = as.numeric(TEAM_ID),
                 FROM_YEAR = as.numeric(FROM_YEAR),
                 TO_YEAR = as.numeric(TO_YEAR))
        d$lg = 'wnba'

        dd = bind_rows(dd, d)
        saveRDS(dd, 'rawdata/players/wnba.players.rds')
      }
      if(length(varlistmatch)==0){
        print(paste0('Player ', p$PERSON_ID[j], ' failed, sadly. :('))
      }
      ## Without this, will get rate-limited
      ## Was running smoothly until j=602, so maybe 600 per hour?
      ## That is 10 per minute, 6 per second.
      ## This gives mean greater than 10. Seems to be sufficient.
      Sys.sleep(runif(1, 1, 2))
      gc()
    } ## for
  } ## end if


  return(dd)

}

get.wnba.pbg = function(gid, season){
  library("wehoop")
  d <- subset(load_wnba_player_box(seasons = season),
              game_id == gid)
  d$lg = "wnba"
  return(d)
}

clean.wnba.pbg = function(data){

  if (setequal(colnames(data), c("athlete_display_name",
                                 "team_short_display_name", "min","fg", "fg3",
                                 "ft", "oreb", "dreb", "reb", "ast", "stl",
                                 "blk", "to", "pf", "plus_minus", "pts",
                                 "starter", "ejected", "did_not_play", "active",
                                 "athlete_jersey", "athlete_id", "athlete_short_name",
                                 "athlete_headshot_href", "athlete_position_name",
                                 "athlete_position_abbreviation", "team_name",
                                 "team_logo", "team_id", "team_abbreviation",
                                 "team_color", "game_id", "season", "season_type",
                                 "game_date", "lg"))) {
    data <- data %>%
      mutate(lg = 'wnba',
             season_type = ifelse(season_type == 2, "reg", "post"),
             gp = ifelse(as.numeric(min)>0 & !is.na(min), 1, 0)) %>%
      rename(mins = min,
             player = athlete_display_name,
             date = game_date,
             pos = athlete_position_name,
             gid = game_id,
             pid = athlete_id,
             team = team_name,
             season.type = season_type,
             assists = ast,
             offensive_rebounds = oreb,
             defensive_rebounds = dreb,
             rebounds = reb,
             turnovers = to,
             steals = stl,
             blocks = blk,
             points = pts,
             fouls = pf
      ) %>%
      separate(col = fg, into = c("field_goals_made", "field_goals_attempted"), sep = "-") %>%
      separate(col = fg3, into = c("three_point_field_goals_made", "three_point_field_goals_attempted"), sep = "-") %>%
      separate(col = ft, into = c("free_throws_made", "free_throws_attempted"), sep = "-")

    #### NOT FULL NAMES
    data$opp <- ifelse(data$team == unique(data$team)[1], unique(data$team)[2], unique(data$team)[1])

    data <- data %>%
      select(date, player, pos, team, opp, gp, mins, field_goals_made,
             field_goals_attempted, three_point_field_goals_made,
             three_point_field_goals_attempted, free_throws_made,
             free_throws_attempted, offensive_rebounds, defensive_rebounds,
             rebounds, assists, steals, blocks, turnovers, fouls, plus_minus,
             points, starter, ejected, did_not_play, active, athlete_jersey, lg,
             season, season.type, gid, pid)


    data[, 8:28] <- lapply(data[, 8:28], as.character)

    data <- tidyr::pivot_longer(data      = data,
                                cols      = 8:28,
                                names_to  = "name",
                                values_to = "value")

    data <- data %>% select(date, player, pos, team, opp, gp, mins, name, value,
                            lg, season, season.type, gid, pid)

  } else {
    data <-  data %>%
      mutate(lg = 'wnba',
             season_type = ifelse(season_type == 2, "reg", "post"),
             gp = ifelse(as.numeric(minutes)>0 & !is.na(minutes), 1, 0)) %>%
      rename(
        date        = game_date,
        player      = athlete_display_name,
        pos         = athlete_position_name,
        team        = team_display_name,
        opp         = opponent_team_display_name,
        mins        = minutes,
        season.type = season_type,
        gid         = game_id,
        pid         = athlete_id,
        tid         = team_id
      )

    data[, 13:33] <- lapply(data[, 13:33], as.character)

    data <- tidyr::pivot_longer(data      = data,
                                cols      = 13:33,
                                names_to  = "name",
                                values_to = "value")

    data <- data %>% select(date, player, pos, team, opp, gp, mins, name, value,
                            lg, season, season.type, gid, pid)
  }
  data <- data %>%
    mutate(team = case_when(team=='Liberty'  ~ 'New York Liberty',
                            team=='Fever'   ~ 'Indiana Fever',
                            team=='Sparks'  ~ 'Los Angeles Sparks',
                            team=='Stars'  ~ 'San Antonio Stars',
                            team=='Mercury'   ~ 'Phoenix Mercury',
                            team=='Sun' ~ 'Connecticut Sun',
                            team=='Mystics'   ~ 'Washington Mystics',
                            team=='Lynx'  ~ 'Minnesota Lynx',
                            team=='Storm'  ~ 'Seattle Storm',
                            team=='Sky'  ~ 'Chicago Sky',
                            team=='Wings'  ~ 'Dallas Wings',
                            team=='Dream'  ~ 'Atlanta Dream',
                            team=='Starzz'   ~ 'Utah Starzz',
                            team=='Sol'  ~ 'Miami Sol',
                            team=='Fire'  ~ 'Portland Fire',
                            team=='Monarchs'   ~ 'Sacramento Monarchs',
                            team=='Rockers'  ~ 'Cleveland Rockers',
                            team=='Shock'  ~ 'Tulsa Shock',
                            team=='Sting'  ~ 'Charlotte Sting',
                            team=='Miracle'  ~ 'Orlando Miracle',
                            team=='Aces' ~ 'Las Vegas Aces',
                            team=='Wings' ~ 'Dallas Wings',
                            team=='Comets' ~ 'Houston Comets',
                            team=='Storm' ~ 'Seattle Storm',
                            TRUE ~ team),
           opp = case_when(opp=='Liberty'  ~ 'New York Liberty',
                           opp=='Fever'   ~ 'Indiana Fever',
                           opp=='Sparks'  ~ 'Los Angeles Sparks',
                           opp=='Stars'  ~ 'San Antonio Stars',
                           opp=='Mercury'   ~ 'Phoenix Mercury',
                           opp=='Sun' ~ 'Connecticut Sun',
                           opp=='Mystics'   ~ 'Washington Mystics',
                           opp=='Lynx'  ~ 'Minnesota Lynx',
                           opp=='Storm'  ~ 'Seattle Storm',
                           opp=='Sky'  ~ 'Chicago Sky',
                           opp=='Wings'  ~ 'Dallas Wings',
                           opp=='Dream'  ~ 'Atlanta Dream',
                           opp=='Starzz'   ~ 'Utah Starzz',
                           opp=='Sol'  ~ 'Miami Sol',
                           opp=='Fire'  ~ 'Portland Fire',
                           opp=='Monarchs'   ~ 'Sacramento Monarchs',
                           opp=='Rockers'  ~ 'Cleveland Rockers',
                           opp=='Shock'  ~ 'Tulsa Shock',
                           opp=='Sting'  ~ 'Charlotte Sting',
                           opp=='Miracle'  ~ 'Orlando Miracle',
                           opp=='Aces' ~ 'Las Vegas Aces',
                           opp=='Wings' ~ 'Dallas Wings',
                           opp=='Comets' ~ 'Houston Comets',
                           opp=='Storm' ~ 'Seattle Storm',
                           TRUE ~ opp))
  return(data)
}

