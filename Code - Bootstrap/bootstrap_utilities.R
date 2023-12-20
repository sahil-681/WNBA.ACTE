
library(dplyr)
library(tidyverse)
library(splines)
# source("Code/2_X_learner.R")

# packages for 2_X_learner.R
library(Rforestry)
library(ggplot2)
library(dplyr)
library(splines)
library(reshape2)
library(lme4)
library(tidyverse)
library(pubtheme)
source("Code/X_RF_OLS.R")

# for multi-core use
library(doParallel)
library(foreach)


get_pairs_names <- function(df, min_per_player=25, num_games_together=1, return_n_unique=F){
  
  comb_list <- list()
  
  teams <- unique(r_df$team)
  seasons <- unique(r_df$season)
  
  for(season in seasons){
    for(team in teams){
      
      team_df <- r_df[r_df$team == team & r_df$season == season, ]
      
      pids <- unique(team_df$pid)
      
      for(i in 1:(length(pids)-1)){
        
        p1_gamesid <- team_df$gid[team_df$pid==pids[i] & team_df$mins >= min_per_player]
        
        for(j in (i+1):length(pids)){
          
          p2_gamesid <- team_df$gid[team_df$pid==pids[j] & team_df$mins >= min_per_player]
          
          
          bothplayed_gids <- intersect(p1_gamesid, p2_gamesid)
          
          
          if(length(bothplayed_gids) >= num_games_together){
            
            comb_list[[length(comb_list)+1]] <- data.frame(
              "pid1" = pids[i],
              "pid2" = pids[j],
              "season" = season,
              "team" = team,
              "gid" = bothplayed_gids
            )
          }
        }
      }
    }
  }
  
  pairs_df <- bind_rows(comb_list)
  n_unique_pairs <- length(unique(paste(pairs_df$pid1, pairs_df$pid2)))
  
  print(paste("unique pairs: ", n_unique_pairs))
  print(paste("pair samples: ", dim(pairs_df)[1]))
  
  if(return_n_unique){
    return(list(pairs_df, n_unique_pairs))
  }else{
    return(pairs_df)
  }

}




bootstrap_function <- function(pairs_df, r_df, iter, boot_size, yvars = c("net.rat")){

  sampled_rows <- pairs_df %>%
    sample_n(boot_size, replace=TRUE) %>% 
    pivot_longer(cols=starts_with("pid"), names_to="order", values_to="pid")
  
  boot_df <- merge(sampled_rows[c("gid", "pid")],
                   r_df, by=c("gid", "pid"))
  
  
  #define features and factors for regression
  lfeatures <- c("age", "team", "player", "teamag", "homeind", "season", 'lagmin') 
  lfactor <- c("player", "team", "season.type", "country", "away", "home",
               "teamag", "homeind", "season", "pos")
  boot_df[, lfactor] <- lapply(boot_df[, lfactor], as.factor)
  
  # Age above 39 and 18 has overlap assumption violation, filter
  table(boot_df$age, boot_df$trt)
  boot_df <- boot_df %>% filter(age<35, age>18)
  lp <- as.character(unique(boot_df$player))
  lplayer <- lp[table(as.character(boot_df$player)) > 1] # was 100, but then no code
  boot_df <- boot_df[boot_df$player %in% lplayer, ]

  trt <- 'trt'
  for (yname in yvars){
    x_data <- boot_df[, lfeatures]; y_data <- boot_df[, yname]; w_data <- boot_df[, trt]
    xl_rf <- x.rf(x_data, w_data, y_data)
    boot_df[, paste0("Pred.", yname)] <- x.est.rf(xl_rf, boot_df[, lfeatures]) # assume use rf?
  }
  
  # get mean of each pred
  diff_vars <- grep("Pred.", names(boot_df), value = TRUE)
  
  # avg stats per player
  result <- boot_df %>% 
    group_by(player, age) %>% 
    summarise(across(all_of(diff_vars), list(mean = ~ mean(., na.rm = TRUE))))
  
  # avg across all players league wide  
  result <- result %>% 
    group_by(age) %>% 
    summarise(across(all_of(paste0(diff_vars, '_mean')), list(mean = ~ mean(., na.rm = TRUE)))) %>%
    rename_with(~gsub("_mean_mean", "", .x), matches("_mean_mean")) %>%
    pivot_longer(-c(age),
                 names_to="stat_variable",
                 values_to = "value")
  
  result$boot_idx <- iter
  
  print(paste("finished ", iter))
  
  return(result)
}

initiate_bootstrap <- function(pairs_df, r_df, n_boots, boot_size, ncores=4, yvars=c('net.rat')){
  
  if(ncores >= detectCores()){
    print("Error: More cores than you have, defaulting to 4 Cores instead")
    ncores <- 4
  }
  cl <- makeCluster(ncores)
  registerDoParallel(cl)
  
  # for multicore, need to load packages to each core
  packages_needed <- c("dplyr", "tidyverse", "splines", "Rforestry", "dplyr", "reshape2", "lme4")
  
  print(paste("Starting Bootstrap for ", paste(yvars, collapse = ", ")))
  start_t <- Sys.time()
  
  all_results <- foreach(i = 1:n_boots, .combine="rbind", .packages=packages_needed) %dopar% {
    source("Code/X_RF_OLS.R")
    source("Code/bootstrap_utilities.R")
    bootstrap_function(pairs_df, r_df, i, boot_size=boot_size, yvars=yvars)
  }
  stopCluster(cl)
  
  end_t <- Sys.time()
  
  print(end_t - start_t)
  return(all_results)
}


