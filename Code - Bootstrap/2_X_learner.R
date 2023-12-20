library(Rforestry)
library(ggplot2)
library(dplyr)
library(splines)
library(reshape2)
library(lme4)
library(tidyverse)
source("Code/X_RF_OLS.R")
df <- readRDS("Data/1_Cleaned_df.rds")
lfeatures <- c("age", "team", "player", "teamag", "homeind", "season", 'lagmin') 

# Make factor
lfactor <- c("player", "team", "season.type", "country", "away", "home",
             "teamag", "homeind", "season", "pos")
df[, lfactor] <- lapply(df[, lfactor], as.factor)
# Age above 39 and 18 has overlap assumption violation?
table(df$age, df$trt)
df <- df %>% filter(age<35, age>18)
lp <- as.character(unique(df$player))
lplayer <- lp[table(as.character(df$player)) > 100]
df <- df[df$player %in% lplayer, ]
dim(df)
# yvars <- c("net.rat", "off.rat", "def.rat", "p100", "a100", "r100", "s100", 
#            "b100", "to100", "fgp", "oreb.p", "dreb.p", "ts.p",
#            "or100", "dr100", 'fg3p')
yvars <- c("p100", "a100", "r100")
trt <- 'trt'
# yvars <- c('net.rat', 'p100')
for (yname in yvars){
  x_data <- df[, lfeatures]; y_data <- df[, yname]; w_data <- df[, trt]
  xl_rf <- x.rf(x_data, w_data, y_data)
  df[, paste0("Pred.", yname)] <- x.est(xl_rf, df[, lfeatures])
}

saveRDS(df, "Output/Predictions/X_Pred.rds")
############
# Calculate ACTE for X-learner
############
df <- readRDS("Output/Predictions/X_Pred.rds")

# Create a list of all diff_Pred variables
diff_vars <- grep("Pred.", names(df), value = TRUE)

# Calculate mean and standard deviation for each diff_Pred variable grouped by age
result <- df %>% 
  group_by(player, age) %>% 
  summarise(across(all_of(diff_vars), list(mean = ~ mean(., na.rm = TRUE))))

result <- result %>% 
  group_by(age) %>% 
  summarise(across(all_of(paste0(diff_vars, '_mean')), list(mean = ~ mean(., na.rm = TRUE), sd = ~ sd(., na.rm = TRUE))))
result <- data.frame(result)

library(ggplot2)
library(tidyr)

# Selecting only relevant columns (mean and sd columns for each variable)
selected_cols <- grep("mean$|sd$", names(long_data_1), value = TRUE)

# Reshaping the data to a long format for plotting
long_plot_data <- long_data_1 %>%
  select(age, variable, all_of(selected_cols)) %>%
  pivot_longer(-c(age, variable), 
               names_to = "variable_stat", 
               values_to = "value") %>%
  separate(variable_stat, into = c("stat_variable", "stat"), sep = "_") %>%
  pivot_wider(names_from = "stat", values_from = "value") 


# Adding a new column to check if zero is in the interval
scale_sd <- 1
long_plot_data$zero_in_interval <- long_plot_data$mean - scale_sd*long_plot_data$sd <= 0 & 
  long_plot_data$mean + scale_sd*long_plot_data$sd >= 0

# Plotting
ggplot(long_plot_data %>% filter(stat_variable %in% c('Pred.net.rat','Pred.off.rat', 'Pred.def.rat' )), aes(x = age, y = mean, group = stat_variable, color = stat_variable)) +
  geom_line() +
  geom_ribbon(aes(ymin = mean - scale_sd*sd, ymax = mean + scale_sd*sd, fill = stat_variable), alpha = 0.2) +
  labs(title = "Mean and Standard Deviation of diff_Pred Variables by Age",
       x = "Age", y = "Mean Value") +
  theme_minimal() +
  guides(color = guide_legend(title = "Variables"), fill = guide_legend(title = "Variables"))

# Checking if there are any variables where zero is not in the interval
any_zero_not_in_interval <- any(!long_plot_data$zero_in_interval)

# Viewing the result
any_zero_not_in_interval
