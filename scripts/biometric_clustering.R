library(broom)
library(dplyr)
library(tidyr)
library(pubtheme)
library(tidyverse)

biometric_clustering <- function(data) {
  # Ensure data is in numerical format
  data$ht <- as.numeric(as.character(data$ht))
  data$wt <- as.numeric(as.character(data$wt))
  data <- data[!is.na(data$ht) & 
                 !is.na(data$wt) & 
                 !is.infinite(data$ht) & 
                 !is.infinite(data$wt), ]
  
  # Perform K-means clustering
  set.seed(123)
  clusters <- kmeans(data[, c("ht", "wt")], centers = 3)
  
  # Add cluster assignments to data
  data$cluster <- clusters$cluster
  
  return(data)
}

kmeans_plot <- function(data) {
  ggplot(data, aes(x = ht, y = wt, color = as.factor(cluster))) +
    geom_point() +
    labs(title = "K-means Clustering of WNBA Players by Height and Weight",
         x = "Height",
         y = "Weight",
         color = "Cluster") +
    theme_pub(type = "scatter")
}
