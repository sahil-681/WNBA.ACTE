library(shiny)
library(ggplot2)
library(tidyverse)
library(reshape2)
library(pubtheme)
library(tidyverse)

# Data preparation ----
b2bdata <- "nob2b"
metric <- "a40"
season <- "2003"

df.grid <- readRDS(paste0("output/model/", metric, season, b2bdata, "GridXOLS102.rds"))

yvars <- c("p40", "a40", "r40", "s40", "b40", "field_goals_pct",
           "three_point_field_goals_pct", "free_throws_made_pct")
# UI
ui <- fluidPage(
  fluidRow(
    column(3, 
           selectInput(inputId = "player", 
                       label = "Choose a player", 
                       choices = sort(unique(df.grid$player)))), 
    column(3,
           selectInput(inputId = "outcome", 
                       label = "Choose outcomes", 
                       choices = yvars, multiple = TRUE))), 
  fluidRow(
    # column(3,
    #        selectInput(inputId = "team1", 
    #                    label = "Which team the player playes for?", 
    #                    choices = sort(unique(df.grid$team)))), 
    column(3, 
           selectInput(inputId = "team2", 
                       label = "Choose the team against", 
                       choices = sort(unique(df.grid$teamag)))),
    column(3, 
           selectInput(inputId = "season", 
                       label = "Choose a season", 
                       choices = sort(unique(as.character(seq(2003, 2022))))))
  ),
  
  plotOutput("line"))

server <- function(input, output) {
 
  output$line <- renderPlot({
    #setwd("~/GitHub/ACTE")
    
    outcomes <- input$outcome
    player <- input$player
    #team1 <- input$team1
    team2 <- input$team2
    s <- input$season; 
    lall <- list()
    i <- 1
    
    if (length(outcomes) > 0) {
      for (po in outcomes) {
        df.grid <- readRDS(paste0("output/model/", po, s, b2bdata, "GridXOLS102.rds"))
        if (i < length(outcomes)) {
          lall[[i]] <- df.grid[, paste0("Pred.", po)]
          i <- i + 1
        }
      }
      if (length(unique(sapply(lall, nrow))) == 1) {
        dfall <- do.call(cbind, lall)
        if (length(outcomes) > 1) {
          colnames(dfall) <- paste0("Pred.", head(outcomes, -1))
        }
        df.grid <- cbind(df.grid, dfall)
      } else {
        print("Mismatch in row numbers among elements in lall")
        return()
      }
      
      lvars <- c("age", paste0("Pred.", outcomes))
      dft <- df.grid[df.grid$player == player & 
                       #df.grid$team == team1 & 
                       df.grid$teamag == team2 &
                       df.grid$season == s, lvars]
      
      dft <- aggregate(. ~ age, dft, FUN = mean)
      dfp <- melt(dft, id.vars = "age")
      xolsp <- dfp %>% 
        ggplot(aes(x=age, y = value, group = variable, color = variable)) +
        geom_line() + 
        labs(title = "X-learner (Spline Regression)",
             subtitle = 'Heterogeneous ACTE',
             x = 'Age', 
             y = "Outcome") +  
        scale_x_continuous() + 
        scale_y_continuous() + 
        coord_cartesian(clip='off', expand=FALSE) +
        theme_pub(type='line', base_size=36/3) 
      xolsp
    }
    
  })
}

# View App
shinyApp(ui, server)
