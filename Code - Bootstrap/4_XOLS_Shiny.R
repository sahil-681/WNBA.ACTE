library(shiny)
library(ggplot2)
library(tidyverse)
library(reshape2)
library(orgthemes)

# Data preparation ----
setwd("~/GitHub/ACTE")
b2bdata <- "nob2b"

df.grid <- readRDS(paste0("Output/Model/", b2bdata, "GridXOLS102.rds"))
library(shiny)
library(tidyverse)
yvars <- c("net.rat", "off.rat", "def.rat", "p100", "a100", "r100", "s100", 
           "b100", "to100", "fgp", "ftp", "oreb.p", "dreb.p", "ts.p",
           "or100", "dr100")
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
    column(3,
           selectInput(inputId = "team1", 
                       label = "Which team the player playes for?", 
                       choices = sort(unique(df.grid$team)))), 
    column(3, 
           selectInput(inputId = "team2", 
                       label = "Choose the team against", 
                       choices = sort(unique(df.grid$teamag)))),
    column(3, 
           selectInput(inputId = "season", 
                       label = "Choose a season", 
                       choices = sort(unique(as.character(seq(2012, 2022))))))
  ),
  
  plotOutput("line"))

server <- function(input, output) {
 
  output$line <- renderPlot({
    setwd("~/GitHub/ACTE")
    
    outcomes <- input$outcome; player <- input$player
    team1 <- input$team1; team2 <- input$team2; s <- input$season; 
    lall <- list(); i <- 1
    print(outcomes)
    print(input)
    if (length(outcomes) > 0){
      for (po in outcomes){
        
        df.grid <- readRDS(paste0("Output/Model/", po, s, b2bdata, 
                                  "GridXOLS102.rds"))
        if (i < length(outcomes)){
          lall[[i]] <- df.grid[, paste0("Pred.", po)]
          i <- i + 1
        }
      }
      dfall <- do.call(cbind, lall)
      if (length(outcomes) > 1){
        colnames(dfall) <- paste0("Pred.", head(outcomes, -1))
      }
      df.grid <- cbind(df.grid, dfall)
      lvars <- c("age", paste0("Pred.", outcomes))
      dft <- df.grid[df.grid$player == player & 
                       df.grid$team == team1 & 
                       df.grid$teamag == team2 &
                       df.grid$season == s,
                     lvars]
      
      dft <- aggregate(. ~ age, dft, FUN = mean)
      dfp <- melt(dft, id.vars = "age")
      xolsp <- dfp %>% ggplot(aes(x=age, y = value, group = variable,
                                  color = variable)) +
        geom_line() + 
        labs(title    = "X-learner (Spline Regression)",
             subtitle = 'Heterogeneous ACTE', x = 'Age', 
             y = "Outcome") +  
        scale_x_continuous() + 
        scale_y_continuous() + 
        coord_cartesian(clip='off', expand=FALSE) +
        theme_org(type='line', base_size=36/3) 
      xolsp
    }
    
  })
}

# View App
shinyApp(ui, server)
