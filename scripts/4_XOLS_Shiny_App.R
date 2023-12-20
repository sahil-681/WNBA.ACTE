library(shiny)
library(ggplot2)
library(tidyverse)
library(reshape2)
library(shinyjs)
library(pubtheme)

b2bdata <- "nob2b"
metric <- "a40"
season <- "2003"
df.grid <- readRDS(paste0("../output/clusteredmodel/cluster1/", metric, season, b2bdata, "GridXOLS102_cluster1.rds"))
yvars <- c("p40", "a40", "r40", "s40", "b40", "field_goals_pct",
           "three_point_field_goals_pct", "free_throws_made_pct")

# UI
ui <- fluidPage(
  useShinyjs(),  # Initialize shinyjs

  tabsetPanel(
    tabPanel(
      "Player Comparison",
      fluidRow(
        column(3,
               selectInput(inputId = "player",
                           label = "Choose a player",
                           choices = sort(unique(df.grid$player)))),
        column(3,
               selectInput(inputId = "outcome",
                           label = "Choose outcomes (at least 2)",
                           choices = yvars, multiple = TRUE, selected = c("a40", "p40")))),
      fluidRow(
        column(3,
               selectInput(inputId = "team2",
                           label = "Choose the team against",
                           choices = sort(unique(df.grid$teamag)))),
        column(3,
               selectInput(inputId = "season",
                           label = "Choose a season",
                           choices = sort(unique(as.character(seq(2003, 2022)))))),
        column(3,
               # Add an actionButton
               actionButton(inputId = "submit_button", label = "Submit")
        )
      ),
      plotOutput("line")
    ),

    tabPanel(
      "Cluster Comparison",
      fluidRow(
        column(3,
               selectInput(inputId = "cluster_player",
                           label = "Choose a player",
                           choices = sort(unique(df.grid$player)))),
        column(3,
               selectInput(inputId = "cluster_outcome",
                           label = "Choose outcomes (at least 2)",
                           choices = yvars, multiple = TRUE, selected = c("a40", "p40"))),
        column(3,
               selectInput(inputId = "cluster_team2",
                           label = "Choose the team against",
                           choices = sort(unique(df.grid$teamag)))),
        column(3,
               selectInput(inputId = "cluster_season",
                           label = "Choose a season",
                           choices = sort(unique(as.character(seq(2003, 2022)))))),
        column(3,
               selectInput(inputId = "cluster_number",
                           label = "Choose clusters",
                           choices = c(1, 2, 3), multiple = FALSE)),
        column(3,
               # Add an actionButton for cluster comparison
               actionButton(inputId = "cluster_submit_button", label = "Submit"))
      ),
      plotOutput("cluster_line")
    )
  )
)

# Server
server <- function(input, output, session) {

  # Player Comparison

  observe({
    shinyjs::enable("submit_button")
  })

  submit_click <- eventReactive(input$submit_button, {
    outcomes <- input$outcome
    player <- input$player
    team2 <- input$team2
    s <- input$season

    lall <- list()
    i <- 1

    if (length(outcomes) > 0) {
      for (po in outcomes) {
        df.grid <- readRDS(paste0("../output/model/", po, s, "nob2b", "GridXOLS102.rds"))
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

  output$line <- renderPlot({
    submit_click()
  })

  # Cluster Comparison
  observe({
    shinyjs::enable("cluster_submit_button")

    # Update player choices based on selected cluster
    selected_cluster <- input$cluster_number
    s <- input$cluster_season
    df.grid_cluster <- readRDS(paste0("../output/clusteredmodel/cluster", selected_cluster, "/a40", s, b2bdata, "GridXOLS102_cluster", selected_cluster, ".rds"))

    updateSelectInput(session, "cluster_player", choices = sort(unique(df.grid_cluster$player)))
  }, priority = 100)

  cluster_submit_click <- eventReactive(input$cluster_submit_button, {
    outcomes <- input$cluster_outcome
    player <- input$cluster_player
    team2 <- input$cluster_team2
    s <- input$cluster_season
    clusters <- input$cluster_number

    lall <- list()
    i <- 1

    if (length(outcomes) > 0) {
      for (po in outcomes) {
        df.grid <- readRDS(paste0("../output/clusteredmodel/cluster", clusters, "/", po, s, b2bdata, "GridXOLS102_cluster", clusters, ".rds"))
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

  output$cluster_line <- renderPlot({
    cluster_submit_click()
  })
}

# Run the app
shinyApp(ui, server)
