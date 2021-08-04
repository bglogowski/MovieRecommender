## ui.R
#
# Project_4_0147_bryanmg2_BryanGlogowski
#
# CS598: PSL - Fall, 2020
# Bryan Glogowski <bryanmg2@illinois.edu>
#

library(shiny)
library(shinydashboard)
library(recommenderlab)
library(data.table)
library(ShinyRatingInput)
library(shinyjs)

source('functions/helpers.R')

shinyUI(
    dashboardPage(
          skin = "blue",
          dashboardHeader(title = "Movie Recommender"),
          
          dashboardSidebar(disable = TRUE),

          dashboardBody(includeCSS("css/movies.css"),
                        fluidRow(
                        box(width = 12,
                            title = "Step 1: Select a genre",
                            status = "info",
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            p("Select a genre to be presented with movie recommendations for that genre at the next step."),
                            div(class = "genre",
                                uiOutput('genres')
                            )
                        )
          ),
          fluidRow(
                  box(width = 12, title = "Step 2: Rate as many movies as possible",
                      status = "info",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      p("Rate the movies in your selected genre to be presented with personalized recommendations in the next step."),
                      div(class = "rateitems",
                          uiOutput('ratings')
                      )
                  )
                ),
              fluidRow(
                  useShinyjs(),
                  box(
                    width = 12, status = "info", solidHeader = TRUE,
                    title = "Step 3: Discover movies you might like",
                    
                    br(),
                    withBusyIndicatorUI(
                      actionButton("btn", "Click here to get your recommendations", class = "btn-warning")
                    ),
                    br(),
                    tableOutput("results")
                  )
               )
          )
    )
) 