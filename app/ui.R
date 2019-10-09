library(plyr)
library(shiny)
library(shinythemes)
library(leaflet.extras)
library(ggmap)
library(dplyr)
library(leaflet)
library(tigris)
library(RJSONIO)
library(RCurl)
library(tidyverse)
library(ggmap)
library(maptools)

setwd("../output")
load("data.RData")
load("uniquedata.RData")


ui <- bootstrapPage(theme = shinytheme("cyborg"),
                    navbarPage(title="New York City Restaurants",
                               
                               tabPanel("Score Comparison", titlePanel("Score Distribution"),
                                        # Sidebar layout with input and output definitions ----
                                        sidebarLayout(
                                          # Sidebar panel for inputs ----
                                          sidebarPanel(
                                            selectInput('borough1', 'region', 
                                                        borough_list, selected='Brooklyn'),
                                            selectInput('cuisine1', 'cuisine type', 
                                                        cuisine_list1, selected='Pizza')),
                                          # Main panel for displaying outputs ----
                                          mainPanel(
                                            plotOutput(outputId = "plots4", height = "300"),
                                            h4("Summary"),
                                            verbatimTextOutput("Summary1"))),
                                        hr(),
                                        sidebarLayout(
                                          # Sidebar panel for inputs ----
                                          sidebarPanel(
                                            selectInput('borough2', 'region', 
                                                        borough_list, selected='Brooklyn'),
                                            selectInput('cuisine2', 'cuisine type', 
                                                        cuisine_list1, selected='Pizza')),
                                          # Main panel for displaying outputs ----
                                          mainPanel(
                                            plotOutput(outputId = "plots5", height = "300"),
                                            h4("Summary"),
                                            verbatimTextOutput("Summary2")
                                          ))
                               ),
                               
                               tabPanel("Restaurant recommendation",
                                        titlePanel("Top 5 Recommended Restaurants"),
                                        # Sidebar layout with input and output definitions ----
                                        sidebarLayout(
                                          # Sidebar panel for inputs ----
                                          sidebarPanel(
                                            selectInput('borough', 'region', 
                                                        borough_list, selected='Brooklyn')),
                                          # Main panel for displaying outputs ----
                                          mainPanel(
                                            plotOutput(outputId = "plots1", height = "300"))),
                                        hr(),
                                        sidebarLayout(
                                          # Sidebar panel for inputs ----
                                          sidebarPanel(
                                            selectInput('cuisine', 'cuisine type', 
                                                        cuisine_list1, selected='Pizza')),
                                          # Main panel for displaying outputs ----
                                          mainPanel(
                                            plotOutput(outputId = "plots2", height = "300"),
                                            plotOutput(outputId = "plots3", height = "300"))),
                                        hr(),
                                        sidebarLayout(
                                          # Sidebar panel for inputs ----
                                          sidebarPanel(
                                            numericInput("top", "Top restaurants to view:", 5)),
                                          # Main panel for displaying outputs ----
                                          mainPanel(
                                            h4("Top restaurants"),
                                            tableOutput("view")))),
                               
                               tabPanel("Zipcode Map Visualization",
                                        div(class="outer",includeCSS("style_1.css"),
                                            
                                            tags$style(type = "text/css", ".outer {position: fixed; top: 50px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                                            
                                            tags$head(tags$style(HTML('#controls {background-color: rgba(0,0,0,0.45);}'))),
                                            
                                            leafletOutput("heat_map", width = "100%", height = "100%"),
                                            absolutePanel(top = 10, left = 10,
                                                          
                                                          
                                                          checkboxGroupInput("restaurant_choice", "Restaurant Choice:",
                                                                             c("Chinese" = '1',
                                                                               "Japanese" = '2',
                                                                               "Mexican" = '3',
                                                                               "Pizza" = '4',
                                                                               "Bakery" = '5'
                                                                             )),
                                                          checkboxInput("legend", "Show legend", TRUE), draggable = TRUE
                                            )
                                        )),
                               
                               
                               tabPanel("Borough Map Visualization",
                                        div(class="outer",includeCSS("style_1.css"),
                                            
                                            tags$style(type = "text/css", ".outer {position: fixed; top: 50px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                                            
                                            tags$head(tags$style(HTML('#controls {background-color: rgba(0,0,0,0.45);}'))),
                                            
                                            leafletOutput("map", width = "100%", height = "100%"),
                                            absolutePanel(top = 10, left = 10,
                                                          
                                                          
                                                          selectInput("boro", "Borough",
                                                                      choices = boro
                                                          ),
                                                          selectInput("cusine", "Cusine",
                                                                      choices = cusine
                                                          ), draggable = TRUE
                                            )
                                        )),
                               
                               
                               tabPanel("Contact", div(class="outer",includeCSS("style_1.css"),
                                                       
                                                       tags$style(type = "text/css", ".outer {position: fixed; top: 50px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                                                       
                                                       tags$head(tags$style(HTML('#controls {background-color: rgba(0,0,0,0.45);}'))),
                                                       
                                                       plotOutput("plot_nyc")
                               )))
)
