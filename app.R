library(shiny)
library(bslib)
library(htmltools)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(sf)
library(raster)
library(scales)
library(usmap)
library(leaflet)
library(ggrepel)
library(patchwork)

# Data Preparation --------------------------------------------------------
source("preprocess.R")
# source("kriging.R")


# Plot Function -----------------------------------------------------------
source("plot.R")



# UI ----------------------------------------------------------------------
ui = dashboardPage(
  header = dashboardHeader(title = "Parking Dashboard"),
  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", icon = icon("info"), tabName = "intro"),
      menuItem("Map", icon = icon("map"), tabName = "map"),
      menuItem("Summary Plots", icon = icon("chart-bar"), tabName = "sum"),
      menuItem("Modeling", icon = icon("cubes"), tabName = "model"),
      menuItem("About", icon = icon("user"), tabName = "about")
    )
  ),
  body = dashboardBody(
    tags$head(tags$style(includeCSS("www/style.css"))),
    tabItems(
      tabItem(tabName = "intro",
        box(width = 12, status = "primary",
            includeMarkdown("www/intro.md")
        )
      ),
      tabItem(tabName = "map",
        box(title="Observed Areas", width = 12, status = "warning",
          column(6, 
                 selectInput(
                   "stateSelect", "Select a state",
                   choices = c("All Across US", 
                               sort(unique(parking$State)))
                  )
                ),
          column(6,
                 selectInput("citySelect", "Select a city",
                             choices = sort(unique(parking$City)))),
        ),
        box(width = 12, status = "primary", style="aspect-ratio:16/9",
            leafletOutput("leaflet", height = "100%"))
      ),
      tabItem(tabName = "sum",
        fluidRow(
          column(6,
            box(status = "primary", width = 12,
              selectInput("varBarSelect", "Select variable", choices = varnames), 
              plotOutput("bar_top", height = "600px")
            ),
          ),
          column(6,
            box(status = "primary", width = 12,
              selectInput("varScatterSelect", "Select variable", choices = varnames), 
              plotOutput("scatter")
            ),
            box(status = "info", width = 12,
              fluidRow(
                column(4, 
                  selectInput(
                    "stateHistSelect", "Select a state",
                    choices = c("All states", sort(unique(parking$State)))
                  )
                ),
                column(4,
                  selectInput(
                    "cityHistSelect", "Select a city",
                    choices = c("All cities in this state", sort(unique(parking$City))))
                ),
                column(4,
                  selectInput("varHistSelect", "Select variable", choices = varnames),
                )
              ),
              plotOutput("hist")
            )
          ),
        )
      ),
      tabItem(tabName = "model",
        box(title = "Kriging Interpolation",
          p("We use Kriging method to interpolate the parking",
            "lot finding time for areas without data, by changing",
            " the threshold you might be able to identify the most ",
            "difficult area for parking"),
          fluidRow(
            column(6, 
              selectInput("krigCity", "Select City", choices = names(krig))),
            column(6,
              sliderInput("krigThre", "Select Threshold",
                          value = 0.7, min = 0, max = 1, ticks=FALSE))
          ),
          leafletOutput("krig", width = "100%", height = "600px"),
          width = 8
        )
      ),
      tabItem(tabName = "about",
        box(width = 12, status = "info",
          includeMarkdown("www/about.md")
        )
      )
    )
  )
)

# Server ------------------------------------------------------------------
server = function(input, output, session) {
  observe({
    if(input$stateSelect != "All Across US") {
      updateSelectInput(
        session, "citySelect",
        choices = parking %>% 
          filter(State == input$stateSelect) %>%
          pull(City) %>% unique() %>% sort()
      )
    }
  })
  leaflet_ds = reactive(
    if(input$stateSelect == "All Across US" ||
       is.null(input$citySelect)) parking
    else dplyr::filter(parking, City == input$citySelect, 
                       State == input$stateSelect)
  )
  output$leaflet = renderLeaflet(plot_leaflet(leaflet_ds()))
  output$krig = renderLeaflet(plot_krig_new(krig[[input$krigCity]], input$krigThre))
  output$bar_top = renderPlot(plot_bar_top(county_mean, input$varBarSelect))
  output$scatter = renderPlot(plot_scatter(county_mean, input$varScatterSelect))
  
  observe({
    if(input$stateHistSelect != "All states") {
      city_choices = parking %>% 
        filter(State == input$stateHistSelect) %>%
        pull(City) %>% unique() %>% sort()
      updateSelectInput(
        session, "cityHistSelect",
        choices = c("All cities in this state", city_choices)
      )
    }
  })
  output$hist = renderPlot(
    plot_hist(input$stateHistSelect, input$cityHistSelect, input$varHistSelect))
}

shinyApp(ui, server)

