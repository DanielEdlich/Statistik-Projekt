#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(tidyverse)
library(data.table)
library(DT)
library(plotly)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "Covid19 Statistik", titleWidth = 300), 
  dashboardSidebar(
    includeCSS("www/style.css"),
    selectInput(inputId="var_a",
                label="Choose Variable:",
                choices= list(cases="Cases",
                              deaths= "deaths",
                              vaccinated="vaccinated")),
    sliderInput(inputId = "Date",
                label = "Choose Date:",
                min = (as.Date("2020-01-01","%Y-%m-%d")),
                max = (as.Date("2022-01-01","%Y-%m-%d")),
                value = c(as.Date("2020-01-01","%Y-%m-%d"),as.Date("2022-01-01","%Y-%m-%d")),
                ticks = FALSE
                
    ),
    
    checkboxInput(inputId = "ger", label = "Germany", value=FALSE, width = NULL)
    
  ),
  dashboardBody(
    
    # plotlyOutput(outputId = p)
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # https://github.com/owid/covid-19-data/blob/master/public/data/README.md
  owid <-  read.csv2("https://covid.ourworldindata.org/data/owid-covid-data.csv", 
                     header = TRUE, sep = ",", dec = "." )
  
# 
#   loc <- subset(owid, location == "Germany")
#   range <- input$Date # interval
  
  # output$p <- renderPlotly({
  #   plot_ly(loc, x = ~date, y = ~new_cases) 
  #     add_lines()
  # })
  
  
  # loc <- subset(owid, location == "Germany")
  # # plot(as.Date(loc[,'date']), loc[, 'new_cases'], type = 'l')
  # y <- c(150:200)
  # plot(as.Date(loc[y,'date']), loc[y, 'new_cases'], type = 'l',
  #      main = 'cases in Gemany per day', xlab = 'day', ylab = 'cases')
  
  
}


# Run the application 
shinyApp(ui = ui, server = server)