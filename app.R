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

# https://github.com/owid/covid-19-data/blob/master/public/data/README.md
# owid <-  read.csv2("/data/owid-covid-data.csv",
#                    header = TRUE, sep = ",", dec = "." )

df <- data.table::fread("D:/Code/R/Statistik-Projekt/data/owid-covid-data.csv")

extractCountryData <- function(country) {
  loc <- subset(df, location == country)
  
}






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
                min = (as.Date("2020-03-01","%Y-%m-%d")),
                max = (as.Date("2022-01-01","%Y-%m-%d")),
                value = c(as.Date("2020-01-01","%Y-%m-%d"),as.Date("2022-01-01","%Y-%m-%d")),
                ticks = FALSE
                
    ),
    

    # checkboxInput(inputId = "ger", label = "Germany", value=FALSE, width = NULL),
    checkboxGroupInput("checkGroup", 
                       h3("Checkbox group"), 
                       choices = list("Germany" = 1 , 
                                      "France" = 2, 
                                      "Netherlands" = 3),
                       selected = 1)
    ),
    
  dashboardBody(
    
    textOutput("min_max"),
    
    
    box(plotOutput("plot"), width = 8)

  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  output$min_max <- renderText({ 
    paste("You have chosen a range that goes from",
          input$Date[1], "to", input$Date[2])
  })
  
  
  
  reactive({
  loc <- extractCountryData(input$checkGroup[1])
  })
  output$min_max <- renderText({ 
    paste()})
  
  output$plot <- renderPlot({
    
    y <- seq.Date(input$Date[1], input$Date[2], 1)
    
    ggplot(data = loc ,aes(x = as.Date(date), y = new_cases)) +
      xlim(input$Date[1], input$Date[2]) +
      geom_line()

  })
  
  
  
  
  

#   output$ger <- renderTable(extractCountryData("Germany")) 
#   
# 
#   output$range <- seq(input$Date[1], input$Date[2], 1)
#   
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