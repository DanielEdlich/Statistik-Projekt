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
library(ggplot2)
# library(plotly)

df <- data.table::fread("D:/Code/R/Statistik-Projekt/data/owid-covid-data.csv")
# df <- data.table::fread("https://covid.ourworldindata.org/data/owid-covid-data.csv")
extractCountryData <- function(country) {
  loc <- subset(df, location == country, )
}






# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "Covid19 Statistik", titleWidth = 300), 
  dashboardSidebar(
    includeCSS("www/style.css"),
    selectInput(inputId="var",
                label="Choose Variable:",
                choices= list(cases="new_cases_smoothed_per_million",
                              deaths= "new_deaths_smoothed_per_million",
                              vaccinated="new_vaccinations_smoothed_per_million")),
    sliderInput(inputId = "Date",
                label = "Choose Date:",
                min = (as.Date(min(df$date),"%Y-%m-%d")),
                max = (as.Date(max(df$date),"%Y-%m-%d")),
                value = c(as.Date(min(df$date),"%Y-%m-%d"),(as.Date(max(df$date),"%Y-%m-%d"))),
                ticks = FALSE
                
    ),
    
    

    # checkboxInput(inputId = "ger", label = "Germany", value=FALSE, width = NULL),
    checkboxGroupInput("checkGroup", 
                       h3("Checkbox group"), 
                       choices = list("Austria" = "Austria",
                                       "Belgium" = "Belgium",
                                       "Bulgaria" = "Bulgaria",
                                       "Czechia" = "Czechia",
                                       "Denmark" = "Denmark",
                                       "Estonia" = "Estonia",
                                       "Finland" = "Finland",
                                       "France" = "France",
                                       "Germany" = "Germany",
                                       "Greece" = "Greece",
                                       "Hungary" = "Hungary",
                                       "Iceland" = "Iceland",
                                       "Ireland" = "Ireland",
                                       "Italy" = "Italy",
                                       "Luxembourg" = "Luxembourg",
                                       "Netherlands" = "Netherlands",
                                       "Norway" = "Norway",
                                       "Poland" = "Poland",
                                       "Portugal" = "Portugal",
                                       "Slovakia" = "Slovakia",
                                       "Slovenia" = "Slovenia",
                                       "Spain" = "Spain",
                                       "Sweden" = "Sweden",
                                       "Switzerland" = "Switzerland",
                                       "Ukraine" = "Ukraine",
                                       "United Kingdom" = "United Kingdom"
                                     ),
                       selected = "Germany")
    ),
    
  dashboardBody(
    
    box(plotOutput("plot"), width = 15)

  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  
  loc <- reactive({
    
    extractCountryData(input$checkGroup)
    
   })
  
  var <- reactive({input$var})
  
  legend <- c("Austria" = "#FFAA00",
              "Belgium" = "#FFF000",
              "Bulgaria" = "#BDFF00",
              "Czechia" = "#13FF00",
              "Denmark" = "#00FFD1",
              "Estonia" = "#00E0FF",
              "Finland" = "#FF0000",
              "France" = "#0097FF",
              "Germany" = "#0068FF",
              "Greece" = "#5D00FF",
              "Hungary" = "#9300FF",
              "Iceland" = "#BD00FF",
              "Ireland" = "#F300FF",
              "Italy" = "#FF00E0",
              "Luxembourg" = "#FF0097",
              "Netherlands" = "#FF0070",
              "Norway" = "#FF0036",
              "Poland" = "#FF001B",
              "Portugal" = "#7CD1FF",
              "Slovakia" = "#2C8F57",
              "Slovenia" = "#0C4B6D",
              "Spain" = "#ABB462",
              "Sweden" = "#77478B",
              "Switzerland" = "#BF6FA8",
              "Ukraine" = "#0A4E0D",
              "United Kingdom" = "#B7984B")
  
  output$plot <- renderPlot({
    
    
    ggplot() +
      xlim(input$Date[1], input$Date[2]) +
      # ylim() +
      {if ("Austria" %in% input$checkGroup) geom_line(data = extractCountryData("Austria"), aes(x = as.Date(date, "%Y-%m-%d"), y = eval(as.name(input$var)), color = "Austria") )} +
      {if ("Belgium" %in% input$checkGroup) geom_line(data = extractCountryData("Belgium"), aes(x = as.Date(date, "%Y-%m-%d"), y = eval(as.name(input$var)), color = "Belgium" ))} +
      {if ("Bulgaria" %in% input$checkGroup) geom_line(data = extractCountryData("Bulgaria"), aes(x = as.Date(date, "%Y-%m-%d"), y = eval(as.name(input$var)), color = "Bulgaria" ))} +
      {if ("Czechia" %in% input$checkGroup) geom_line(data = extractCountryData("Czechia"), aes(x = as.Date(date, "%Y-%m-%d"), y = eval(as.name(input$var)), color = "Czechia" ))} +
      {if ("Denmark" %in% input$checkGroup) geom_line(data = extractCountryData("Denmark"), aes(x = as.Date(date, "%Y-%m-%d"), y = eval(as.name(input$var)), color = "Denmark" ))} +
      {if ("Estonia" %in% input$checkGroup) geom_line(data = extractCountryData("Estonia"), aes(x = as.Date(date, "%Y-%m-%d"), y = eval(as.name(input$var)), color = "Estonia" ))} +
      {if ("Finland" %in% input$checkGroup) geom_line(data = extractCountryData("Finland"), aes(x = as.Date(date, "%Y-%m-%d"), y = eval(as.name(input$var)), color = "Finland" ))} +
      {if ("France" %in% input$checkGroup) geom_line(data = extractCountryData("France"), aes(x = as.Date(date, "%Y-%m-%d"), y = eval(as.name(input$var)), color = "France" ))} +
      {if ("Germany" %in% input$checkGroup) geom_line(data = extractCountryData("Germany"), aes(x = as.Date(date, "%Y-%m-%d"), y = eval(as.name(input$var)), color = "Germany" ))} +
      {if ("Greece" %in% input$checkGroup) geom_line(data = extractCountryData("Greece"), aes(x = as.Date(date, "%Y-%m-%d"), y = eval(as.name(input$var)), color = "Greece" ))} +
      {if ("Hungary" %in% input$checkGroup) geom_line(data = extractCountryData("Hungary"), aes(x = as.Date(date, "%Y-%m-%d"), y = eval(as.name(input$var)), color = "Hungary" ))} +
      {if ("Iceland" %in% input$checkGroup) geom_line(data = extractCountryData("Iceland"), aes(x = as.Date(date, "%Y-%m-%d"), y = eval(as.name(input$var)), color = "Iceland" ))} +
      {if ("Ireland" %in% input$checkGroup) geom_line(data = extractCountryData("Ireland"), aes(x = as.Date(date, "%Y-%m-%d"), y = eval(as.name(input$var)), color = "Ireland" ))} +
      {if ("Italy" %in% input$checkGroup) geom_line(data = extractCountryData("Italy"), aes(x = as.Date(date, "%Y-%m-%d"), y = eval(as.name(input$var)), color = "Italy" ))} +
      {if ("Luxembourg" %in% input$checkGroup) geom_line(data = extractCountryData("Luxembourg"), aes(x = as.Date(date, "%Y-%m-%d"), y = eval(as.name(input$var)), color = "Luxembourg" ))} + 
      {if ("Netherlands" %in% input$checkGroup) geom_line(data = extractCountryData("Netherlands"), aes(x = as.Date(date, "%Y-%m-%d"), y = eval(as.name(input$var)), color = "Netherlands" ))} +
      {if ("Norway" %in% input$checkGroup) geom_line(data = extractCountryData("Norway"), aes(x = as.Date(date, "%Y-%m-%d"), y = eval(as.name(input$var)), color = "Norway" ))} +
      {if ("Poland" %in% input$checkGroup) geom_line(data = extractCountryData("Poland"), aes(x = as.Date(date, "%Y-%m-%d"), y = eval(as.name(input$var)), color = "Poland" ))} +
      {if ("Portugal" %in% input$checkGroup) geom_line(data = extractCountryData("Portugal"), aes(x = as.Date(date, "%Y-%m-%d"), y = eval(as.name(input$var)), color = "Portugal" ))} +
      {if ("Slovakia" %in% input$checkGroup) geom_line(data = extractCountryData("Slovakia"), aes(x = as.Date(date, "%Y-%m-%d"), y = eval(as.name(input$var)), color = "Slovakia" ))} +
      {if ("Slovenia" %in% input$checkGroup) geom_line(data = extractCountryData("Slovenia"), aes(x = as.Date(date, "%Y-%m-%d"), y = eval(as.name(input$var)), color = "Slovenia" ))} +
      {if ("Spain" %in% input$checkGroup) geom_line(data = extractCountryData("Spain"), aes(x = as.Date(date, "%Y-%m-%d"), y = eval(as.name(input$var)), color = "Spain" ))} +
      {if ("Sweden" %in% input$checkGroup) geom_line(data = extractCountryData("Sweden"), aes(x = as.Date(date, "%Y-%m-%d"), y = eval(as.name(input$var)), color = "Sweden" ))} +
      {if ("Switzerland" %in% input$checkGroup) geom_line(data = extractCountryData("Switzerland"), aes(x = as.Date(date, "%Y-%m-%d"), y = eval(as.name(input$var)), color = "Switzerland" ))} +
      {if ("Ukraine" %in% input$checkGroup) geom_line(data = extractCountryData("Ukraine"), aes(x = as.Date(date, "%Y-%m-%d"), y = eval(as.name(input$var)), color = "Ukraine" ))} +
      {if ("United Kingdom" %in% input$checkGroup) geom_line(data = extractCountryData("United Kingdom"), aes(x = as.Date(date, "%Y-%m-%d"), y = eval(as.name(input$var)), color = "United Kingdom" ))} +
     
      labs(x = "date",
           color = "legend") +
      switch( input$var,
        new_cases_smoothed_per_million = ylab("cases in million"),
        new_deaths_smoothed_per_million = ylab("deaths in million"),
        new_vaccinations_smoothed_per_million = ylab("vacination in million")
      ) 
    
    

  })
  

  
}


# Run the application 
shinyApp(ui = ui, server = server)