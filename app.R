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
# library(plotly)

# https://github.com/owid/covid-19-data/blob/master/public/data/README.md
# owid <-  read.csv2("/data/owid-covid-data.csv",
#                    header = TRUE, sep = ",", dec = "." )

df <- data.table::fread("D:/Code/R/Statistik-Projekt/data/owid-covid-data.csv")
#df <- data.table::fread("https://covid.ourworldindata.org/data/owid-covid-data.csv")
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
                value = c(as.Date(min(df$date),"%Y-%m-%d"),as.Date("2022-01-01","%Y-%m-%d")),
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
    
    textOutput("min_max"),
    
    box(plotOutput("plot"), width = 15)

  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  
  output$min_max <- renderText({ 
    paste( c(input$checkGroup))
  })
  
  
  
  
  loc <- reactive({
    
    extractCountryData(input$checkGroup)
    
   })
  
  var <- reactive({input$var})
  
  output$plot <- renderPlot({
    
    
    ggplot() +
      xlim(input$Date[1], input$Date[2]) +
      {if ("Austria" %in% input$checkGroup) geom_line(data = extractCountryData("Austria"), aes(x = as.Date(date, "%Y-%m-%d"), y = eval(as.name(input$var))), color = "cornflowerblue" )} +
      {if ("Belgium" %in% input$checkGroup) geom_line(data = extractCountryData("Belgium"), aes(x = as.Date(date, "%Y-%m-%d"), y = eval(as.name(input$var))), color = "coral3" )} +
      {if ("Bulgaria" %in% input$checkGroup) geom_line(data = extractCountryData("Bulgaria"), aes(x = as.Date(date, "%Y-%m-%d"), y = eval(as.name(input$var))), color = "chartreuse1" )} +
      {if ("Czechia" %in% input$checkGroup) geom_line(data = extractCountryData("Czechia"), aes(x = as.Date(date, "%Y-%m-%d"), y = eval(as.name(input$var))), color = "chartreuse4" )} +
      {if ("Denmark" %in% input$checkGroup) geom_line(data = extractCountryData("Denmark"), aes(x = as.Date(date, "%Y-%m-%d"), y = eval(as.name(input$var))), color = "cadetblue4" )} +
      {if ("Estonia" %in% input$checkGroup) geom_line(data = extractCountryData("Estonia"), aes(x = as.Date(date, "%Y-%m-%d"), y = eval(as.name(input$var))), color = "cadetblue2" )} +
      {if ("Finland" %in% input$checkGroup) geom_line(data = extractCountryData("Finland"), aes(x = as.Date(date, "%Y-%m-%d"), y = eval(as.name(input$var))), color = "brown4" )} +
      {if ("France" %in% input$checkGroup) geom_line(data = extractCountryData("France"), aes(x = as.Date(date, "%Y-%m-%d"), y = eval(as.name(input$var))), color = "blueviolet" )} +
      {if ("Germany" %in% input$checkGroup) geom_line(data = extractCountryData("Germany"), aes(x = as.Date(date, "%Y-%m-%d"), y = eval(as.name(input$var))), color = "blue" )} +
      {if ("Greece" %in% input$checkGroup) geom_line(data = extractCountryData("Greece"), aes(x = as.Date(date, "%Y-%m-%d"), y = eval(as.name(input$var))), color = "green" )} +
      {if ("Hungary" %in% input$checkGroup) geom_line(data = extractCountryData("Hungary"), aes(x = as.Date(date, "%Y-%m-%d"), y = eval(as.name(input$var))), color = "blue" )} +
      {if ("Iceland" %in% input$checkGroup) geom_line(data = extractCountryData("Iceland"), aes(x = as.Date(date, "%Y-%m-%d"), y = eval(as.name(input$var))), color = "green" )} +
      {if ("Ireland" %in% input$checkGroup) geom_line(data = extractCountryData("Ireland"), aes(x = as.Date(date, "%Y-%m-%d"), y = eval(as.name(input$var))), color = "blue" )} +
      {if ("Italy" %in% input$checkGroup) geom_line(data = extractCountryData("Italy"), aes(x = as.Date(date, "%Y-%m-%d"), y = eval(as.name(input$var))), color = "green" )} +
      {if ("Luxembourg" %in% input$checkGroup) geom_line(data = extractCountryData("Luxembourg"), aes(x = as.Date(date, "%Y-%m-%d"), y = eval(as.name(input$var))), color = "blue" )} + 
      {if ("Netherlands" %in% input$checkGroup) geom_line(data = extractCountryData("Netherlands"), aes(x = as.Date(date, "%Y-%m-%d"), y = eval(as.name(input$var))), color = "blue" )} +
      {if ("Norway" %in% input$checkGroup) geom_line(data = extractCountryData("Norway"), aes(x = as.Date(date, "%Y-%m-%d"), y = eval(as.name(input$var))), color = "green" )} +
      {if ("Poland" %in% input$checkGroup) geom_line(data = extractCountryData("Poland"), aes(x = as.Date(date, "%Y-%m-%d"), y = eval(as.name(input$var))), color = "blue" )} +
      {if ("Portugal" %in% input$checkGroup) geom_line(data = extractCountryData("Portugal"), aes(x = as.Date(date, "%Y-%m-%d"), y = eval(as.name(input$var))), color = "green" )} +
      {if ("Slovakia" %in% input$checkGroup) geom_line(data = extractCountryData("Slovakia"), aes(x = as.Date(date, "%Y-%m-%d"), y = eval(as.name(input$var))), color = "blue" )} +
      {if ("Slovenia" %in% input$checkGroup) geom_line(data = extractCountryData("Slovenia"), aes(x = as.Date(date, "%Y-%m-%d"), y = eval(as.name(input$var))), color = "green" )} +
      {if ("Spain" %in% input$checkGroup) geom_line(data = extractCountryData("Spain"), aes(x = as.Date(date, "%Y-%m-%d"), y = eval(as.name(input$var))), color = "blue" )} +
      {if ("Sweden" %in% input$checkGroup) geom_line(data = extractCountryData("Sweden"), aes(x = as.Date(date, "%Y-%m-%d"), y = eval(as.name(input$var))), color = "green" )} +
      {if ("Switzerland" %in% input$checkGroup) geom_line(data = extractCountryData("Switzerland"), aes(x = as.Date(date, "%Y-%m-%d"), y = eval(as.name(input$var))), color = "blue" )} +
      {if ("Ukraine" %in% input$checkGroup) geom_line(data = extractCountryData("Ukraine"), aes(x = as.Date(date, "%Y-%m-%d"), y = eval(as.name(input$var))), color = "green" )} +
      {if ("United Kingdom" %in% input$checkGroup) geom_line(data = extractCountryData("United Kingdom"), aes(x = as.Date(date, "%Y-%m-%d"), y = eval(as.name(input$var))), color = "blue" )} +
     
      xlab("date") +
      switch( input$var,
        new_cases_smoothed_per_million = ylab("cases in million"),
        new_deaths_smoothed_per_million = ylab("deaths in million"),
        new_vaccinations_smoothed_per_million = lab("vacination in million")
      )
    
  

    

  })
  

  
}


# Run the application 
shinyApp(ui = ui, server = server)