library(shiny)
library(shinydashboard)
library(DT)

dashboardPage(
  dashboardHeader(title = "Florida's COVID-19"),
  dashboardSidebar(sidebarMenu(id = "sidebar",
    menuItem("Map", tabName = "map", icon = icon("map")), 
    menuItem("Plot", tabName = "plot", icon = icon("bar-chart-o")),
    conditionalPanel(condition ="input.sidebar == 'map'", selectInput("dates", "Select a date:", choices=c(""))),
    conditionalPanel(condition ="input.sidebar == 'map'", radioButtons("mapOptions", "Select an option:", c("Cases by county"= "cases", "Deaths by county"= "deaths"))),
    conditionalPanel(condition ="input.sidebar == 'plot'", radioButtons("plotOptions", "Select an option:", c("All Florida cases"= "florida", "Cases by counties"= "county"))),
    conditionalPanel(condition = "input.sidebar == 'plot' && input.plotOptions == 'florida'", checkboxInput("model", "Add fitted model", FALSE)),
    conditionalPanel(condition ="input.sidebar == 'plot' && input.plotOptions == 'county'", selectInput("counties", "Select counties:", choices=c(""), multiple = TRUE), 
                     radioButtons("demographic", "Select a demographic view:", choices = c("Age" = "age", "Ethnicity" = "eth")))
  )),
  dashboardBody(tabItems(
    tabItem(tabName = "map",
      fluidRow(box(width=8, status="primary", title= "Map of Counties", solidHeader = TRUE, plotOutput("mymap")),
         box(width=4, status="primary", title = "Table of Counties", solidHeader = TRUE, collapsible = TRUE, DT::dataTableOutput("mytable")))),
    tabItem(tabName = "plot",
      fluidRow(box(width=10, status="primary", title= "Cases Over Time", solidHeader = TRUE, plotOutput("mytimeseries"))),
      conditionalPanel(condition ="input.sidebar == 'plot' && input.plotOptions == 'county'",
                       fluidRow(box(width = 10, status = "primary", title = "Demographics by County", solidHeader = TRUE, plotOutput("countiesinfo"))))
    ))
))

