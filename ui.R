library(shiny)
library(shinydashboard)
library(DT)
library(shinyWidgets)
library(plotly)
library(ggplot2)

js <- '.nav-tabs-custom .nav-tabs li.active {
    border-top-color: #d73925;
}"'

dashboardPage(skin="red",
  dashboardHeader(title = "Florida's COVID-19"),
    dashboardSidebar(disable=TRUE),
  dashboardBody(
      tags$style(js),
      setSliderColor("Red",1),
    
    #Info boxes
    fluidRow(box(width = 4, valueBoxOutput("info1", tags$style("#dri {width:200px;}"))),
             box(width = 4, valueBoxOutput("info2", tags$style("#dri {width:200px;}"))),
             box(width = 4, valueBoxOutput("info3", tags$style("#dri {width:200px;}")))),
    
    
    #Large Tab Box 
    fluidRow(
      tabBox(
        title = NULL, width = 12,
        # The id lets us use input$tabset1 on the server to find the current tab
        id = "tabset1", height = "250px",
        
        
        #Map Tab
        tabPanel("Map", tabName = "map", icon = icon("map"), 
                 
                 #Maps
                 fluidRow(box(width=6, status="danger", title= "Map of Cases by Counties",solidHeader = TRUE, plotlyOutput("mymapC")),
                          box(width=6, status="danger", title= "Map of Deaths by Counties",solidHeader = TRUE, plotlyOutput("mymapD"))), 
                 
                 #Slider Input 
                 fluidRow(column(12,align="center", sliderInput("dates", "Select a date:", min=0, max=0,value = NULL)))),
        
        
        #Counties Tab
        tabPanel("Counties", tabName = "counties", icon = icon("users", lib = "font-awesome"), 
                 
                 #Plots 
                 fluidRow(box(width=6, status="danger", title= "Cases Over Time by County",solidHeader = TRUE, plotOutput("mytimeseries")),
                                                                                                        
                          box(width=6, status="danger", title= "Demographics by County",solidHeader = TRUE, plotOutput("countiesinfo"))),
                 
                 #Plot Inputs
                 fluidRow(column(6, align="center", selectInput("counties", "Select counties:", choices=c(""), multiple = TRUE)),
                          
                          column(6, align="center", radioButtons("demographic", "Select a demographic view:", choices = c("Age" = "age", "Ethnicity" = "eth"), inline=TRUE)))),
        
        
        #State Tab
        tabPanel("Florida", tabName = "florida", icon = icon("flag", lib = "font-awesome"),
                 
                 #Plot
                 fluidRow(column(3),box(width=6,status="danger", title= "Florida Cases",solidHeader = TRUE, plotOutput("mytimeseries2"))),
                 
                 #Plot Input
                 fluidRow(column(12, align="center", checkboxInput("model", "Add fitted model", FALSE)))
              )
        )
    )
    ))


