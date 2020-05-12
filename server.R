library(shiny)
library(shinydashboard)
library(DT)
library(mapproj)
library(readr)
library(tidyverse)
library(scales)
library(lubridate)

shinyServer(function(input, output, session) {
  mygitpath <- 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv'
  df<- reactiveFileReader(1000, session, mygitpath, read_csv)
  observe({updateSelectInput(session, "dates", label = "Select a date:", choices = unique(df()[df()$state=="Florida",]$date))})
  observe({updateSelectInput(session, "counties", label = "Select counties:", choices = unique(df()[df()$state=="Florida",]$county))})

  output$mytable <-DT::renderDataTable({
    df<- df()
    df<- df %>% filter(state == "Florida")
    colnames(df)<- c("Date", "County", "State", "Fips", "Cases", "Deaths")
    if (input$mapOptions == "deaths"){
      df<- df %>% filter(Date == input$dates) %>% select(County, Date, Deaths) %>% arrange(desc(Deaths)) %>% datatable(options = list(pageLength = 20)) %>% formatRound(columns = 'Deaths', digits = 0)
      }
    if (input$mapOptions == "cases"){
      df<- df %>% filter(Date == input$dates) %>% select(County, Date, Cases) %>% arrange(desc(Cases)) %>% datatable(options = list(pageLength = 20)) %>% formatRound(columns = 'Cases', digits = 0)
      }
    return(df)
    })
  output$mymap <- renderPlot({
    df<- df()
    df<- df %>% filter(state == "Florida")
    colnames(df)<- c("Date", "County", "State", "Fips", "Cases", "Deaths")
    for (i in 1:nrow(df)){
      df[i,2]<- tolower(df[i,2])
    }
    if (input$mapOptions == "deaths"){
      df<- df %>% filter(is.na(County) == FALSE & Date == input$dates)
      counties<- df
      counties<- counties %>% filter(County != "unknown")
      counties[counties$County == "dade",][2]<- "miami-dade"
      counties[counties$County == "st. johns",][2]<- "st johns"
      counties[counties$County == "st. lucie",][2]<- "st lucie"
      counties[counties$County == "desoto",][2]<- "de soto"
      florida_data <- map_data("county") %>% filter(region == "florida")
      map_data<- florida_data %>% left_join(counties, by = c("subregion" = "County"))
      map_data$Cases[is.na(map_data$Cases)]<- 0
      map_data$Deaths[is.na(map_data$Deaths)]<- 0
      
      p<- ggplot(map_data, aes(x = long, y = lat, group = group, fill = Deaths)) + 
        geom_polygon(color = "black", size = 0.5) + scale_fill_gradient(low = "lightsalmon", high = "midnightblue") + 
        labs(fill = "Deaths", x = "Longitude", y = "Latitude") + 
        coord_map(projection = "albers", lat0 = 25, lat1 = 31) + theme_minimal()
    }
    if (input$mapOptions == "cases"){
      df<- df %>% filter(is.na(County) == FALSE & Date == input$dates)
      counties<- df
      counties<- counties %>% filter(County != "unknown")
      counties[counties$County == "dade",][2]<- "miami-dade"
      counties[counties$County == "st. johns",][2]<- "st johns"
      counties[counties$County == "st. lucie",][2]<- "st lucie"
      counties[counties$County == "desoto",][2]<- "de soto"
      florida_data <- map_data("county") %>% filter(region == "florida")
      map_data<- florida_data %>% left_join(counties, by = c("subregion" = "County"))
      map_data$Cases[is.na(map_data$Cases)]<- 0
      map_data$Deaths[is.na(map_data$Deaths)]<- 0
      
      p<- ggplot(map_data, aes(x = long, y = lat, group = group, fill = Cases)) + 
        geom_polygon(color = "black", size = 0.5) + scale_fill_gradient(low = "lightsalmon", high = "midnightblue") + 
        labs(fill = "Cases", x = "Longitude", y = "Latitude") + 
        coord_map(projection = "albers", lat0 = 25, lat1 = 31) + theme_minimal()
    }
      return(p)
    })
  output$mytimeseries<- renderPlot({
    df<- df()
    df<- df %>% filter(state == "Florida")
    colnames(df)<- c("Date", "County", "State", "Fips", "Cases", "Deaths")
    if (input$plotOptions == "florida"){
      florida_df<- df %>% group_by(Date) %>% summarise(Cases = sum(Cases))
      p<- florida_df %>% ggplot(aes(x = Date, y = Cases)) + geom_line(aes(colour = "red")) + geom_point(aes(colour = "red")) +
        theme_classic() + theme(legend.position = "none") +  scale_x_date(labels = date_format("%b-%d"), breaks='6 days') +
        labs(title = "COVID-19 Cases in Florida Since March")
      if (input$model == TRUE){
        df_model<- florida_df %>% mutate(log_Cases = log(Cases))
        model_fit<- lm(log_Cases~Date, data = df_model)
        florida_df<- florida_df %>% mutate(Predictions = exp(model_fit$fitted.values))
        florida_df<- florida_df %>% gather(key = "Type",value = "Cases", Cases:Predictions)
        x<- data.frame("Date" = rep(c(as.Date("2020-04-10"), as.Date("2020-04-11"), as.Date("2020-04-12"), as.Date("2020-04-13"), as.Date("2020-04-14")), 2))
        for (i in 1:nrow(x)){
          x[i,1]<- today() + (i-1)
        }
        y<- exp(predict(model_fit, x, interval = "c"))
        n<- rep(c("Predictions"), 10)
        zeros<- rep(0, 10)
        pred_df<- cbind(x, n, as.data.frame(y)$fit, zeros, as.data.frame(y)$upr, as.data.frame(y)$lwr)
        colnames(pred_df)<- c("Date", "Type", "Cases", "log_Cases", "Upper", "Lower")
        pred_df_without_bounds<- pred_df %>% select(Date, Type, Cases)
        florida_df<- rbind(florida_df, pred_df_without_bounds)
        p<- florida_df %>% ggplot(aes(x = Date, y = Cases)) + geom_line(aes(colour = Type)) + geom_point(aes(colour = Type)) +
          geom_smooth(aes(ymin = Lower, ymax = Upper, colour = Type), data = pred_df, stat = 'identity') +
          theme_classic() + scale_x_date(labels = date_format("%b-%d"), breaks='6 days') +
          labs(title = "COVID-19 Cases in Florida Since March", subtitle = "Fitted versus Actual (with Confidence Bands)", y = "Cases")
      }
    }
    if (input$plotOptions == "county") {
      x<- input$counties
      county_df<- df %>% filter(County %in% x) %>% group_by(County, Date) %>% summarise(Cases = sum(Cases))
      p<- county_df %>% ggplot(aes(x = Date, y = Cases)) + geom_line(aes(colour = County)) + geom_point(aes(colour = County)) + 
        theme_classic() +  scale_x_date(labels = date_format("%b-%d"), breaks='6 days') + labs(title = "COVID-19 Cases by County Since March")
    }
    return(p)
  })
  })
