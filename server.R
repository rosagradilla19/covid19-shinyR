library(shiny)
library(shinydashboard)
library(DT)
library(mapproj)
library(readr)
library(readxl)
library(data.table)
library(tidyverse)
library(scales)
library(lubridate)
library(ggplot2)
library(splines)

shinyServer(function(input, output, session) {
  
  #csv reads
  mygitpath <- 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv'
  df<- reactiveFileReader(1000, session, mygitpath, read_csv)
  clean_dem<- read_excel("Data/Demographics.xls")
  clean_eth<- read_excel("Data/Ethnicity.xlsx")

  #Info Box 1
  output$info1<- renderValueBox({
    today_date<- today() -1
    df<- df()
    df<- df %>% filter(state == "Florida" & date == today_date)
    colnames(df)<- c("Date", "County", "State", "Fips", "Cases", "Deaths")
    total_cases<- sum(df$Cases)
    valueBox(total_cases, subtitle = paste("Total cases as of ", today()-1, sep = ""), color = "green", icon = icon("warning"))
  })
  
  #Info Box 2
  output$info2<- renderValueBox({
    today_date<- today() -1
    df<- df()
    df<- df %>% filter(state == "Florida" & date == today_date)
    colnames(df)<- c("Date", "County", "State", "Fips", "Cases", "Deaths")
    county_df<- df %>% left_join(select(pop_table, c("County", "Total")), by = "County") %>% group_by(County, Date) %>% 
      mutate("Cases per Capita" = sum(Cases)/Total) %>% arrange(desc(Cases))
    top_cases<- county_df[c(1:4),]
    valueBox(top_cases[1,5], subtitle = paste("County with the most cases: ", top_cases[1,2], sep = ""), color = "purple", icon = icon("line-chart"))
  })
  
  #Info Box 3
  output$info3<- renderValueBox({
    today_date<- today() -1
    df<- df()
    df<- df %>% filter(state == "Florida" & date == today_date)
    colnames(df)<- c("Date", "County", "State", "Fips", "Cases", "Deaths")
    county_df<- df %>% left_join(select(pop_table, c("County", "Total")), by = "County") %>% group_by(County, Date) %>% 
      mutate("Cases per Capita" = sum(Cases)/Total) %>% arrange(desc(Cases))
    top_cases<- county_df[c(1:4),]
    valueBox(sprintf("%0.3f", round(top_cases[1,8], digits = 3)), subtitle = paste("Cases per capita in ", top_cases[1,2], sep = ""), color = "blue", icon = icon("bar-chart-o"))
  })
  
  #Cleaning Data
  data_long <- gather(clean_dem, Age, Count, `Under 5 years`:`85 years and over`, factor_key=TRUE)
  pop_table<- data_long %>% group_by(County) %>% summarise(Total = sum(Total_Pop))  
  data_long<- data_long %>% left_join(select(pop_table, c("County", "Total")), by = "County")
  temp_age<- data_long %>% group_by(County, Age) %>% summarise(count = sum(Count))
  new_age<- NULL
  for (i in seq(1, nrow(temp_age), by = 3)) {
    j = i + 1
    k = i + 2
    new_total<- data.frame("Count" = sum(temp_age[i,3] + temp_age[j,3] + temp_age[k,3]))
    county<- data.frame("County" = temp_age[i,1])
    new_row<- cbind(county, new_total)
    new_age<- rbind(new_age, new_row)
    new_age %>% left_join(select(data_long, c("County", "Total")), by = "County")
  }
  Age<- rep(c("0-14", "15-29", "30-44", "45-59", "60-74", "75+"), 67)
  new_age<- data.frame(cbind(new_age, Age))
  pop_table<- new_age %>% group_by(County) %>% summarise(Total = sum(Count))
  new_age<- new_age %>% left_join(select(pop_table, c("County", "Total")), by = "County")
  
  
  #update of slider inputs and county selections
  observe({updateSliderInput(session, "dates", label = "Select a date:", min(df()[df()$state=="Florida",]$date), max(df()[df()$state=="Florida",]$date), value=max(df()[df()$state=="Florida",]$date))})
  observe({updateSelectInput(session, "counties", label = "Select counties:", choices = unique(df()[df()$state=="Florida",]$county))})

  
#Map of Deaths
  output$mymapD <- renderPlotly({
    df<- df()
    df<- df %>% filter(state == "Florida")
    colnames(df)<- c("Date", "County", "State", "Fips", "Cases", "Deaths")
    maxcases <- max(df$Deaths)
    for (i in 1:nrow(df)){
      df[i,2]<- tolower(df[i,2])
    }
 
    df$Date <- lapply(df$Date, as.Date)
    df<- df %>% filter(is.na(County) == FALSE & Date == input$dates)
    counties<- df
    counties<- counties %>% filter(County != "unknown")
    counties[counties$County == "dade",2]<- "miami-dade"
    counties[counties$County == "st. johns",2]<- "st johns"
    counties[counties$County == "st. lucie",2]<- "st lucie"
    counties[counties$County == "desoto",2]<- "de soto"
    florida_data <- map_data("county") %>% filter(region == "florida")
    map_data<- florida_data %>% left_join(counties, by = c("subregion" = "County"))
    map_data$Cases[is.na(map_data$Cases)]<- 0
    map_data$Deaths[is.na(map_data$Deaths)]<- 0
    p<- ggplot(map_data, aes(x = long, y = lat, group = group, fill = Deaths, text=paste('County: ', subregion) )) +
      geom_polygon(color = "black", size = 0.5) +
      scale_fill_gradient(low = "lightsalmon", high = "midnightblue", limits=c(0, maxcases) ) +
      labs(fill = "Deaths", x = "Longitude", y = "Latitude") +
      coord_map(projection = "albers", lat0 = 25, lat1 = 31) + theme_minimal()
    return(ggplotly(p))
  })
  
#Map of Cases 
  output$mymapC <- renderPlotly({
    df<- df()
    df<- df %>% filter(state == "Florida")
    colnames(df)<- c("Date", "County", "State", "Fips", "Cases", "Deaths")
    maxcases <- max(df$Cases)
    for (i in 1:nrow(df)){
      df[i,2]<- tolower(df[i,2])
    }
    
    df$Date <- lapply(df$Date, as.Date)
    df<- df %>% filter(is.na(County) == FALSE & Date == input$dates)
    counties<- df
    counties<- counties %>% filter(County != "unknown")
    counties[counties$County == "dade",2]<- "miami-dade"
    counties[counties$County == "st. johns",2]<- "st johns"
    counties[counties$County == "st. lucie",2]<- "st lucie"
    counties[counties$County == "desoto",2]<- "de soto"
    florida_data <- map_data("county") %>% filter(region == "florida")
    map_data<- florida_data %>% left_join(counties, by = c("subregion" = "County"))
    map_data$Cases[is.na(map_data$Cases)]<- 0
    map_data$Deaths[is.na(map_data$Deaths)]<- 0
    p<- ggplot(map_data, aes(x = long, y = lat, group = group, fill = Cases, text=paste('County: ', subregion))) +
      geom_polygon(color = "black", size = 0.5) + scale_fill_gradient(low = "lightsalmon", high = "midnightblue", limits=c(0, maxcases)) +
      labs(fill = "Cases", x = "Longitude", y = "Latitude") +
      coord_map(projection = "albers", lat0 = 25, lat1 = 31) + theme_minimal()
    
    return(ggplotly(p))
  })
  
  
  
#Plot by county  
  output$mytimeseries <- renderPlot({
    df<- df()
    df<- df %>% filter(state == "Florida")
    colnames(df)<- c("Date", "County", "State", "Fips", "Cases", "Deaths")
    x<- input$counties
    county_df<- df %>% filter(County %in% x) 
    county_df<- county_df %>% left_join(select(pop_table, c("County", "Total")), by = "County") %>% group_by(County, Date) %>% summarise(Cases = sum(Cases)/Total)
    p<- county_df %>% ggplot(aes(x = Date, y = Cases)) + geom_line(aes(colour = County)) + geom_point(aes(colour = County)) + 
      theme_classic() +  scale_x_date(labels = date_format("%b-%d"), breaks='6 days') + labs(title = "COVID-19 Cases by County Since March", y = "Cases per Capita")
    return(p)
  })
  
  
#Florida plot with fitted splines
  output$mytimeseries2<- renderPlot({
    df<- df()
    df<- df %>% filter(state == "Florida")
    colnames(df)<- c("Date", "County", "State", "Fips", "Cases", "Deaths")
    florida_df<- df %>% group_by(Date) %>% summarise(Cases = sum(Cases))
    p<- florida_df %>% ggplot(aes(x = Date, y = Cases)) + geom_line(aes(colour = "red")) + geom_point(aes(colour = "red")) +
      theme_classic() + theme(legend.position = "none") +  scale_x_date(labels = date_format("%b-%d"), breaks='6 days') +
      labs(title = "COVID-19 Cases in Florida Since March")
    if (input$model == TRUE){
      df_model<- florida_df %>% mutate(log_Cases = log(Cases))
      model_fit<- lm(Cases ~ bs(Date), data = df_model)
      #model_fit<- lm(log_Cases~Date, data = df_model)
      florida_df<- florida_df %>% mutate(Predictions = model_fit$fitted.values)
      florida_df<- florida_df %>% gather(key = "Type",value = "Cases", Cases:Predictions)
      x<- data.frame("Date" = rep(c(as.Date("2020-04-10"), as.Date("2020-04-11"), as.Date("2020-04-12"), as.Date("2020-04-13"), as.Date("2020-04-14"))))
      for (i in 1:nrow(x)){
        x[i,1]<- today() + (i-1)
      }
    
      y<- predict(model_fit, x, interval = "c")
      n<- rep(c("Predictions"), 5)
      zeros<- rep(0, 5)
      pred_df<- cbind(x, n, as.data.frame(y)$fit, zeros, as.data.frame(y)$upr, as.data.frame(y)$lwr)
      colnames(pred_df)<- c("Date", "Type", "Cases", "log_Cases", "Upper", "Lower")
      pred_df_without_bounds<- pred_df %>% select(Date, Type, Cases)
      florida_df<- rbind(florida_df, pred_df_without_bounds)
      
      p<- florida_df %>% ggplot(aes(x = Date, y = Cases)) + geom_line(aes(colour = Type)) + geom_point(aes(colour = Type)) +
        geom_smooth(aes(ymin = Lower, ymax = Upper, colour = Type), data = pred_df, stat = 'identity') +
        theme_classic() + scale_x_date(labels = date_format("%b-%d"), breaks='6 days') +
        labs(title = "COVID-19 Cases in Florida Since March", y = "Cases")
    }
    
    return(p)
  })
  
  
  
#County Plot of Age or Demographics
  output$countiesinfo<- renderPlot({
    if (input$demographic == "age") {
      x<- input$counties
      q<- new_age %>% filter(County %in% x) %>% mutate(Perc = (Count/Total)*100) %>% ggplot(aes(x = Age, y = Perc, fill = County)) + geom_bar(stat = 'identity', position = 'dodge') +
        theme_classic() + scale_y_continuous(expand = c(0, 0)) + labs(title = "Percentage of County Population by Age", y = "Percentage")
      
    }
    if (input$demographic == "eth") {
      x<- input$counties
      clean_eth<- clean_eth %>% filter(State == "Florida" & `2018` == "Yes") %>% left_join(pop_table, by = "County")
      colnames(clean_eth)[14]<- "Total"
      colnames(clean_eth)[7]<- "Eth Total"
      clean_eth<- clean_eth %>% filter(`Hispanic Origin` != "Total") %>% filter(grepl(paste(x, collapse="|"), County)) %>% 
        mutate(Perc = (`Eth Total`/Total)*100)
      q<- clean_eth %>% ggplot(aes(x = County, y = Perc, fill = `Hispanic Origin`)) + geom_bar(stat = 'identity', position = 'dodge') +
        theme_classic() + theme(axis.line=element_blank(), axis.ticks.y = element_blank(), axis.text.y=element_blank(), axis.title.y=element_blank())
    }
    return(q)
  })
})