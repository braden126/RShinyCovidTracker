library(shiny)
library(shinydashboard)
library(tidyverse)
library(rvest)
library(tidyverse)
library(stringr)
library(scales)

shinyServer(function(input,output){

  
    observe({
      invalidateLater(60000) #completes the webscrape every minute 
      
      datalink <- read_html("https://www.worldometers.info/coronavirus/country/us/")
      datahtmlnodes <- html_nodes(datalink,'#usa_table_countries_today')
      
      data1 <- html_table(datahtmlnodes)[[1]]
      
      
      data1 <- data1 %>%
        select(-Source, -Projections) %>%       #remove unneeded columns 
        mutate_all(na_if,"") %>%                #turn blanks into NA values
        filter(!str_detect(USAState, 'Total|Guam|Northern Mariana|Puerto|Virgin Islands|Veteran|Military|Prisons|Navajo|Ship|Wuhan'))  #remove the "total" row and keep only 50 States and District of Colombia
      
      data1 <- as.data.frame(lapply(data1, function(y) gsub(",", "", y)))    #removes commas in numeric values from all columns
      data1 <- as.data.frame(lapply(data1, function(y) gsub("\\+", "", y)))  #removes plus sign from all columns
      
      #change datatypes from character to numeric 
      data1$TotalCases <- as.numeric(data1$TotalCases)
      data1$NewCases <- as.numeric(data1$NewCases)
      data1$TotalDeaths <- as.numeric(data1$TotalDeaths)
      data1$NewDeaths <- as.numeric(data1$NewDeaths)
      data1$ActiveCases <- as.numeric(data1$ActiveCases)
      data1$Tot.Cases.1M.pop <- as.numeric(data1$Tot.Cases.1M.pop)
      data1$Deaths.1M.pop <- as.numeric(data1$Deaths.1M.pop)
      data1$TotalTests <- as.numeric(data1$TotalTests)
      data1$Tests.1M.pop <- as.numeric(data1$Tests.1M.pop)

      #captures time to see when last update was
      updatetime <- Sys.time()
      
      usatot <- sum(data1$TotalCases)
      
      output$text1 <- renderText({
        paste("Last Update at", updatetime) 
      })
      
      #Total Cases Valuebox
      output$vb1 <- renderValueBox({
        valueBox(usatot, "Total Cases in the United States", icon = NULL, color = "light-blue", width = 6,
                 href = NULL)
      })
      
      
      #Finds state with highest deaths reported and checks to make sure data has been reported at the time of the webscrape
      Newdeaths <- data1 %>%
        slice(which.max(NewDeaths)) 
      
      if(length(Newdeaths$NewDeaths) == 0){
        deathsresult <- paste("No data Reported")
      } else if(is.na(Newdeaths$NewDeaths)){
        deathsresult <- paste("No data Reported")
      } else{
        deathsresult <- paste(Newdeaths$USAState, ":", Newdeaths$NewDeaths)
      }
      
      
      output$vb3 <- renderValueBox({
        valueBox(deathsresult, "State With Highest New Deaths Today", icon = NULL, color = "light-blue", width = 12,
                 href = NULL)
      })
      
      #Finds state with highest cases reported and checks to make sure data has been reported at the time of the webscrape
      Newcases <- data1 %>%
        slice(which.max(NewCases)) 
      
      
      if(length(Newcases$NewCases) == 0){
        casesresult <- paste("No data Reported")
      } else if(is.na(Newcases$NewCases)){
        casesresult <- paste("No data Reported")
      } else{
        casesresult <- paste(Newcases$USAState, ":", Newcases$NewCases)
      }
      
      output$vb4 <- renderValueBox({
        valueBox(casesresult, "State With Hightest New Cases Today", icon = NULL, color = "light-blue", width = 12,
                 href = NULL)
      })
      
      #Finds number of cases reported
      #removes states that have not reported yet and checks to make sure at least one state has reported at the time of the webscrape
      totalnewcases <- na.omit(data1$NewCases)
      totalnewcases <- sum(totalnewcases)
      
      totalnewcases
      if(length(totalnewcases) == 0){
        totalnewcases <- paste("No data Reported")
      } else if(is.na(totalnewcases)){
        totalnewcases <- paste("No data Reported")
      } else if(totalnewcases == 0){
        totalnewcases <- paste("No data Reported")
      } 
      
      output$vb5 <- renderValueBox({
        valueBox(totalnewcases, "Total New Cases Reported in the United States Today", icon = NULL, color = "light-blue", width = 12,
                 href = NULL)
      })
      
      #selects the three metrics in a reactive so they can be interactive with inputs
      selectedstate <- reactive({ 
        selectedstate <- data1 %>%
          filter(USAState == input$stateinput) %>%
          select(USAState, Tot.Cases.1M.pop, Tests.1M.pop, Deaths.1M.pop)
      })
      
        stateaverages <- data1 %>%
          summarise(Tot.Cases.1M.pop=round(mean(Tot.Cases.1M.pop),0), Tests.1M.pop = round(mean(Tests.1M.pop),0), Deaths.1M.pop = round(mean(Deaths.1M.pop),0))
        stateaverages$USAState <- 'USAAverage'
      
      #cases per million ggplot
      output$plot1 <- renderPlot({
        plot1data <- rbind(stateaverages, selectedstate())
        ggplot(plot1data, aes(x = USAState, y = Tot.Cases.1M.pop, fill = USAState)) + geom_col() + scale_y_continuous(labels = comma) + geom_text(aes(label=Tot.Cases.1M.pop), vjust=-0.6) + 
          labs(x = "State", y = "Cases Per Million Population", title = "Cases Per Million Population") + theme(legend.position="none")
      })

      #Tests per million ggplot     
      output$plot2 <- renderPlot({
        plot1data <- rbind(stateaverages, selectedstate())
        ggplot(plot1data, aes(x = USAState, y = Tests.1M.pop, fill = USAState)) + geom_col() + scale_y_continuous(labels = comma) + geom_text(aes(label=Tests.1M.pop), vjust=-0.6) + 
          labs(x = "State", y = "Tests Per Million Population", title = "Tests Per Million Population") + theme(legend.position="none")
      })
      
      #cases Deaths million ggplot
      output$plot3 <- renderPlot({
        plot1data <- rbind(stateaverages, selectedstate())
        ggplot(plot1data, aes(x = USAState, y = Deaths.1M.pop, fill = USAState)) + geom_col() + scale_y_continuous(labels = comma) + geom_text(aes(label=Deaths.1M.pop), vjust=-0.6) + 
          labs(x = "State", y = "Deaths Per Million Population", title = "Deaths Per Million Population") + theme(legend.position="none")
      })
   }) 
})