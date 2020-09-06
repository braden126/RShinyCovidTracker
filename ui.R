library(shiny)
library(shinydashboard)
library(tidyverse)
library(rvest)
library(tidyverse)
library(stringr)
library(scales)

#gathers data once for the state list input and removes extra rows
datalink <- read_html("https://www.worldometers.info/coronavirus/country/us/")
datahtmlnodes <- html_nodes(datalink,'#usa_table_countries_today')

data1 <- html_table(datahtmlnodes, fill=TRUE)[[1]] %>%
  select(USAState)%>%
  filter(!str_detect(USAState, 'Total|Guam|Northern Mariana|Puerto|Virgin Islands|Veteran|Military|Prisons|Navajo|Ship|Wuhan'))  #remove the "total" row and keep only 50 States and District of Colombia


shinyUI(
  
  dashboardPage(
    dashboardHeader(title = "Covid Tracker"),
    dashboardSidebar(),
    dashboardBody(
      textOutput("text1"),
        fluidRow(
          valueBoxOutput("vb1", width = 12)
        ),
        fluidRow(
          valueBoxOutput("vb3", width = 4),
          valueBoxOutput("vb4", width = 4),
          valueBoxOutput("vb5", width = 4)
        ),
        box(
          width = 12,
          title = "Individual State and USA Average",
          selectInput("stateinput", "Choose a state:",
                      sort(data1$USAState)),
          fluidRow(
            column(4,plotOutput(outputId="plot1")),  
            column(4,plotOutput(outputId="plot2")),
            column(4,plotOutput(outputId="plot3"))
          )
        )
      )
    )
  )
