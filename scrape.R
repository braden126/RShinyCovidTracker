library(rvest)
library(tidyverse)
library(stringr)

datalink <- read_html("https://www.worldometers.info/coronavirus/country/us/")

datahtmlnodes <- html_nodes(datalink,'#usa_table_countries_today')

data <- html_table(datahtmlnodes, fill=TRUE)[[1]]

data <- data %>%
  select(-Source, -Projections) %>%       #remove unneeded columns 
  mutate_all(na_if,"") %>%                #turn blanks into NA values
  filter(!str_detect(USAState, 'Total|Guam|Northern Mariana|Puerto|Virgin Islands|Veteran|Military|Prisons|Navajo|Ship|Wuhan|American Samoa'))  #remove the "total" row and keep only 50 States and District of Colombia

#str(data)

data <- as.data.frame(lapply(data, function(y) gsub(",", "", y)))    #removes commas in numeric values from all columns
data <- as.data.frame(lapply(data, function(y) gsub("\\+", "", y)))  #removes plus sign from all columns

#changes the datatypes from character to numeric 
data$TotalCases <- as.numeric(data$TotalCases)
data$NewCases <- as.numeric(data$NewCases)
data$TotalDeaths <- as.numeric(data$TotalDeaths)
data$NewDeaths <- as.numeric(data$NewDeaths)
data$ActiveCases <- as.numeric(data$ActiveCases)
data$Tot.Cases.1M.pop <- as.numeric(data$Tot.Cases.1M.pop)
data$Deaths.1M.pop <- as.numeric(data$Deaths.1M.pop)
data$TotalTests <- as.numeric(data$TotalTests)
data$Tests.1M.pop <- as.numeric(data$Tests.1M.pop)

#str(data)
