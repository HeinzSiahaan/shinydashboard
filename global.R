# import library
library(dplyr)
library(ggplot2)
library(scales)
library(glue)
library(plotly)
library(lubridate)
library(DT)
library(stringr)
library(tidyverse)
options(scipen = 100)

library(shiny)
library(shinyWidgets)
library(shinydashboard)

# read data
games.sales <- read.csv("data_input/salesgamesdata.csv", stringsAsFactors = FALSE, encoding = "latin1")

# cleansing data 
match.row <- match(unique(games.sales$Name), games.sales$Name)
games.sales.clean <- games.sales[match.row, ]

games.sales.clean<-games.sales%>% 
  filter(Genre!="",Year_of_Release!="N/A",Global_Sales > 0.1)%>%
  mutate(
    Name = as.character(Name),
    Platform = as.factor(Platform),
    Genre = as.factor(Genre),
    Publisher = as.factor(Publisher),
    Year_of_Release = as.numeric(Year_of_Release)
  )

games.sales.clean=select(.data = games.sales.clean, -Critic_Score, -Critic_Count, -User_Score,-User_Count,-Developer,-Rating)

games.sales.clean.option=select(.data = games.sales.clean, Platform,Genre,Publisher,Year_of_Release)
games.sales.clean.option <- games.sales.clean %>%
  mutate( 
    Platform = as.character(Platform),
    Genre = as.character(Genre),
    Publisher = as.character(Publisher),
    Year_of_Release = as.numeric(Year_of_Release)
  ) 

