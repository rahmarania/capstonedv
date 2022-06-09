library(ggplot2)
library(scales)
library(glue)
library(plotly)
library(dplyr)
library(tidyr)
library(lubridate)
library(shiny)
library(shinydashboard)

anime <- read.csv('tidy_anime.csv', stringsAsFactors = FALSE)

anime <- anime[, -c(3,4,5,13,24,25,28)]

anime$animeID <- as.character(anime$animeID)
anime$type <- as.factor(anime$type)
anime$genre <- as.factor(anime$genre)

anime$start_date <- ymd(anime$start_date)
anime$end_date <- ymd(anime$end_date)

anime <- anime %>% 
  separate(premiered, c("prem_season", "prem_year"))

anime$prem_season <- as.factor(anime$prem_season)
anime$prem_year <- as.numeric(anime$prem_year)

anime$prem_Season <- ifelse(as.numeric(format.Date(anime$start_date, "%m")) %in% c(12,1,2), "Winter",
                            ifelse(as.numeric(format.Date(anime$start_date, "%m")) %in% c(3,4,5), "Spring",
                                   ifelse(as.numeric(format.Date(anime$start_date, "%m")) %in% c(6,7,8), "Summer",
                                          ifelse(as.numeric(format.Date(anime$start_date, "%m")) %in% c(9,10,11), "Fall",
                                                 no = NA )))) 

unique_anime <- data.frame(anime %>% distinct(animeID, .keep_all = TRUE))