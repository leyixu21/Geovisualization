  # ============================================================================
  # Read, analyse and plot Battle-related deaths (number of people) around the world
  # ============================================================================
  
  # ============================================================================
  # UZH, GEO454, Leyi Xu, Miya Yuan, Corrado Muratori: April 2022
  # ============================================================================
  
  ############## 1) CLEAR WORKSPACE ############################################
  
  
  rm(list=ls())
  if (length(dev.list()["RStudioGD"]) > 0) {
    dev.off(dev.list()["RStudioGD"])
  }
  
  ########## 2) LOAD LIBRARIES #####################################################
  
  library(rsconnect)
  library(geojsonR)
  
  
  library(tidyverse)
  library(sf)
  library(rgdal)
  library(dplyr)
  library(magrittr)
  library(rvest)
  library(maps)
  library(ggplot2)
  library(reshape2)
  library(ggiraph)
  library(RColorBrewer)
  library(leaflet)
  library(tidyr)
  
  setwd("~/DEV/GEO/GEO454")
  source("~/DEV/GEO/GEO454/MapFunctions.R")
  
  # #----Corrado Here Setting--------------------------
  # here::i_am("./DEV/GEO/GEO454/GeoMap.R") 
  # 
  # #----General Here Setting--------------------------
  # here::here()
  
  
  ########### 4) READING DATA #######################################################
  
  # ---------------GITHUB with parallel folder for DATA -------------------------
  
  battle_deaths <- na.omit(data.frame(read_csv("../Geo454Data/battle_deaths.csv")))
  mlt_exp <- read_csv("../Geo454Data/military_expenditure.csv")
  country_basemap <- geojsonio::geojson_read("../Geo454Data/map_data/custom.geo.json", 
                                             what = "sp")
  gdp <- data.frame(read_csv("../Geo454Data/GDP.csv"))
  population <- data.frame(read_csv("../Geo454Data/population.csv"))
  geo_event <- read.csv("../Geo454Data/2021_ucdp-ged-211.csv")
  
  
  ###### 5) CLEANING  DATA FRAMES ################################################
  
  # ---------------POPULATION--------------------------------------------
  population[population=='..' ] <- NA
  population <- population[,-c(3:34)]
  population <- population[-c(2,4,8,37,50,62,63,64,65,66,69,74,75,96,99,103,104,105,106,108,129,135,136,137,140,141,143,154,157,162,171,182,184,192,198,199,205,216,218,219,231,232,237,239,241,242,250,260),]
  population[,c(3:33)] = sapply(population[,c(3:33)], as.numeric)
  for (i in 3:33){
    population[,i]=round(as.numeric(population[,i]/1000000), 3) # Convert in Milions
  }
  # Year columns will be renamed later
  
  # ---------------BATTLE DEATHS--------------------------------------------------
  
  battle_deaths[battle_deaths=='..' ] <- 0
  battle_deaths <- battle_deaths[,-c(5:23)]
  battle_deaths <- battle_deaths[,-c(1:2)]
  battle_deaths <- battle_deaths[-c(218:nrow(battle_deaths)),]
  battle_deaths[,c(3:33)] = sapply(battle_deaths[,c(3:33)], as.numeric)
  
  # ---------------BATTLE DEATHS over POPULATION--------------------------------------------------
  # Battle deaths per Million inhabitants
  deathsOnPopulation <- merge.data.frame(battle_deaths, population, by = "Country.Code")
  for (i in 1:31) {
    deathsOnPopulation[,2+i] <- round(deathsOnPopulation[,2+i]/deathsOnPopulation[,2+i+32], digits = 1)
  }
  deathsOnPopulation <- deathsOnPopulation[,-c(34:ncol(deathsOnPopulation))]
  deathsOnPopulation[deathsOnPopulation==Inf ] <- NA
  
  # ---------------GDP (current US$) -------------------------------------------
  
  gdp <- gdp[, -c(3:34)]
  gdp[gdp=='..' ] <- NA
  gdp <- gdp[-c(218:nrow(gdp)),]
  gdp[,c(3:33)] = sapply(gdp[,c(3:33)], as.numeric)
  for (i in 3:33){
    gdp[,i]=round(gdp[,i]/1000000000, 2) # Convert in Bilions
  }
  
  # ---------------MILITAR EXPENDITURE--------------------------------------------
  
  mlt_exp <- mlt_exp[, -c(5:34)]
  mlt_exp <- mlt_exp[-c(435:nrow(mlt_exp)),]
  for (i in 3:33){
    mlt_exp[i][mlt_exp[i] == ".."] <- NA
  }
  
  
  # -----MILITAR EXPENDITURE-----DIVIDE % FROM ABSOLUTE DATA ---------------------
  
  mlt_exp_type <- unique(mlt_exp$"Series Name")
  mlt_exp_percentage <- mlt_exp[(mlt_exp$"Series Name" == mlt_exp_type[2]), ]
  mlt_exp <- mlt_exp[(mlt_exp$"Series Name" == mlt_exp_type[1]), ]
  
  # Military expenditure (% of general government expenditure)
  mlt_exp_percentage <- mlt_exp_percentage[, -c(3:4)]
  mlt_exp_percentage[,c(3:33)] = sapply(mlt_exp_percentage[,c(3:33)], as.numeric)
  
  # Military expenditure (current USD)
  mlt_exp <- mlt_exp[, -c(3:4)]
  mlt_exp[,c(3:33)] = sapply(mlt_exp[,c(3:33)], as.numeric)
  for (i in 3:33){
    mlt_exp[,i]=round(mlt_exp[,i]/1000000000,0) # Convert in Bilions
  }
  mlt_exp_percentage[,c(3:33)] = sapply(mlt_exp_percentage[,c(3:33)], round, digits = 1)
  
  
  #-----Renaming columns of BATTLE DEATHS, GDP & MILITAR EXPENDITURE-------------- 
  
  year <- 1990:2020
  for (i in 1:31) {
    colnames(mlt_exp)[i+2] = year[i]
    colnames(mlt_exp_percentage)[i+2] = year[i]
    colnames(gdp)[i+2] = year[i]
    colnames(battle_deaths)[i+2] = year[i]
    colnames(population)[i+2] = year[i]
    colnames(deathsOnPopulation)[i+2] = year[i]
  }
  #-------------Name for the 3-letter code in MAP DATA-----------------
  colnames(mlt_exp)[2] <-"Country.Code" 
  colnames(mlt_exp_percentage)[2] <-"Country.Code"
  colnames(gdp)[2] <- "Country.Code"
  colnames(population)[2] <- "Country.Code"
  colnames(battle_deaths)[2] <- "Country.Code"
  names(country_basemap@data)[names(country_basemap@data) == "brk_a3"] <- "Country.Code"
  
  #------------- Change Country Name title ---------------------------
  colnames(mlt_exp)[1] <-"Country.Name" 
  colnames(mlt_exp_percentage)[1] <-"Country.Name" 
  colnames(gdp)[1] <- "Country.Name" 
  colnames(population)[1] <- "Country.Name" 
  colnames(battle_deaths)[1] <- "Country.Name" 
  colnames(deathsOnPopulation)[2] <- "Country.Name" 
  
  # -----------------UCPD --------------------------------------------------------
  # Countries that provide weapons for countries in war
  # Click one country, show specific line/bar charts of that country over the time

  geo_event <- geo_event[,c(3,6,9,15,18,26,30,31,34,36,39,40,41,42,43,44)]
  geo_event$n_deaths <- rowSums(geo_event[,13:16])
  # geo_event_sf = st_as_sf(geo_event, coords = c("longitude", "latitude"), 
  #                  crs = 4326)
  
  # filter out one-sided violence
  geo_event <- geo_event %>%
    filter(type_of_violence!=3)
  
  geo_event_changed <- geo_event
  
  geo_event_changed["type_of_violence"][geo_event_changed["type_of_violence"] == 1] <- "state-based conflict"
  geo_event_changed["type_of_violence"][geo_event_changed["type_of_violence"] == 2] <- "non-state conflict"
  
  # -----------------MAP DATA (SP)-----------------------------------------------------
  # Merge sp data with Data Frames
  # "name":"Trinidad and Tobago","brk_a3":"TTO"
  country_basemap@data <- country_basemap@data %>%
    select('Country.Code', 'name')
  
  gdp_sp <- mergeDbWithSp(gdp)
  death_sp <- mergeDbWithSp(battle_deaths)
  mlt_exp_percentage_sp <- mergeDbWithSp(mlt_exp_percentage)
  mlt_exp_sp <- mergeDbWithSp(mlt_exp)
  population_sp <- mergeDbWithSp(population)
  deathsOnPopulation_sp <- mergeDbWithSp(deathsOnPopulation) 
  
  
  ###### 6) CREATE MAPS using leaflet ############################################
  
  #---------Create Longer Data Frames for Interactive Table------------------------------------
  battleDeathsLong <- battle_deaths %>%
    pivot_longer(-c(Country.Name, Country.Code),
                 names_to = "year", 
                 values_to = "n_deaths",
                 values_drop_na = TRUE, 
                 names_transform = list(year = as.integer))
  
  deathsOnPopulationLong <- deathsOnPopulation %>%
    pivot_longer(-c(Country.Name, Country.Code),
                 names_to = "year", 
                 values_to = "deaths_on_the_population",
                 values_drop_na = TRUE, 
                 names_transform = list(year = as.integer))
  
  gdpLong <- gdp %>%
    pivot_longer(-c(Country.Name, Country.Code),
                 names_to = "year", 
                 values_to = "GDP_in_Billions_USD",
                 values_drop_na = TRUE, 
                 names_transform = list(year = as.integer))
  
  mltExpLong <- mlt_exp %>%
    pivot_longer(-c(Country.Name, Country.Code),
                 names_to = "year", 
                 values_to = "Military_Expenditure_in_Billions_USD",
                 values_drop_na = TRUE, 
                 names_transform = list(year = as.integer))
  
  mltExpPercentageLong <- mlt_exp_percentage %>%
    pivot_longer(-c(Country.Name, Country.Code),
                 names_to = "year", 
                 values_to = "Military_Expenditure_On_Total_Expenditure",
                 values_drop_na = TRUE, 
                 names_transform = list(year = as.integer))
  
  populationLong <- population %>%
    pivot_longer(-c(Country.Name, Country.Code),
                 names_to = "year", 
                 values_to = "Population_in_Millions",
                 values_drop_na = TRUE, 
                 names_transform = list(year = as.integer))
  
  ## -------------------------------------------------------- #
  ## 5 Save the data
  ## -------------------------------------------------------- #
  
  save(
    battle_deaths,
    battleDeathsLong,
    country_basemap,
    death_sp, 
    gdp, 
    gdp_sp, 
    gdpLong, 
    geo_event,
    mlt_exp,
    mlt_exp_percentage,
    mlt_exp_percentage_sp,
    mlt_exp_sp,
    mltExpLong,
    mltExpPercentageLong,
    population,
    population_sp,
    populationLong,
    geo_event_changed,
    file = paste0(
      "~/DEV/GEO/GEO454/", "CleanedData",
      ".RData"))
