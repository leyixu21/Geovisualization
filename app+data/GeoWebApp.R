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


########## 3) LOAD LIBRARIES #####################################################

library(shiny)
library(rsconnect)
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(profvis)
library(here)

# library(tidyverse)
# library(sf)
# library(maps)
# library(ggplot2)
# library(reshape2)
# library(ggiraph)
# library(RColorBrewer)
# library(tidyr)


# setwd("~/DEV/GEO/GEO454")
# source("~/DEV/GEO/GEO454/MapFunctions.R")
# load("~/DEV/GEO/GEO454/CleanedData.RData")

######### 4) LOAD FILES ######################################################
here::here()


battle_deaths <- readRDS('data/battle_deaths.rds')
battleDeathsLong <- readRDS('data/battleDeathsLong.rds')
country_basemap <- readRDS('data/country_basemap.rds')
death_sp <- readRDS('data/death_sp.rds')
gdp <- readRDS('data/gdp.rds')
gdp_sp <- readRDS('data/gdp_sp.rds')
gdpLong <- readRDS('data/gdpLong.rds')
geo_event <- readRDS('data/geo_event.rds')
geo_event_changed <- readRDS('data/geo_event_changed.rds')
mlt_exp <- readRDS('data/mlt_exp.rds')
mlt_exp_percentage <- readRDS('data/mlt_exp_percentage.rds')
mlt_exp_percentage_sp <- readRDS('data/mlt_exp_percentage_sp.rds')
mlt_exp_sp <- readRDS('data/mlt_exp_sp.rds')
mltExpLong <- readRDS('data/mltExpLong.rds')
mltExpPercentageLong <- readRDS('data/mltExpPercentageLong.rds')
population <- readRDS('data/population.rds')
population_sp <- readRDS('data/population_sp.rds')
populationLong <- readRDS('data/populationLong.rds')


######### 8) LOAD FUNCTIONS ######################################################
library(leaflet)
library(leaflet.extras)
library(rsconnect)

#----------- Necessary for Leaflet Extras ---------------------------------
# install.packages("remotes")
# remotes::install_github("bhaskarvk/leaflet.extras")


#------------Settings-----------------------------------------------------
# minZoom = 0
# maxZoom = 13

# #---------Robinson Projection--------------------------------------------------
# 
# epsg54030 <- leafletCRS(
#   crsClass = "L.Proj.CRS",
#   code = "EPSG:54030",
#   proj4def = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs",
#   resolutions = 2^(16:7))

#--------- ROBINSON PROJ Battle Deaths Distribution Map----------------------------------------

# deathsByYear <- function(chosenYear){
#   bin2 <- c(0, 25, 100, 500, 1000, 5000, 10000, 50000, 100000, Inf)
#   pal2 <- colorBin("YlOrRd", domain = death_sp@data[,chosenYear], bins = bin2)
#   
#   state_popup2 <- paste0("<strong>Country: </strong>", 
#                          death_sp@data$name, 
#                          "<br><strong>Battle Deaths: </strong>", 
#                          death_sp@data[,chosenYear])
#   
#   deaths_map <- leaflet(death_sp, options = leafletOptions(
#     crs = epsg54030, 
#     minZoom = minZoom, maxZoom = maxZoom)) %>%
#     setView(0, 0, 1.5) %>% addPolygons(
#       fillColor = ~pal2(death_sp@data[,chosenYear]),
#       weight = 1,
#       opacity = 1,
#       color = "white",
#       dashArray = "3",
#       fillOpacity = 0.7,
#       popup = state_popup2) %>% 
#     addLegend(pal = pal2, values = death_sp@data[,chosenYear],
#               opacity = 0.7, title = "Battle Deaths",
#               position = "bottomright") %>% 
#     addProviderTiles(providers$CartoDB.Positron)
#   
#   return(deaths_map)
# }

#--------- Battle Deaths Distribution Map----------------------------------------

deathsByYear <- function(chosenYear){
  bin2 <- c(Inf,50000,10000,2000,500, 100,25,0)
  pal2 <- colorBin("YlOrRd", domain = death_sp@data[,chosenYear], bins = bin2)
  
  state_popup2 <- paste0("<strong>Country: </strong>", 
                         death_sp@data$name, 
                         "<br><strong>Conflict Deaths: </strong>", 
                         death_sp@data[,chosenYear])
  
  deaths_map <- leaflet(death_sp) %>%
    setView(0, 0, 1.5) %>% addPolygons(
      fillColor = ~pal2(death_sp@data[,chosenYear]),
      weight = 1,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      popup = state_popup2) %>% 
    addLegend(pal = pal2, values = death_sp@data[,chosenYear],
              opacity = 0.7, title = "Conflict Deaths",
              position = "bottomright") %>% 
    addProviderTiles(providers$CartoDB.Positron) %>%
    addFullscreenControl( position = "topleft", pseudoFullscreen = TRUE)
  
  return(deaths_map)
}
#--------- Cumulative Battle Deaths Distribution Map----------------------------------------

deathsByYearRange <- function(startingYear, endingYear){
  startingIndex <- grep(startingYear, colnames(death_sp@data))
  endingIndex <- grep(endingYear, colnames(death_sp@data))
  death_sp@data$requiredSum <- rowSums(death_sp@data[,startingIndex:endingIndex])
  
  bin2 <- c(Inf,200000,100000,50000,10000,2000,1000,500,100,0)
  pal2 <- colorBin("YlOrRd", domain = death_sp@data[,"requiredSum"], bins = bin2)
  
  state_popup2 <- paste0("<strong>Country: </strong>", 
                         death_sp@data$name, 
                         "<br><strong>Conflict Deaths: </strong>", 
                         death_sp@data[,"requiredSum"])
  
  deaths_map_cum <- leaflet(death_sp) %>%
    setView(0, 0, 1.5) %>% addPolygons(
      fillColor = ~pal2(death_sp@data[,"requiredSum"]),
      weight = 1,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      popup = state_popup2) %>% 
    addLegend(pal = pal2, values = death_sp@data[,"requiredSum"],
              opacity = 0.7, title = "Cumulative Conflict Deaths",
              position = "bottomright") %>% 
    addProviderTiles(providers$CartoDB.Positron) %>%
    addFullscreenControl( position = "topleft", pseudoFullscreen = TRUE)
  
  return(deaths_map_cum)
}

#---------Military Expenditure Distribution Map----------------------------------------

expenditureByYear <- function(chosenYear){
  bin2 <- c(Inf,100,70,50,30,20,10,5,1,0)
  pal2 <- colorBin("YlOrRd", domain = mlt_exp_sp@data[,chosenYear], bins = bin2)
  
  state_popup2 <- paste0("<strong>Country: </strong>", 
                         mlt_exp_sp@data$name, 
                         "<br><strong>Military Expenditure in USD Billions: </strong>", 
                         mlt_exp_sp@data[,chosenYear])
  
  m_expenditure <- leaflet(mlt_exp_sp) %>%
    setView(0, 0, 1.5) %>% addPolygons(
      fillColor = ~pal2(mlt_exp_sp@data[,chosenYear]),
      weight = 1,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      popup = state_popup2) %>% 
    addLegend(pal = pal2, values = mlt_exp_sp@data[,chosenYear],
              opacity = 0.7, labFormat = labelFormat(
                suffix = " $"),
              title = "Military Expenditure in Billions",
              position = "bottomright") %>% 
    addProviderTiles(providers$CartoDB.Positron) %>%
    addFullscreenControl( position = "topleft", pseudoFullscreen = TRUE)
  
  m_expenditure
}

#--------- Cumulative Military Expenditure Distribution Map----------------------------------------

expenditureByYearRange <- function(startingYear, endingYear){
  startingIndex <- grep(startingYear, colnames(mlt_exp_sp@data))
  endingIndex <- grep(endingYear, colnames(mlt_exp_sp@data))
  mlt_exp_sp@data$requiredSum <- rowSums(mlt_exp_sp@data[,startingIndex:endingIndex])
  
  bin2 <- c(Inf,3000,2000,1500,900,600,300,150,30,0)
  pal2 <- colorBin("YlOrRd", domain = mlt_exp_sp@data[,"requiredSum"], bins = bin2)
  
  state_popup2 <- paste0("<strong>Country: </strong>", 
                         mlt_exp_sp@data$name, 
                         "<br><strong>Military Expenditure in USD Billions: </strong>", 
                         mlt_exp_sp@data[,"requiredSum"])
  
  m_expenditure_cum <- leaflet(mlt_exp_sp) %>%
    setView(0, 0, 1.5) %>% addPolygons(
      fillColor = ~pal2(mlt_exp_sp@data[,"requiredSum"]),
      weight = 1,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      popup = state_popup2) %>% 
    addLegend(pal = pal2, values = mlt_exp_sp@data[,"requiredSum"],
              opacity = 0.7, 
              labFormat = labelFormat(
                suffix = " $",
                digits = 0),
              title = "Military Expenditure in Billions",
              position = "bottomright") %>% 
    addProviderTiles(providers$CartoDB.Positron) %>%
    addFullscreenControl( position = "topleft", pseudoFullscreen = TRUE)
  
  return(m_expenditure_cum)
}


#--------- Percentage Military Expenditure Distribution Map----------------------------------------

militaryExpenditurePercentageByYear <- function(chosenYear){
  bin2 <- c(100,20,15,10,5,3,1.5,0)
  pal2 <- colorBin("YlOrRd", domain = mlt_exp_percentage_sp@data[,chosenYear], bins = bin2)
  
  state_popup2 <- paste0("<strong>Country: </strong>", 
                         mlt_exp_percentage_sp@data$name, 
                         "<br><strong> % of Military / Total Expenditures: </strong>",
                         mlt_exp_percentage_sp@data[,chosenYear])
  
  mlt_exp_percentage_map <- leaflet(mlt_exp_percentage_sp) %>%
    setView(0, 0, 1.5) %>% addPolygons(
      fillColor = ~pal2(mlt_exp_percentage_sp@data[,chosenYear]),
      weight = 1,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      popup = state_popup2) %>% 
    addLegend(pal = pal2, values = mlt_exp_percentage_sp@data[,chosenYear],
              opacity = 0.7,labFormat = labelFormat(
                suffix = "%"
              )
              , title = "% of Military / Total Expenditures",
              position = "bottomright") %>% 
    addProviderTiles(providers$CartoDB.Positron) %>%
    addFullscreenControl( position = "topleft", pseudoFullscreen = TRUE)
  
  mlt_exp_percentage_map
}

#---------  GDP Distribution Map -----------------------------------------------

gdpByYear <- function(chosenYear){
  bin2 <- c(Inf,5000,2000,1000,500,200,50,20,0)
  pal2 <- colorBin("YlOrRd", domain = gdp_sp@data[,chosenYear], bins = bin2)
  
  state_popup2 <- paste0("<strong>Country: </strong>", 
                         gdp_sp@data$name, 
                         "<br><strong>GDP (Billions of $): </strong>", 
                         gdp_sp@data[,chosenYear])
  
  gdp_map <- leaflet(gdp_sp) %>%
    setView(0, 0, 1.5) %>% addPolygons(
      fillColor = ~pal2(gdp_sp@data[,chosenYear]),
      weight = 1,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      popup = state_popup2) %>% 
    addLegend(pal = pal2, values = gdp_sp@data[,chosenYear],
              opacity = 0.7,labFormat = labelFormat(
                suffix = " $"),
              title = "GDP (Billions)",
              position = "bottomright") %>% 
    addProviderTiles(providers$CartoDB.Positron) %>%
    addFullscreenControl( position = "topleft", pseudoFullscreen = TRUE)
  
  return(gdp_map)
}

#--------- Cumulative GDP Distribution Map----------------------------------------

gdpByYearRange <- function(startingYear, endingYear){
  startingIndex <- grep(startingYear, colnames(gdp_sp@data))
  endingIndex <- grep(endingYear, colnames(gdp_sp@data))
  gdp_sp@data$requiredSum <- rowSums(gdp_sp@data[,startingIndex:endingIndex])
  
  bin2 <- c(Inf,100000,40000,20000,10000,4000,1000,400,0)
  pal2 <- colorBin("YlOrRd", domain = gdp_sp@data[,"requiredSum"], bins = bin2)
  
  state_popup2 <- paste0("<strong>Country: </strong>", 
                         gdp_sp@data$name, 
                         "<br><strong>Cumulated GDP (Billions of $): </strong>", 
                         gdp_sp@data[,"requiredSum"])
  
  gdp_map_cum <- leaflet(gdp_sp) %>%
    setView(0, 0, 1.5) %>% addPolygons(
      fillColor = ~pal2(gdp_sp@data[,"requiredSum"]),
      weight = 1,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      popup = state_popup2) %>% 
    addLegend(pal = pal2, values = gdp_sp@data[,"requiredSum"],
              opacity = 0.7,
              labFormat = labelFormat(
                suffix = " $"),
              title = "Cumulated GDP (Billions)",
              position = "bottomright") %>% 
    addProviderTiles(providers$CartoDB.Positron) %>%
    addFullscreenControl( position = "topleft", pseudoFullscreen = TRUE)
  
  return(gdp_map_cum)
}

#---------Population Distribution Map----------------------------------------

populationByYear <- function(chosenYear){
  bin2 <- c(Inf,1000,200,100,50,25,10,5,0)
  pal2 <- colorBin("YlOrRd", domain = population_sp@data[,chosenYear], bins = bin2)
  
  state_popup2 <- paste0("<strong>Country: </strong>", 
                         population_sp@data$name, 
                         "<br><strong>Population (Millions): </strong>", 
                         population_sp@data[,chosenYear])
  
  population_map <- leaflet(population_sp) %>%
    setView(0, 0, 1.5) %>% addPolygons(
      fillColor = ~pal2(population_sp@data[,chosenYear]),
      weight = 1,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      popup = state_popup2) %>% 
    addLegend(pal = pal2, values = population_sp@data[,chosenYear],
              opacity = 0.7, title = "Population (Millions)",
              position = "bottomright") %>% 
    addProviderTiles(providers$CartoDB.Positron) %>%
    addFullscreenControl( position = "topleft", pseudoFullscreen = TRUE)
  
  return(population_map)
}


#--------- Geo Conflicts Distribution Map----------------------------------------
ConflictByYear <- function(chosenYear){
  state_popup2 <- paste0("<strong>Country: </strong>", 
                         geo_event_changed[geo_event_changed$year==chosenYear,]$country)
  # Create a palette that maps factor levels to colors
  pal <- colorFactor(palette = c("navy", "red"), domain = c("non-state conflict", "state-based conflict"))
  # "state-based conflict", "non-stated conflict"
  m_conflict <- leaflet(geo_event_changed[geo_event_changed$year==chosenYear,]) %>% 
    addProviderTiles(providers$CartoDB.Positron) %>%
    setView(0, 0, 1.5) %>%
    addCircleMarkers(
      lng = ~longitude, lat = ~latitude,
      radius = 3,
      color = ~pal(geo_event_changed[geo_event_changed$year==chosenYear,]$type_of_violence),
      stroke = FALSE, fillOpacity = 0.5,
      popup = state_popup2) %>%
    addLegend(pal = pal, 
              values = geo_event_changed[geo_event_changed$year==chosenYear,]$type_of_violence,
              opacity = 0.7, title = "Conflicts",
              position = "bottomright") %>%
    addFullscreenControl( position = "topleft", pseudoFullscreen = TRUE)
  m_conflict
}


#------------- Merge with SP df -------------------------------------------------

mergeDbWithSp <- function(dataFrame){
  country_basemap %>%
    merge(dataFrame, country_basemap@data, by.x = "Country.Code", by.y = 'Country.Code')
}

# ------------ Create Cumulative Column On Data Frame -------------------------------

cumulativeTable <- function(startingYear, endingYear,dataFrame){
  startingIndex <- grep(startingYear, colnames(dataFrame))
  endingIndex <- grep(endingYear, colnames(dataFrame))
  dataFrame$Range_Sum <- rowSums(dataFrame[,startingIndex:endingIndex])
  
  return(dataFrame[,c(1,2,ncol(dataFrame))])
}


# pivot_longer(-c(Country.Name, Country.Code),
#              names_to = "year", 
#              values_to = "Military_Expenditure_On_Total_Expenditure",
#              values_drop_na = TRUE, 
#              names_transform = list(year = as.integer))




# Funtion for Render Table, that for some reason doesn't work
# topListByInputYear <- function(dataFrame, inputYear, dataColumnIndex = 4, showedElements=50){
#   top_10_year_battles <- dataFrame %>%
#     filter(year == as.character(inputYear)) %>%
#     dplyr::slice_max(n = showedElements, order_by = dataFrame[,colnames(dataFrame)[4]], with_ties = TRUE )
# }

# output$try <- renderTable(
#   topListByInputYear(gdpLong,inputYear = input$year, order_by = as.character(GDP) )
# )


######### 8) R Shiny WEB APP ######################################################

ui <- fluidPage(
  
  titlePanel("Global Conflicts"),
  theme = shinythemes::shinytheme("readable"),
  
  sidebarLayout(
    sidebarPanel(
      # Selection of data
      selectInput('dataChoice', 'Choose Data Set', choices = c("Conflict Deaths", "Conflicts",  "Military Expenditure", "GDP", "Population")),
      
      # Slider input chosen between fixed or cumulative
      uiOutput("sliders"),
      uiOutput("cumulativeDataCheck"),
      uiOutput("percentageDataCheck"),
      
      width = 3), # The total width must be 12 or less

    mainPanel(
      tabsetPanel(
        tabPanel("Map",
                 tags$style(type="text/css",
                            ".shiny-output-error { visibility: hidden; }",
                            ".shiny-output-error:before { visibility: hidden; }"
                 ), #For not showing errors in red
                 leafletOutput("map",height="90vh")),
        tabPanel("Top-50 Table", 
                 tableOutput("topListByInputYear"))
      )
    , width = 9), # The total width must be 12 or less
    position = "left",
    fluid = TRUE
  )
)

server <- function(input, output, session) {
  
#------- INTERACTIVE TABLE------------------------------------------------------
  
  output$topListByInputYear <- renderTable(
    if (input$dataChoice == "Conflict Deaths") {
    if (input$cumulativeDataCheck == 0) {
          topList <- battleDeathsLong %>%
            filter(year == as.character(input$year)) %>%
            dplyr::slice_max(
              n = 50,
              order_by = n_deaths,
              with_ties = TRUE
            )
          colnames(topList)[1] <- "Country"
          colnames(topList)[2] <- "Code"
          colnames(topList)[3] <- "Year"
          colnames(topList)[4] <- "Number of Deaths"
          topList
    }
    else {
      topList <- cumulativeTable(as.character(input$range[1]),as.character(input$range[2]),battle_deaths) %>%
      dplyr::slice_max(
        n = 50,
        order_by = Range_Sum,
        with_ties = TRUE
      )
      colnames(topList)[1] <- "Country"
      colnames(topList)[2] <- "Code"
      colnames(topList)[3] <- "Number of Deaths"
      topList
    }
  }
  else if (input$dataChoice == "Military Expenditure") {
    if (input$cumulativeDataCheck == 0) {
      if (input$percentageDataCheck == 0) {
        topList <- mltExpLong %>%
          filter(year == as.character(input$year)) %>%
          dplyr::slice_max(
            n = 50,
            order_by = Military_Expenditure_in_Billions_USD,
            with_ties = TRUE
          )
        colnames(topList)[1] <- "Country"
        colnames(topList)[2] <- "Code"
        colnames(topList)[3] <- "Year"
        colnames(topList)[4] <- "Military Expenditure i Billions $"
        topList
      }
      else {
        topList <- mltExpPercentageLong %>%
          filter(year == as.character(input$year)) %>%
          dplyr::slice_max(
            n = 50,
            order_by = Military_Expenditure_On_Total_Expenditure,
            with_ties = TRUE
          )
        colnames(topList)[1] <- "Country"
        colnames(topList)[2] <- "Code"
        colnames(topList)[3] <- "Year"
        colnames(topList)[4] <- "% of Military Over Total Expenditure"
        topList
      }
    }
    else {
      topList <- cumulativeTable(as.character(input$range[1]),as.character(input$range[2]),mlt_exp) %>%
        dplyr::slice_max(
          n = 50,
          order_by = Range_Sum,
          with_ties = TRUE
        )
      colnames(topList)[1] <- "Country"
      colnames(topList)[2] <- "Code"
      colnames(topList)[3] <- "Military Expenditure in Billions $"
      topList
    }
  }
  else if (input$dataChoice == "GDP") {
    if (input$cumulativeDataCheck == 0) {
        topList <- gdpLong %>%
          filter(year == as.character(input$year)) %>%
          dplyr::slice_max(
            n = 50,
            order_by = GDP_in_Billions_USD,
            with_ties = TRUE
          )
        colnames(topList)[1] <- "Country"
        colnames(topList)[2] <- "Code"
        colnames(topList)[3] <- "Year"
        colnames(topList)[4] <- "GDP in Billions $"
        topList
    } else {
      topList <- cumulativeTable(as.character(input$range[1]),as.character(input$range[2]),gdp) %>%
        dplyr::slice_max(
          n = 50,
          order_by = Range_Sum,
          with_ties = TRUE
        )
      colnames(topList)[1] <- "Country"
      colnames(topList)[2] <- "Code"
      colnames(topList)[3] <- "GDP in Billions $"
      topList
    }
  }
  else if (input$dataChoice == "Population") {
    topList <- populationLong %>%
      filter(year == as.character(input$year)) %>%
      dplyr::slice_max(
        n = 50,
        order_by = Population_in_Millions,
        with_ties = TRUE
      )
    colnames(topList)[1] <- "Country"
    colnames(topList)[2] <- "Code"
    colnames(topList)[3] <- "Year"
    colnames(topList)[4] <- "Inhabitants"
    topList
  }
  else if (input$dataChoice == "Conflicts") {
      topList <- filter(geo_event_changed,year == as.character(input$year))
      topList <- aggregate(topList$n_deaths, by=list(Category=topList$conflict_name), FUN=sum)
      topList <- dplyr::slice_max(topList,
        n = 50,
        order_by = x,
        with_ties = TRUE
        )
      colnames(topList)[1] <- "Conflict Name"
      colnames(topList)[2] <- "Number of Deaths"
      topList
  },  
  width = "auto",
  digits = 0,
  align = 'l'
  )
  
  
  
#------------SLIDERS-------------------------------------------------------  
 
  output$sliders = renderUI({
    if (input$cumulativeDataCheck == 1 &
        input$dataChoice != "Population" &
        input$dataChoice != "Conflicts") {
      sliderInput(
        "range",
        "Select Range",
        min = 1990,
        max = 2020,
        value = c(1990, 2020),
        sep = ""
      )
    } else  {
      sliderInput(
        'year',
        'Select Year',
        min = 1990,
        max = 2020,
        value = ifelse ( is.numeric(input$year) ,input$year,2020),
        sep = ""
      )
    }
  })
  
  output$cumulativeDataCheck = renderUI({
    if (input$dataChoice == "Conflict Deaths" |
        input$dataChoice == "Military Expenditure" |
        input$dataChoice == "GDP"
        ) {
      checkboxInput("cumulativeDataCheck", "Use cumulative data", value = FALSE)
    }
  })
  output$percentageDataCheck = renderUI({
    if (input$dataChoice == "Military Expenditure" &
        input$cumulativeDataCheck == 0) {
      checkboxInput("percentageDataCheck", "Use percentage data", value = FALSE)
    }
  })
  
#------------MAPS-------------------------------------------------------  
  
  output$map <- renderLeaflet({
    if (input$dataChoice == "Conflict Deaths") {
      if (input$cumulativeDataCheck == 0) {
          deathsByYear(as.character(input$year))
      }
      else {
        deathsByYearRange(as.character(input$range[1]),
                          as.character(input$range[2]))
      }
    }
    else if (input$dataChoice == "Military Expenditure") {
      if (input$cumulativeDataCheck == 0) {
        if (input$percentageDataCheck == 0) {
          expenditureByYear(as.character(input$year))
        }
        else {
          militaryExpenditurePercentageByYear(as.character(input$year))
        }
      }
      else {
        expenditureByYearRange(as.character(input$range[1]),
                               as.character(input$range[2]))
      }
    }
    else if (input$dataChoice == "GDP") {
      if (input$cumulativeDataCheck == 0) {
        gdpByYear(as.character(input$year))
      } else {
        gdpByYearRange(as.character(input$range[1]),
                       as.character(input$range[2]))
      }
    }
    else if (input$dataChoice == "Population") {
      populationByYear(as.character(input$year))
    }
    else if (input$dataChoice == "Conflicts") {
      ConflictByYear(as.character(input$year))
    }
  })
  
  
}
shinyApp(ui = ui, server = server)