library(shiny)
library(here)
library(rsconnect)
library(tidyverse)
library(sf)
library(geojsonR)
library(rgdal)
library(dplyr)
library(ggplot2)
library(reshape2)
library(ggiraph)
library(RColorBrewer)
library(hrbrthemes)
library(shinyWidgets)
options(shiny.usecairo=T) #used to get the plot to look crisp in Shiny
options(scipen=999999)


here()

# read data
conflicts_deaths <- readRDS('data/conflicts_deaths.rds')
yr_conflict <- readRDS("data/yr_conflicts.rds")
y <- as.character(c(1990:2020))


#Make Maps
ui <- fluidPage(
  # titlePanel("The time of your life in Conflicts: "),
  # setBackgroundImage(src = "https://www.fillmurray.com/1920/1080"),
  fluidRow(
    column(4, 
           br(),
           strong("THE TIME OF YOUR LIFE", style = "font-size: 28pt"),
           br(),
           strong("IN CONFLICTS", style = "color:darkred; font-size: 28pt"),
           br(),
           strong("& PEACE", style = "color:grey; font-size: 28pt"),
           br(),
           helpText("Create a pie chart and a bar chart to display the fraction of your life in conflicts as well as conflicts deaths in your country."),
           offset = 1),
    column(3,
           br(),
           selectInput("country", "Select Your Country:", choices = rownames(yr_conflict)),
           selectInput("year", "Select Your Birth Year:", choices = y),
           offset = 0.9)
  ),
  fluidRow(
    column(3,
           br(),
           br(),
           br(),
           br(),
           ggiraphOutput("distPie", width = "700px", height = "700px")),
    column(3,
           ggiraphOutput('barplot', width = "1400px", height = "800px"),
           offset = 0.7)
  )


  
  
  # sidebarLayout(
  #   sidebarPanel(
  #     helpText("Create a pie chart to display the fraction of your life in conflicts"),
  #     selectInput("country", "Select Your Country:", choices = rownames(yr_conflict)),
  #     selectInput("year", "Select Your Birth Year:", choices = y)
  #   ),
  #   mainPanel(
  #     strong("THE TIME OF YOUR LIFE", style = "color:red; font-size: 25pt"),
  #     br(),
  #     strong("IN CONFLICTS", style = "color:darkred; font-size: 25pt"),
  #     br(),
  #     strong("& PEACE", style = "color:grey; font-size: 25pt"),
  #     ggiraphOutput('barplot', width = "1500px", height = "800px"),
  #     ggiraphOutput("distPie")
  #   )
  # )
)

server <- function(input, output) {
  
  output$distPie <- renderggiraph({
    yr_start <- as.numeric(input$year)
    count = 0
    for(yr in yr_start:2020){
      if (yr_conflict[input$country, as.character(yr)] == 1){
        count = count+1
      }
    }
    
    # Create test data.
    data <- data.frame(
      Status=c("CONFLICTS", "PEACE"),
      count=c(count, 2020-yr_start-count+1)
    )
    
    # Compute percentages
    data$fraction <- round((data$count/(2020-yr_start+1))*100)

    donut_plot <- ggplot(data, aes(y = fraction, fill = Status)) +
      geom_bar_interactive(
        aes(x = 2, tooltip = paste("If you were born in", paste0(input$year, ","),"\n", input$country, "has been in",Status, "for", fraction, "% of your life.")),
        width = 0.8,
        stat = "identity",
        show.legend = FALSE
      ) +
      annotate(
        geom = "text",
        x = 0.5,
        y = 0,
        label = paste0(data$fraction[1], '%'),
        size = 13,
        color = "darkred"
      ) +
      scale_fill_manual(values = c(CONFLICTS = "darkred", PEACE = "lightgrey")) +
      coord_polar(theta = "y", start = 0) +
      theme_void()+
      xlim(.5, 2.5)
    
    ggiraph(code = print(donut_plot))
    
    
  }
  )
  # https://www.jadaliyya.com/Details/27474
  output$barplot <- renderggiraph({
    c_death <- conflicts_deaths[conflicts_deaths$country==input$country,]
    barplot <- ggplot(c_death, aes(x = year)) +
      geom_bar_interactive(stat = "identity",  fill="darkred", aes(x = year, y = total, tooltip = paste("The death of conflict in", year, "in", input$country, "is", total))) +
      theme_ipsum(grid = "Y", axis_title_just = "mc") +
      scale_x_continuous(labels=as.character(c_death$year),breaks=c_death$year) +
      scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
      labs(x = "Year", y = "Conflicts deaths", face = "bold") +
      theme(aspect.ratio = 4/7,
            axis.text.x = element_text(color = 'darkgrey', size = 6, angle=90),
            axis.text.y = element_text(color = 'darkred', size = 6),
            axis.title.x = element_text(margin=margin(t=10), size = 7, face = "bold"),
            axis.title.y = element_text(margin=margin(r=10), size = 7, face = "bold"),
            panel.grid.minor.x=element_blank(),
            panel.grid.major.x=element_blank())
      # scale_y_reverse()
      # ggtitle(paste("Conflicts deaths in", input$country))
    
    ggiraph(code = {print(barplot)})
    
  })
}

shinyApp(ui, server)

# # https://www.royfrancis.com/a-guide-to-elegant-tiled-heatmaps-in-r-2019/
# conflicts_deaths <- conflicts_deaths %>%
#   # convert state to factor and reverse order of levels
#   mutate(state=factor(country, levels=rev(sort(unique(country))))) %>%
#   # create a new variable from count
#   mutate(totalfactor=cut(total, breaks=c(-1, 0, 25, 100, 500, 1000, 5000, max(total, na.rm=T)),
#                          labels=c("0", "1-25", "26-100", "101-500", "501-1000", "1001-5000", ">5000"))) %>%
#   # change level order
#   mutate(totalfactor=factor(as.character(totalfactor), levels=rev(levels(totalfactor))))
# 
# textcol <- "grey40"
# heatmap <- ggplot(conflicts_deaths, aes(x = year, y = country, fill = totalfactor), width = 30, height = 70) +
#   coord_fixed() +
#   geom_tile(colour="white", size=0.2) +
#   guides(fill=guide_legend(title="Conflicts deaths"))+
#   labs(x="", y="", title="Global Conflicts from 1990 to 2020")+
#   scale_y_discrete(expand=c(0, 0))+
#   scale_x_discrete(expand=c(0, 0), breaks=c("1990", "1995", "2000", "2005", "2010", "2015", "2020"))+
#   scale_fill_manual(values=c("#d53e4f","#f46d43",  "#fdae61", "#fee08b","#abdda4", "#e6f598", "#ddf1da"), na.value = "grey90")+
#   theme_grey(base_size=10)+
#   theme(aspect.ratio= 7/3,
#         legend.position="right", legend.direction="vertical",
#         legend.title=element_text(colour=textcol),
#         legend.margin=margin(grid::unit(0, "cm")),
#         legend.text=element_text(colour=textcol, size=7, face="bold"),
#         legend.key.height=grid::unit(0.8, "cm"),
#         legend.key.width=grid::unit(0.2, "cm"),
#         axis.text.x=element_text(size=10, colour=textcol),
#         axis.text.y=element_text(size=1, colour=textcol),
#         axis.ticks=element_line(size=0.4),
#         plot.background=element_blank(),
#         panel.border=element_blank(),
#         plot.margin=margin(0.7, 0.4, 0.1, 0.2, "cm"),
#         plot.title=element_text(colour=textcol, hjust=0, size=10, face="bold"))
# 
# heatmap
