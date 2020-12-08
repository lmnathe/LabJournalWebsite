library(shiny)
library(dplyr)
library(shinydashboard)
#library(policyPlot)
library(plotly)  

ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard",
                  dropdownMenuOutput("messageMenu")
  ),
  #dashboardSidebar(),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Google Search Trends", tabName = "states", icon = icon("cog",lib = "glyphicon")),
      menuItem("DMA - Cases", tabName = "dmas", icon = icon("credit-card"))#From glyphicon library
      )
    ),
    
  

## Body content
dashboardBody(
  tabItems(

    # Credit tab content
    tabItem(tabName = "states",
            fluidRow(
              box(plotlyOutput("plot7", height = 500,width = 1000))
            ),
            fluidRow(
              box(plotlyOutput("plot8", height = 500,width = 1000))
            ),
            fluidRow(
              box(plotlyOutput("plot9", height = 500,width = 1000))
            ),
            fluidRow(
              box(plotlyOutput("plot10", height = 500,width = 1000))
            )
    ),
    tabItem(tabName = "dmas",
            fluidRow(
              box(plotlyOutput("plot1", height = 500,width = 850)),
              box(plotlyOutput("plot2", height = 500,width = 850)),
              box(plotlyOutput("plot3", height = 500,width = 850)),
              box(plotlyOutput("plot4", height = 500,width = 850)),
              box(plotlyOutput("plot5", height = 500,width = 850)),
              box(plotlyOutput("plot6", height = 500,width = 850))

  )
)
)
)
)
