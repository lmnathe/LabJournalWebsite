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
      menuItem("Google Search Trends", tabName = "Overview", icon = icon("cog",lib = "glyphicon")),
      menuItem("DMA - Cases", tabName = "Credit", icon = icon("credit-card"))#From glyphicon library
      )
    ),
    
  

## Body content
dashboardBody(
  tabItems(

    # Credit tab content
    tabItem(tabName = "Overview",
            fluidRow(
              box(plotlyOutput("plot1", height = 500,width = 1000))
            ),
            fluidRow(
              box(plotlyOutput("plot2", height = 500,width = 1000))
            )
    ),
    tabItem(tabName = "Credit",
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