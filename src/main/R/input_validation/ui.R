library(shiny)
library(shinydashboard)

source('directoryInput.R')

header <- dashboardHeader(
            title = "Input Monitoring Tool - SANDAG",
            titleWidth = 320
)

sidebar <- dashboardSidebar(
              width = 320,

              tags$hr(),
              fluidRow(column(12, selectInput("inputType", "Select Input Type", choices = c("", "Socioeconomic Data", "Highway Network", "Transit Network")))),
              
              tags$hr(),
              directoryInput("inputDir", label = "Select Input Location", value = ""),
              
              tags$hr(),
              fluidRow(column(6, align="center", offset = 3, actionButton("action", "RUN")))
)
  
body <- dashboardBody(
          uiOutput('mainTabs')
)

dashboardPage(header, sidebar, body)