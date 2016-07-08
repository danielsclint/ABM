library(shiny)
library(shinydashboard)

source('directoryInput.R')
source('properties.R')

header <- dashboardHeader(
            title = paste("Input Monitoring Tool", client.name, sep = " - "),
            titleWidth = 320
)

sidebar <- dashboardSidebar(
              width = 320,

              tags$hr(),
              fluidRow(column(12, selectInput("inputType", "Select Input Type", choices = c("", "Socioeconomic Data", "Highway Network", "Transit Network")))),
              
              tags$hr(),
              directoryInput("inputDir", label = "Select Input Location", value = ""),
              
              conditionalPanel("input.inputType == 'Highway Network' || input.inputType == 'Transit Network'", 
                               checkboxInput("buildNetwork", "Build Network", FALSE)),
              
              tags$hr(),
              conditionalPanel("input.mainTabs > 0", fluidRow(column(6, align="center", offset = 3, actionButton("action", "RUN"))))
)
  
body <- dashboardBody(
          uiOutput('mainTabs')
)

dashboardPage(header, sidebar, body)