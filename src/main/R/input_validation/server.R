options(shiny.maxRequestSize=10000*1024^2)
options(shiny.port=1234)

library(shiny)
library(shinydashboard)
library(DT)
library(rgdal)
library(tmap)
library(dplyr)
library(leaflet)
library(readr)

source('checker_functions.R')

read_data <- function(type,  inputPath){
  if(type == "Socioeconomic Data"){
    source("seData.R")
    #getData is a function (with argument as inputPath) in seData.R to read csv file
    return(getData(inputPath))
  }
  else if (type == "Highway Network"){
    source("hwyData.R")
    #getData is a function (with argument as inputPath) in hwyData.R to read bin file
    return(getData(inputPath)) 
  }
  else{
    return(NULL)
  }
}

create_plots_old <- function(type, inputData, shapeFilePath){
  if(type == "Socioeconomic Data"){
    source("seData.R")
    #getPlots is a function (with argument as inputData and shapeFilePath) in seData.R
    return(getPlots(inputData, shapeFilePath))
  }
  else {
    return(NULL) 
  }
}

create_static_plots <- function(type, inputData, taz_shape){
  if(type == "Socioeconomic Data"){
    source("seData.R")
    #getPlots is a function (with argument as inputData and taz_shape) in seData.R
    return(getPlots(inputData, taz_shape))
  }
  else {
    return(NULL) 
  }
}

create_growth_plots <- function(growthData){
  source("seData.R")
  #getGrowthPlots is a function (with argument as growthData) in seData.R
  return(getGrowthPlots(growthData))
}

create_ext_ext_growth_plot <- function(data, oCordon, dCordon){
  source("seData.R")
  return(getExtExtGrowthPlot(data, oCordon, dCordon))
}

create_ext_int_growth_plot <- function(data, cordonName){
  source("seData.R")
  return(getExtIntGrowthPlot(data, cordonName))
}

# The conditional panel to show when shiny is busy
loadingPanel <- conditionalPanel(paste("input.action > 0 &&", "$('html').hasClass('shiny-busy')"), "Calculation in progress ...")

shinyServer(server <- function(input, output, session){
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$inputDir
    },
    handlerExpr = {
      if (input$inputDir > 0) {
        # condition prevents handler execution on initial app launch
        
        # launch the directory selection dialog with initial path read from the widget
        path = choose.dir(default = readDirectoryInput(session, 'inputDir'))
        
        # update the widget value
        updateDirectoryInput(session, 'inputDir', value = path)
      }
    }
  )
  
  #-----------------------------------------------------#
  #---------------- Read UI user inputs ----------------#
  #-----------------------------------------------------#
  input_type <- renderText({
    input$inputType
  })
  
  input_location <- renderText({
    #readDirectoryInput(session, 'inputDir')
    return("C:\\Projects\\SANDAG\\ABM Support, 2016\\socioeconomic")
    #return("C:\\Projects\\SANDAG\\ABM Support, 2016\\network_building")
  })
  
  shapefile_location <- renderText({
    #readDirectoryInput(session, 'shapeFileDir')
    return("C:\\Projects\\SANDAG\\ABM Support, 2016\\taz_shape")
    
  })
  
  growthData_location <- renderText({
    return("C:\\Projects\\SANDAG\\ABM Support, 2016\\growthFiles")
  })
  
  input_data <- reactive({
    input$action
    isolate({
      read_data(input_type(), input_location())
    })
  })
  
  output$mainTabs <- renderUI({
    if(input_type() == "") {
      
    }
    
    else if(input_type() == "Socioeconomic Data"){
      tabsetPanel(id = "mainTabs",
          tabPanel("Report", br(), br(),
                   tagList(
                     tags$head(
                       tags$link(rel="stylesheet", type="text/css",href="js/style.css"),
                       tags$script(type="text/javascript", src = "js/busy.js")
                     )
                   ),
                   
                   div(class = "busy",  
                       p("Calculation in progress ..."),
                       img(src="js/hourglass.gif")
                   ),
                   
                   htmlOutput("employmentRatioCheck"), br(), br(),
                   htmlOutput("enrollmentRatioCheck")
          ),
          
          tabPanel("Year By Year Growth",
              tabsetPanel(id = "growthTabs",
                  tabPanel("Plots", br(), uiOutput("growthPlots")),
                  tabPanel("Ext - Ext Growth", br(),
                           h2("External External Trips by Year"),
                           uiOutput("extExtOriginZones"),
                           br(), uiOutput("extExtGrowthPlots")),
                  tabPanel("Ext - Int Growth", br(), 
                           h2("External Internal Trips by Year"),
                           uiOutput("extIntZones"),
                           br(), plotOutput("extIntGrowth", width = "65%", height = "450px"))
              )
          ),
          
          tabPanel("Static Plots",
                   br(), selectInput("staticMapNum", "Select Map:",
                                        choices = c("Military Employment" = 1, "Military Group Quarters" = 2,
                                                        "School Enrollment (K to 8)" = 3, "School Enrollment (9 to 12)" = 4,
                                                        "College Enrollment" = 5, "College (Other) Enrollment" = 6, "College Students" = 7,
                                                        "Population Density" = 8, "Average Income" = 9), width = "225px"),
                   tagList(
                     tags$head(
                       tags$link(rel="stylesheet", type="text/css",href="js/style.css"),
                       tags$script(type="text/javascript", src = "js/busy.js")
                     )
                   ),
                   
                   div(class = "busy",  
                       p("Rendering in progress ..."),
                       img(src="js/hourglass.gif")
                   ),
                   
                   plotOutput("staticPlot", width = "800px", height = "600px")
                   
                   #br(), plotOutput("spatialPlot1", width = "800px", height = "600px"),
                   #br(), plotOutput("spatialPlot2", width = "800px", height = "600px"),
                   #br(), plotOutput("spatialPlot3", width = "800px", height = "600px"),
                   #br(), plotOutput("spatialPlot4", width = "800px", height = "600px"),
                   #br(), plotOutput("spatialPlot5", width = "800px", height = "600px"),
                   #br(), plotOutput("spatialPlot6", width = "800px", height = "600px"),
                   #br(), plotOutput("spatialPlot7", width = "800px", height = "600px"),
                   #br(), plotOutput("spatialPlot8", width = "800px", height = "600px"),
                   #br(), plotOutput("spatialPlot9", width = "800px", height = "600px")
          ),

          tabPanel("Interactive Plots", 
                   br(), fluidRow(column(7, offset = 5, align="left",
                            selectInput("leafletMapNum", "Select Map:",
                                     choices = c("Military Employment" = 1, "Military Group Quarters" = 2,
                                                 "School Enrollment (K to 8)" = 3, "School Enrollment (9 to 12)" = 4,
                                                 "College Enrollment" = 5, "College (Other) Enrollment" = 6, "College Students" = 7,
                                                 "Population Density" = 8, "Average Income" = 9), width = "220px"))),
                   tagList(
                     tags$head(
                       tags$link(rel="stylesheet", type="text/css",href="js/style.css"),
                       tags$script(type="text/javascript", src = "js/busy.js")
                     )
                   ),
                   
                   div(class = "busy",  
                       p("Rendering in progress ..."),
                       img(src="js/hourglass.gif")
                   ),
                   
                   tagList(
                     tags$head(
                       tags$style(
                         ".leaflet .legend {line-height: 22px;}"
                         #".leaflet .legend i{float: left;}",
                         #".leaflet .legend label{float:left; text-align: left;}",
                         #".leaflet .legend span{position: absolute; top: 0; left: 0; float: left; text-align: left;}"
                       )
                     )
                   ),
                   
                   leafletOutput("leafletPlot", width = 800, height = 600), actionButton("reset_leaflet", "Reset view")
                   
                   #br(), fluidRow(column(8, align="center", h4("Military Employment"))),
                   #leafletOutput("leafletPlot_milEmp", width = 800, height = 550), actionButton("reset_milEmp", "Reset view"),
                   #br(), br(), fluidRow(column(8, align="center", h4("Military Group Quarters"))),
                   #leafletOutput("leafletPlot_milGQ", width = 800, height = 550), actionButton("reset_milGQ", "Reset view"),
                   #br(), br(), fluidRow(column(8, align="center", h4("School Enrollment"))),
                   #leafletOutput("leafletPlot_schoolEnroll", width = 800, height = 550), actionButton("reset_schoolEnroll", "Reset view"),
                   #br(), br(), fluidRow(column(8, align="center", h4("College Enrollment"))),
                   #leafletOutput("leafletPlot_collegeEnroll", width = 800, height = 550), actionButton("reset_collEnroll", "Reset view"),
                   #br(), br(), fluidRow(column(8, align="center", h4("College Students"))),
                   #leafletOutput("leafletPlot_collegeStudent", width = 800, height = 550), actionButton("reset_collStudent", "Reset view"),
                   #br(), br(), fluidRow(column(8, align="center", h4("Population Density"))),
                   #leafletOutput("leafletPlot_popDensity", width = 800, height = 550), actionButton("reset_popDen", "Reset view"),
                   #br(), br(), fluidRow(column(8, align="center", h4("Average Income"))),
                   #leafletOutput("leafletPlot_averageIncome", width = 800, height = 550), actionButton("reset_avgInc", "Reset view")
          ),
          
          tabPanel("SB Comparison", br(), dataTableOutput("sbComparison"))
      )
    }
    
    else if(input_type() == "Highway Network") {
      tabsetPanel(id = "mainTabs",
          tabPanel("Report", br(),
                    br(), tableOutput("missingCheck1"),
                    br(), tableOutput("missingCheck2"),
                    br(), tableOutput("missingCheck3"),
                    br(), br(), box(title = "Missing Attribute Checks", solidHeader = TRUE, status = "info", width = 5, collapsible = TRUE, collapsed = TRUE, tableOutput("missingCheck4"))),
          
          tabPanel("Highway Data", br(),
                   fluidRow(textInput("hwycovid", "SEARCH HWYCOVID:", "", width = "200px"),
                            br(), dataTableOutput("hwyData"))),
          
          tabPanel("Zone Connectors", br(), br(),
                   fluidRow(dataTableOutput("zoneConnectors")))
      )
    }
    
    else if(input_type() == "Transit Network") {
      tabsetPanel(id = "mainTabs",
                  tabPanel("Tab 1", br(), br()),
                  tabPanel("Tab 2", br(), br()),
                  tabPanel("Tab 3", br(), br())
      )
    }
    
  })
  
  #----------------------------------------------#
  #------------ SE Data Ratio Checks ------------#
  #----------------------------------------------#
  output$employmentRatioCheck <- renderText({
    if(input$action > 0){
      data <- input_data()
      if(!is.null(data)){
        totalEmp <- sum(data$emp_total)
        totalWorkers <- sum(data$workers)
        ratio <- totalEmp/totalWorkers
        
        str1 <- "------------------------------------------------------"
        str2 <- paste("Total Employment: ", prettyNum(round(totalEmp), big.mark = ","))
        str3 <- paste("Total Workers: ", prettyNum(round(totalWorkers), big.mark = ","))
        str4 <- ""
        str5 <- paste("Ratio (Emp to Workers):", prettyNum(round((totalEmp/totalWorkers),3), big.mark = ","))
        str6 <- "------------------------------------------------------"
        
        
        if(ratio < 0.9 | ratio > 1.1){
          str7 <- paste("<b>", "WARNING:", "</b>", "Ratio is NOT within the expected range of", 0.9, "and", 1.1)
          HTML(paste(str1, str2, str3, str4, str5, str6, str7, sep = '<br/>'))
        } else {
          HTML(paste(str1, str2, str3, str4, str5, str6, sep = '<br/>'))
        }
      }
    }
  })
  
  output$enrollmentRatioCheck <- renderText({
    if(input$action > 0){
      data <- input_data()
      if(!is.null(data)){
        totalSchoolEnroll <- sum(data$EnrollGradeKto8) + sum(data$EnrollGrade9to12)
        totalSchoolStudents <- sum(data$school_students)
        ratio <- totalSchoolEnroll/totalSchoolStudents
        
        str1 <- "------------------------------------------------------"
        str2 <- paste("School Enrollment: ", prettyNum(round(totalSchoolEnroll), big.mark = ","))
        str3 <- paste("School Students: ", prettyNum(round(totalSchoolStudents), big.mark = ","))
        str4 <- ""
        str5 <- paste("Ratio (Enrollment to Students):", prettyNum(round((totalSchoolEnroll/totalSchoolStudents),3), big.mark = ","))
        str6 <- "------------------------------------------------------"
        
        if(ratio < 0.9 | ratio > 1.1){
          str7 <- paste("<b>", "WARNING:", "</b>", "Ratio is NOT within the expected range of", 0.9, "and", 1.1)
          HTML(paste(str1, str2, str3, str4, str5, str6, str7, sep = '<br/>'))
        } else {
          HTML(paste(str1, str2, str3, str4, str5, str6, sep = '<br/>'))
        }
      }
    }
  })
  
  #----------------------------------------------#
  #------------- SE Data Spatial Plots ----------#
  #----------------------------------------------#
  values <- reactiveValues()
  
  spatialPlotNum <- reactive({input$staticMapNum})
  
  observe({
    if (!is.null(input_data()))
      #values[['spatialPlots']] <- create_plots_old(input_type(), input_data(), shapefile_location())
      values[['spatialPlots']] <- create_static_plots(input_type(), input_data(), taz_shape())
  })
  
  output$staticPlot <- renderPlot({
    if(input$action > 0)
      if(spatialPlotNum() == 1)
        values[['spatialPlots']][[1]]
      else if(spatialPlotNum() == 2)
        values[['spatialPlots']][[2]]
      else if(spatialPlotNum() == 3)
        values[['spatialPlots']][[3]]
      else if(spatialPlotNum() == 4)
        values[['spatialPlots']][[4]]
      else if(spatialPlotNum() == 5)
        values[['spatialPlots']][[5]]
      else if(spatialPlotNum() == 6)
        values[['spatialPlots']][[6]]
      else if(spatialPlotNum() == 7)
        values[['spatialPlots']][[7]]
      else if(spatialPlotNum() == 8)
        values[['spatialPlots']][[8]]
      else if(spatialPlotNum() == 9)
        values[['spatialPlots']][[9]]
      else
        return(NULL)
  })
  
  #output$spatialPlot1 <- renderPlot({
  #  if(input$action > 0)
  #    values[['spatialPlots']][[1]]
  #})
  #
  #output$spatialPlot2 <- renderPlot({
  #  if(input$action > 0)
  #    values[['spatialPlots']][[2]]
  #})
  #
  #output$spatialPlot3 <- renderPlot({
  #  if(input$action > 0)
  #    values[['spatialPlots']][[3]]
  #})
  #
  #output$spatialPlot4 <- renderPlot({
  #  if(input$action > 0)
  #    values[['spatialPlots']][[4]]
  #})
  #
  #output$spatialPlot5 <- renderPlot({
  #  if(input$action > 0)
  #    values[['spatialPlots']][[5]]
  #})
  #
  #output$spatialPlot6 <- renderPlot({
  #  if(input$action > 0)
  #    values[['spatialPlots']][[6]]
  #})
  #
  #output$spatialPlot7 <- renderPlot({
  #  if(input$action > 0)
  #    values[['spatialPlots']][[7]]
  #})
  #
  #output$spatialPlot8 <- renderPlot({
  #  if(input$action > 0)
  #    values[['spatialPlots']][[8]]
  #})
  #
  #output$spatialPlot9 <- renderPlot({
  #  if(input$action > 0)
  #    values[['spatialPlots']][[9]]
  #})
  
  
  #-------------------------------------------------------#
  #------------- SE data year over year growth -----------#
  #-------------------------------------------------------#
  originCordon <- reactive({input$origin_cordon})
  #dtaz <- reactive({input$dest})
  cordonName <- reactive({input$cordon})
  
  getGrowthData <- function(){
    read_csv(paste(growthData_location(), "parameters_by_years.csv", sep = "/"))
  }
  
  getExtExtGrowthData <- function(){
    read_csv(paste(growthData_location(), "externalExternalTrips_by_year.csv", sep = "/"))
  }
  
  getExtIntGrowthData <- function(){
    read_csv(paste(growthData_location(), "externalInternalControlTotals_by_year.csv", sep = "/"))
  }
  
  getCordonDefinition <- function(){
    read_csv(paste(growthData_location(), "cordon_definition.csv", sep = "/"))
  }
  
  output$extExtOriginZones <- renderUI({
    df <- getExtExtGrowthData()
    extExtZones <- unique(df %>% select(originTaz))
    cordonDef <- getCordonDefinition()
    extExtZones <- left_join(extExtZones, cordonDef, by = c("originTaz" = "TAZ"))
    extExtZones <- extExtZones[,c("Name")]
    selectInput("origin_cordon", "Select Origin Cordon", choices = as.list(extExtZones), width = "200px")
  })

  output$extIntZones <- renderUI({
    df <- getExtIntGrowthData()
    extIntZones <- unique(df %>% select(taz))
    cordonDef <- getCordonDefinition()
    extIntZones <- left_join(extIntZones, cordonDef, by = c("taz" = "TAZ"))
    extIntZones <- extIntZones[,c("Name")]
    selectInput("cordon", "Select Cordon", choices = as.list(extIntZones), width = "200px")
  })
  
  output$growthPlots <- renderUI({
    if(input$action > 0){
      get_growth_plot_output_list()
    }
  })
  
  get_growth_plot_output_list <- function() {
    growth_data <- getGrowthData()
    growth_plots <- create_growth_plots(growth_data)

    #plotsPath = "C:\\Projects\\SANDAG\\ABM Support, 2016\\shinyR\\plots"
    #savePlots <- 0
    #
    #if(savePlots){
    #  for(i in 1:length(growth_plots)){
    #    file = paste(paste(plotsPath, paste("plot", i, sep = "_"), sep = "\\"),".png", sep = "")
    #    png(file, width = 1000, height = 1000)
    #    print(growth_plots[[i]])
    #    dev.off()
    #  }
    #}
    
    # Insert plot output objects into the list
    plot_output_list <- lapply(1:length(growth_plots), function(i) {
      plotname <- paste("growth_plot", i, sep="")
      plot_output_object <- plotOutput(plotname)
      plot_output_object <- renderPlot({growth_plots[[i]]}, width = 600, height = 350)
    })
    
    # Convert the list to a tagList - this is necessary for the list of items to display properly.
    do.call(tagList, plot_output_list)
    return(plot_output_list)
  }
  
  output$extExtGrowthPlots <- renderUI({
    if(input$action > 0){
      if(!is.null(originCordon()))
        get_ext_ext_growth_plot_output_list()
      else
        return(NULL)
    }
  })
  
  get_ext_ext_growth_plot_output_list <- function() {
    extExtGrowthData <- getExtExtGrowthData()
    cordonDef <- getCordonDefinition()
    extExtGrowthData$origCordonName <- cordonDef$Name[match(extExtGrowthData$originTaz, cordonDef$TAZ)]
    extExtGrowthData$destCordonName <- cordonDef$Name[match(extExtGrowthData$destinationTaz, cordonDef$TAZ)]
    
    destCordons <- unique(extExtGrowthData %>% select(destinationTaz))
    destCordons <- left_join(destCordons, cordonDef, by = c("destinationTaz"="TAZ"))
    destCordons <- destCordons$Name
    
    extExt_growth_plots <- vector("list", length(destCordons) - 1)  # -1 beacuase D same as O is not plotted
    i <- 1
    
    for(d in destCordons){
      if(d == originCordon()) next
      subset_data <- extExtGrowthData %>% filter(origCordonName == originCordon(), destCordonName == d)
      extExt_growth_plots[[i]] <- create_ext_ext_growth_plot(subset_data, originCordon(), d)
      i <- i + 1
    } 
    
    # Insert plot output objects into the list
    plot_output_list <- lapply(1:length(extExt_growth_plots), function(i) {
      plotname <- paste("extExt_growth_plot", i, sep="")
      plot_output_object <- plotOutput(plotname)
      plot_output_object <- renderPlot({extExt_growth_plots[[i]]}, width = 550, height = 300)
    })
    
    # Convert the list to a tagList - this is necessary for the list of items to display properly.
    do.call(tagList, plot_output_list)
    return(plot_output_list)
  }
  
  output$extIntGrowth <- renderPlot({
    if(input$action > 0){
      extIntGrowthData <- getExtIntGrowthData()
      cordonDef <- getCordonDefinition()
      extIntGrowthData <- left_join(extIntGrowthData, cordonDef, by = c("taz" = "TAZ")) 
      if(!is.null(cordonName())){
        subset_data <- extIntGrowthData %>% filter(Name == cordonName())
        create_ext_int_growth_plot(subset_data, cordonName())
      }
      else 
        return(NULL)
    }
  })
  
  #-----------------------------------------------------#
  #------------- SE Data Interactive plots -------------#
  #-----------------------------------------------------#
  
  taz_shape <- reactive({
    input$action
    isolate({
      taz_shape <- readOGR(paste(shapefile_location(), "taz_simplified.shp", sep = "\\"), layer = "taz_simplified")
      taz_shape <- spTransform(taz_shape, CRS("+init=epsg:4326"))
      #taz_data <- createTazData(input_data)
      taz_data <- createTazData(input_data())
      taz_shape@data <- left_join(taz_shape@data, taz_data, by = c("TAZ" = "TAZ"))
      taz_shape@data[is.na(taz_shape@data)] <- 0
      return(taz_shape)
    })
  })
  
  leafNum <- reactive({input$leafletMapNum})
  
  output$leafletPlot <- renderLeaflet({
    if(input$action > 0)
      map <- NULL
      
      if(leafNum() == 1)
        map <- createMilEmpLeaflet(taz_shape())
      else if(leafNum() == 2)
        map <- createMilGQLeaflet(taz_shape())
      else if(leafNum() == 3)
        map <- createSchoolKto8EnrollLeaflet(taz_shape())
      else if(leafNum() == 4)
        map <- createSchool9to12EnrollLeaflet(taz_shape())
      else if(leafNum() == 5)
        map <- createCollegeEnrollLeaflet(taz_shape())
      else if(leafNum() == 6)
        map <- createCollegeOtherEnrollLeaflet(taz_shape())
      else if(leafNum() == 7)
        map <- createCollegeStudentsLeaflet(taz_shape())
      else if(leafNum() == 8)
        map <- createPopDenLeaflet(taz_shape())
      else if(leafNum() == 9)
        map <- createAverageIncomeLeaflet(taz_shape())
      
    return(map)
    
    #if(leafNum() == 1)
    #  values[['leafletPlots']][[1]]
    #else if(leafNum() == 2)
    #  values[['leafletPlots']][[2]]
    #else if(leafNum() == 3)
    #  values[['leafletPlots']][[3]]
    #else if(leafNum() == 4)
    #  values[['leafletPlots']][[4]]
    #else if(leafNum() == 5)
    #  values[['leafletPlots']][[5]]
    #else if(leafNum() == 6)
    #  values[['leafletPlots']][[6]]
    #else if(leafNum() == 7)
    #  values[['leafletPlots']][[7]]
    #else if(leafNum() == 8)
    #  values[['leafletPlots']][[8]]
    #else if(leafNum() == 9)
    #  values[['leafletPlots']][[9]]
  })
  
  observe({
    input$reset_leaflet
    leafletProxy("leafletPlot") %>% setView(-116.88, 32.98, 9) 
  })

  #output$leafletPlot_milEmp <- renderLeaflet({
  #  if(input$action > 0)
  #    createMilEmpLeaflet(taz_shape())
  #})
  #
  #observe({
  #  input$reset_milEmp
  #  reset_leaflet_view("leafletPlot_milEmp")
  #})
  #
  #output$leafletPlot_milGQ <- renderLeaflet({
  #  if(input$action > 0)
  #    createMilGQLeaflet(taz_shape())
  #})
  #
  #observe({
  #  input$reset_milGQ
  #  reset_leaflet_view("leafletPlot_milGQ")
  #})
  #
  #output$leafletPlot_schoolEnroll <- renderLeaflet({
  #  if(input$action > 0)
  #    createSchoolEnrollLeaflet(taz_shape())
  #})
  #
  #observe({
  #  input$reset_schoolEnroll
  #  reset_leaflet_view("leafletPlot_schoolEnroll")
  #})
  #
  #output$leafletPlot_collegeEnroll <- renderLeaflet({
  #  if(input$action > 0)
  #    createCollegeEnrollLeaflet(taz_shape())
  #})
  #
  #observe({
  #  input$reset_collEnroll
  #  reset_leaflet_view("leafletPlot_collEnroll")
  #})
  #
  #output$leafletPlot_collegeStudent <- renderLeaflet({
  #  if(input$action > 0)
  #    createCollegeStudentsLeaflet(taz_shape())
  #})
  #
  #observe({
  #  input$reset_collStudent
  #  reset_leaflet_view("leafletPlot_collegeStudent")
  #})
  #
  #output$leafletPlot_averageIncome <- renderLeaflet({
  #  if(input$action > 0)
  #    createAverageIncomeLeaflet(taz_shape())
  #})
  #
  #observe({
  #  input$reset_avgInc
  #  reset_leaflet_view("leafletPlot_averageIncome")
  #})
  #
  #output$leafletPlot_popDensity <- renderLeaflet({
  #  if(input$action > 0)
  #    createPopDenLeaflet(taz_shape())
  #})
  #
  #observe({
  #  input$reset_popDen
  #  reset_leaflet_view("leafletPlot_popDensity")
  #})
  
  #----------------------------------------------#
  #----------- Service Bureau Comparison --------#
  #----------------------------------------------#
  getSBData <- function(){
    read_csv(paste(input_location(), "mgra13_based_input2012_sb.csv", sep = "/"))
  }
  
  getMGRAList <- function(){
    mgraList <- read_csv(paste(input_location(), "lu.csv", sep = "/"))
    mgraList <- as.list(unique(mgraList %>% select(mgra)))
    return(mgraList)
  }
  
  output$sbComparison <- renderDataTable({
    if(input$action > 0){
      if(is.null(input_data()))
        return()
      
      sb_data <- getSBData()
      mgra_list <- getMGRAList()
      sbCompareData <- compareSBData(sb_data, input_data(), mgra_list)
      
      datatable(sbCompareData, options = list("scrollX" = TRUE, "searching" = FALSE, "processing" = TRUE, "lengthMenu" = c(10,25,50,100)), rownames = FALSE)
    }
  })
  
  #----------------------------------------------#
  #---------------- Highway Data Checks ---------#
  #----------------------------------------------#
  output$missingCheck1 <- renderTable({
    if(input$action > 0){
      if(is.null(input_data()))
        return()
      as.data.frame(checkMissingAttribute("HWYCOV.ID", "Link ID", key, input_data()))
    }
  }, include.rownames=FALSE)
  
  output$missingCheck2 <- renderTable({
    if(input$action > 0){
      if(is.null(input_data()))
        return()
      as.data.frame(checkMissingAttribute("SPEED", "Link Speed", key, input_data()))
    }
  }, include.rownames=FALSE)
  
  output$missingCheck3 <- renderTable({
    if(input$action > 0){
      if(is.null(input_data()))
        return()
      as.data.frame(checkMissingAttribute("IFC", "Functional Classification", key, input_data()))
    }
  }, include.rownames=FALSE)
  
  output$missingCheck4 <- renderTable({
    if(input$action > 0){
      if(is.null(input_data()))
        return()
      attrNames <- c("HWYCOV.ID", "SPEED", "IFC")
      attrDescriptions <- c("Link ID", "Link Speed", "Functional Classification")
      
      as.data.frame(checkMissingAttributeNew(attrNames, attrDescriptions, key, input_data()))
    }
  }, include.rownames=FALSE)
  
  
  #----------------------------------------------#
  #-------------- Highway Network Data ----------#
  #----------------------------------------------#
  updateHwyDataForTable <- reactive({
    dataFilteredForTable <- input_data()
    if(input$hwycovid > 0) dataFilteredForTable <- dataFilteredForTable %>% filter(HWYCOV.ID == input$hwycovid)
    dataFilteredForTable
  })
  
  output$hwyData <- renderDataTable({
    if(input$action > 0){
      if(is.null(input_data()))
        return()
      dataForTable <- updateHwyDataForTable()
      datatable(dataForTable, options = list("scrollX" = TRUE, "searching" = FALSE, "processing" = TRUE, "lengthMenu" = c(10,25,50,100)), rownames = FALSE)
    }
  })
  
  output$test <- reactive({
    paste("value is ", input$hwycovid)
  })
  
  #----------------------------------------------#
  #---------------- Zone Connectors -------------#
  #----------------------------------------------#
  output$zoneConnectors <- renderDataTable({
    if(input$action > 0){
      if(is.null(input_data()))
        return()
      dataForTable <- input_data() %>% filter(IFC == 10)
      datatable(dataForTable, options = list("scrollX" = TRUE, "searching" = FALSE, "processing" = TRUE, "lengthMenu" = c(10,25,50,100)), rownames = FALSE)
    }
  })

})

