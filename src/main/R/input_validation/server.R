library(shiny)
library(shinydashboard)
library(DT)
library(rgdal)
library(tmap)
library(dplyr)
library(leaflet)
library(readr)
library(sqldf)

read_data <- function(type,  inputPath, buildNetwork){
  if(type == "Socioeconomic Data"){
    source("seData.R")
    #getData is a function (with argument as inputPath) in seData.R to read csv data file
    return(getData(inputPath))
  }
  else if (type == "Highway Network"){
    source("hwyData.R")
    #getData is a function (with argument as inputPath and flag to build network) in hwyData.R to read highway bin file
    return(getData(inputPath, buildNetwork)) 
  }
  else if (type == "Transit Network"){
    source("transitData.R")
    #getData is a function (with argument as inputPath and flag to build network) in transitData.R to read transit bin file
    return(getData(inputPath, buildNetwork)) 
  }
  else
    return(NULL)
}

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
    readDirectoryInput(session, 'inputDir')
    #return("C:\\Projects\\SANDAG\\ABM Support, 2016\\socioeconomic")
    #return("C:\\Projects\\SANDAG\\ABM Support, 2016\\network_building")
  })
  
  shapefile_location <- renderText({
    #readDirectoryInput(session, 'shapeFileDir')
    return("C:\\Projects\\SANDAG\\ABM Support, 2016\\shapefiles")
  })
  
  growthData_location <- renderText({
    return("C:\\Projects\\SANDAG\\ABM Support, 2016\\growthFiles")
  })
  
  build_network <- renderText({
    input$buildNetwork
  })
  
  input_data <- reactive({
    input$action
    isolate({
      read_data(input_type(), input_location(), build_network())
    })
  })

  output$mainTabs <- renderUI({
    if(input_type() == "") {
      
    }
    
    else if(input_type() == "Socioeconomic Data"){
      tabsetPanel(id = "mainTabs",
          tabPanel("Ratio Checks", value = 1, br(), br(),
                   tagList(
                     tags$head(
                       tags$link(rel="stylesheet", type="text/css",href="js/style.css"),
                       tags$script(type="text/javascript", src = "js/busy.js")
                     )
                   ),
                                          
                   conditionalPanel(paste("input.action > 0 &&", "$('html').hasClass('shiny-busy')"),
                     div(class = "busy",  
                         p("Calculation in progress ..."),
                         img(src="js/hourglass.gif")
                     )
                   ),
                   
                   htmlOutput("employmentRatioCheck"), br(), br(),
                   htmlOutput("enrollmentRatioCheck")
          ),
          
          tabPanel("Year By Year Growth", value = 2,
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
          
          tabPanel("Static Plots", value = 3,
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
          ),

          tabPanel("Interactive Plots", value = 4,
                   br(), selectInput("leafletMapNum", "Select Map:",
                                      choices = c("Military Employment" = 1, "Military Group Quarters" = 2,
                                                  "School Enrollment (K to 8)" = 3, "School Enrollment (9 to 12)" = 4,
                                                  "College Enrollment" = 5, "College (Other) Enrollment" = 6, "College Students" = 7,
                                                  "Population Density" = 8, "Average Income" = 9), width = "220px"),
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
                         ".leaflet .legend {line-height: 22px;}",
                         ".leaflet .leaflet-control-zoom-in {position: fixed; top: 400px;}",
                         ".leaflet .leaflet-control-zoom-out {position: fixed; top: 426px;}"
                       )
                     )
                   ),
                   
                   leafletOutput("leafletPlot", width = 800, height = 600), actionButton("reset_leaflet", "Reset view")
                   
          ),
          
          tabPanel("SB Comparison", value = 5, br(), dataTableOutput("sbComparison"))
      )
    }
    
    else if(input_type() == "Highway Network") {
      tabsetPanel(id = "mainTabs",
          tabPanel("Summary", value = 1, br(), br(),
                   tagList(
                     tags$head(
                       tags$link(rel="stylesheet", type="text/css",href="js/style2.css"),
                       tags$script(type="text/javascript", src = "js/busy.js")
                     )
                   ),
                   
                   conditionalPanel(paste("input.action > 0 &&", "$('html').hasClass('shiny-busy')"),
                                    br(), br(), br()
                   ),
                   
                   conditionalPanel(paste("input.action > 0 &&", "$('html').hasClass('shiny-busy')"),
                      div(class = "busy",  
                          br(), br(), br(),
                          p("Calculation in progress ..."),
                          img(src="js/hourglass.gif")
                      )
                   ),

                   fluidRow(
                     div(style="display:inline-block", class="col-lg-12",
                         box(title = "Lane Miles Summary (Functional Class)", solidHeader = TRUE, status = "info", width = 5, collapsible = TRUE, collapsed = TRUE,
                             tableOutput("laneMilesSummaryByFC"),
                             column(downloadButton("downloadLaneMilesSummaryByFC", "Save"), width = 12, align = 'right')
                         ),
                         box(title = "Lane Miles Summary (Jurisdiction)", solidHeader = TRUE, status = "info", width = 5, collapsible = TRUE, collapsed = TRUE,
                             tableOutput("laneMilesSummaryByJUR"),
                             column(downloadButton("downloadLaneMilesSummaryByJUR", "Save"), width = 12, align = 'right')
                         )
                     )
                   ),
                   
                   fluidRow(
                     div(style="display:inline-block", class="col-lg-12",
                         box(title = "Speed Summary", solidHeader = TRUE, status = "info", width = 5, collapsible = TRUE, collapsed = TRUE,
                             tableOutput("speedSummary"),
                             column(downloadButton("downloadSpeedSummary", "Save"), width = 12, align = 'right')
                         ),
                         box(title = "IFC and IHOV Summary", solidHeader = TRUE, status = "info", width = 5, collapsible = TRUE, collapsed = TRUE,
                             tableOutput("ifcihovTollSummary"),
                             column(downloadButton("downloadIFCIHOVTollSummary", "Save"), width = 12, align = 'right')
                         )
                     )
                   ),
                   
                   fluidRow(
                     div(style="display:inline-block", class="col-lg-12",
                         box(title = "Toll Summary (IFC)", solidHeader = TRUE, status = "info", width = 5, collapsible = TRUE, collapsed = TRUE,
                             tableOutput("ifcTollSummary"),
                             column(downloadButton("downloadIFCTollSummary", "Save"), width = 12, align = 'right')
                         ),
                         box(title = "Toll Summary (IHOV)", solidHeader = TRUE, status = "info", width = 5, collapsible = TRUE, collapsed = TRUE,
                             tableOutput("ihovTollSummary"),
                             column(downloadButton("downloadIHOVTollSummary", "Save"), width = 12, align = 'right')
                        )
                     )
                   ),              
                   
                   fluidRow(
                     div(style="display:inline-block", class="col-lg-12",
                         box(title = "No. of Lanes Summary", solidHeader = TRUE, status = "info", width = 10, collapsible = TRUE, collapsed = TRUE,
                             tableOutput("lanesSummary"),
                             column(downloadButton("downloadLanesSummary", "Save"), width = 12, align = 'right')
                         )
                     )
                   )
                   
                   
          ),
          
          tabPanel("Run Queries", value = 2, br(),
                   selectInput("hwyQuery", "Select Query:",
                               #choices = c("IFC = 1 and ISPD < 55", 
                               #            "(IFC > 1 and IFC < 8) and ISPD > 60",
                               #            "IFC = 10 and (ITOLLO > 0 or ITOLLA > 0 or ITOLLP > 0)",
                               #            "IHOV = 1 and (ITOLLO > 0 or ITOLLA > 0 or ITOLLP > 0)", 
                               #            "IFC = 1 and IHOV = 3 and (ITOLLO = 0 or ITOLLA = 0 or ITOLLP = 0)",
                               #            "(IFC > 1 and IFC < 8) and IHOV = 4",
                               #            "IFC > 1 and ((ABLNA > 5 or ABLNO > 5 or ABLNP > 5)or(BALNA > 5 or BALNO > 5 or BALNP > 5))"
                               #           ),
                               
                               choices = c("", "Freeway links with posted speed less than 55mph" = "IFC = 1 and ISPD < 55", 
                                           "Arterial links with posted speed more than 60mph" = "(IFC > 1 and IFC < 8) and ISPD > 60",
                                           "Centroid links with toll values" = "IFC = 10 and (ITOLLO > 0 or ITOLLA > 0 or ITOLLP > 0)",
                                           "Free links that have a toll value" = "IHOV = 1 and (ITOLLO > 0 or ITOLLA > 0 or ITOLLP > 0)", 
                                           "Managed lanes that have no toll values" = "IFC = 1 and IHOV = 3 and (ITOLLO = 0 or ITOLLA = 0 or ITOLLP = 0)",
                                           "Arterial links coded as toll links" = "(IFC > 1 and IFC < 8) and IHOV = 4",
                                           "Non freeway links with more than 5 lanes in any direction" = "IFC > 1 and ((ABLNA > 5 or ABLNO > 5 or ABLNP > 5)or(BALNA > 5 or BALNO > 5 or BALNP > 5))"
                               ),
                               
                               width = "400px"),
                   br(), dataTableOutput("hwyQueryData")       
          ),
          
          tabPanel("Highway Data", value = 3, br(),
                   tags$head(tags$style(type="text/css", ".span1 {display: inline-block; padding-right: 50px;}")),
                   div(class="span1",textInput("hwycovid", "SEARCH HWYCOVID(s):", "", width = "400px")),
                   div(class="span1",textInput("selCols", "SELECT COLUMN(s):", "", width = "400px")),
                   br(), dataTableOutput("hwyData")
          )
      )
    }
    
    else if(input_type() == "Transit Network") {
      tabsetPanel(id = "mainTabs",
                  tabPanel("Transit Routes", value = 1, br(), dataTableOutput("transitRoutesData")),
                  tabPanel("Transit Stops", value = 2, br(), dataTableOutput("transitStopsData"))
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
        
        
        if(ratio < employment.ratio.min.val | ratio > employment.ratio.max.val){
          str7 <- paste("<b>", "WARNING:", "</b>", "Ratio is NOT within the expected range of", employment.ratio.min.val, "and", employment.ratio.max.val)
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
        
        if(ratio < enrollment.ratio.min.val | ratio > enrollment.ratio.max.val){
          str7 <- paste("<b>", "WARNING:", "</b>", "Ratio is NOT within the expected range of", enrollment.ratio.min.val, "and", enrollment.ratio.max.val)
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
    if (!is.null(input_data()) && input_type() == "Socioeconomic Data")
      values[['spatialPlots']] <- getStaticPlots(input_data(), taz_shape())
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
  
  #-------------------------------------------------------#
  #------------- SE data year over year growth -----------#
  #-------------------------------------------------------#
  originCordon <- reactive({input$origin_cordon})
  cordonName <- reactive({input$cordon})
  
  getGrowthData <- function(){
    read_csv(paste(growthData_location(), growth.data.file.name, sep = "/"))
  }
  
  getExtExtGrowthData <- function(){
    read_csv(paste(growthData_location(), ext.ext.growth.data.file.name, sep = "/"))
  }
  
  getExtIntGrowthData <- function(){
    read_csv(paste(growthData_location(), ext.int.growth.data.file.name, sep = "/"))
  }
  
  getCordonDefinition <- function(){
    read_csv(paste(growthData_location(), cordon.definition.file.name, sep = "/"))
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
    growth_plots <- getGrowthPlots(growth_data)

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
      extExt_growth_plots[[i]] <- getExtExtGrowthPlot(subset_data, originCordon(), d)
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
        getExtIntGrowthPlot(subset_data, cordonName())
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
  })
  
  observe({
    input$reset_leaflet
    leafletProxy("leafletPlot") %>% setView(-116.88, 32.98, 9) 
  })

  #----------------------------------------------#
  #----------- Service Bureau Comparison --------#
  #----------------------------------------------#
  getSBData <- function(){
    read_csv(paste(input_location(), sb.mgra.data.file.name, sep = "/"))
  }
  
  getMGRAList <- function(){
    mgraList <- read_csv(paste(input_location(), sb.mgra.list.file.name, sep = "/"))
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
      
      datatable(sbCompareData, options = list("scrollX" = TRUE, "searching" = FALSE, "processing" = TRUE, "lengthMenu" = c(9,24,60,90)), rownames = FALSE)
    }
  })  
  
  #----------------------------------------------#
  #-------------- Highway Network Data ----------#
  #----------------------------------------------#
  
  updateHwyDataForTable <- reactive({
    dataFilteredForTable <- input_data()
    if(!(input$hwycovid == "")){
      selectIDs <- gsub(" ", "", input$hwycovid)
      selectIDs <- unlist(strsplit(selectIDs, ","))
      
      dataFilteredForTable <- dataFilteredForTable %>% filter(HWYCOV.ID %in% selectIDs)
    }
    
    if(!(input$selCols == "")){
      selectColumns <- gsub(" ", "", input$selCols)
      selectColumns <- unlist(strsplit(selectColumns, ","))
      dataFilteredForTable <- dataFilteredForTable[, c(hwy.key, selectColumns)]
    }
    
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
  
  #----------------------------------------------#
  #---------- Highway Network Summaries ---------#
  #----------------------------------------------#
  
  output$laneMilesSummaryByFC <- renderTable({
    if(input$action > 0){
      if(is.null(input_data()))
        return()
      dataForTable <- createLaneMileSummaryByFC(input_data())
      dataForTable
    }
  }, include.rownames=FALSE, align='rrlrr')
  
  output$downloadLaneMilesSummaryByFC = downloadHandler(
    filename = 'LaneMilesByIFC.csv',
    content = function(file) {
      write.csv(as.data.frame(createLaneMileSummaryByFC(input_data())), file, row.names = F)
    }
  )
  
  output$laneMilesSummaryByJUR <- renderTable({
    if(input$action > 0){
      if(is.null(input_data()))
        return()
      createLaneMileSummaryByJUR(input_data())
    }
  }, include.rownames=FALSE, align='rrlrr')
  
  output$speedSummary <- renderTable({
    if(input$action > 0){
      if(is.null(input_data()))
        return()
      createSpeedSummary(input_data())
    }
  }, include.rownames=FALSE, align='rrlrr')
  
  output$lanesSummary <- renderTable({
    if(input$action > 0){
      if(is.null(input_data()))
        return()
      createLanesSummary(input_data())
    }
  }, include.rownames=FALSE, align='rrlrrrrrr')
  
  output$ihovTollSummary <- renderTable({
    if(input$action > 0){
      if(is.null(input_data()))
        return()
      createIHOVTollSummary(input_data())
    }
  }, include.rownames=FALSE, align='rrrrrrrr')
  
  output$ifcihovTollSummary <- renderTable({
    if(input$action > 0){
      if(is.null(input_data()))
        return()
      createIFCIHOVTollSummary(input_data())
    }
  }, include.rownames=FALSE, align='rrlrrrr')
  
  output$ifcTollSummary <- renderTable({
    if(input$action > 0){
      if(is.null(input_data()))
        return()
      createIFCTollSummary(input_data())
    }
  }, include.rownames=FALSE, align='rrrrrrrr')
  
  
  #----------------------------------------------#
  #------------- Highway Network Query ----------#
  #----------------------------------------------#
  
  getHwyQueryData <- reactive({
    qdata <- input_data()
    qdata <- qdata %>% select(HWYCOV.ID, IFC, IHOV, ISPD, ITOLLO, ITOLLA, ITOLLP, ABLNO, ABLNA, ABLNP, BALNO, BALNA, BALNP)
    create_query <- paste("SELECT * FROM qdata WHERE (", input$hwyQuery, ")", sep = "")
    queryData <- sqldf(create_query)
    queryData
  })
  
  output$hwyQueryData <- renderDataTable({
    if(input$action > 0){
      if(is.null(input_data()))
        return()
      if(input$hwyQuery == "")
        return()
      dataForTable <- getHwyQueryData()
      datatable(dataForTable, options = list("scrollX" = TRUE, "searching" = FALSE, "processing" = TRUE, "lengthMenu" = c(10,25,50,100)), rownames = FALSE)
    }
  })

  #----------------------------------------------#
  #-------------- Transit Network Data ----------#
  #----------------------------------------------#
  output$transitRoutesData <- renderDataTable({
    if(input$action > 0){
      if(is.null(input_data()))
        return()
      dataForTable <- input_data()[[1]]
      datatable(dataForTable, options = list("scrollX" = TRUE, "searching" = FALSE, "processing" = TRUE, "lengthMenu" = c(10,25,50,100)), rownames = FALSE)
    }
  })

  output$transitStopsData <- renderDataTable({
    if(input$action > 0){
      if(is.null(input_data()))
        return()
      dataForTable <- input_data()[[2]]
      datatable(dataForTable, options = list("scrollX" = TRUE, "searching" = FALSE, "columnDefs" = list(list(width = "5%", targets = 0)), "processing" = TRUE, "lengthMenu" = c(10,25,50,100)), rownames = FALSE)
    }
  })
})