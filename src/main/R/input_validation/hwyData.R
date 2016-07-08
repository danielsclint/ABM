# Ashish Kulshrestha | kulshresthaa@pbworld.com | Parsons Brinckerhoff
# Last Edited: Jun 25, 2016
# Script for Highway Network Data Manipulation and Summaries

library(tcadr)
library(reshape2)

# get highway data - read hwy.bin file
getData <- function(path, buildNetwork){
  # build the network first, if checked by user
  if(buildNetwork == TRUE)
    buildHighwayNetwork()
  
  # to test function 
  #path <- "C:/Projects/SANDAG/ABM Support, 2016/network_building"
  
  # file names
  hwy_bin_file_name <- paste(path, "hwy.bin", sep = "/")

  # read bin file
  data <- read_tcad(hwy_bin_file_name)
  
  return(data)
}

buildHighwayNetwork <- function(){
  TC_PATH <- "C:\\Program Files\\TransCAD 6.0"                                     # transcad path
  NETWORK_PATH <- "C:\\Projects\\SANDAG\\ABM Support, 2016\\network_building"      # save highway network here
  GISDK_CODE_PATH <- "C:\\Projects\\SANDAG\\ABM Support, 2016\\ABM\\src\\main\\gisdk" # path for gisdk resource files
  LSTFILE <- "sandag_abm_generic_ak.lst"                                           # gisdk listfile name
  MACRO_NAME <- "buildHwyNetwork"                                                  # gisdk macro name to build network
  
  # compile rsc to create ui database
  str <- paste('"', paste(TC_PATH, "rscc.exe", sep = "\\"), '"', sep = "")
  str <- paste(str, "-c -u")
  str <- paste(str, paste('"', paste(NETWORK_PATH, "ui.dbd", sep = "\\"), '"', sep = ""))  
  str <- paste(str, paste('"', "@", paste(GISDK_CODE_PATH, LSTFILE, sep = "\\"), '"', sep = ""))
  system(str)
  
  # run macro to build highway network
  str <- paste('"', paste(TC_PATH, "tcw.exe", sep = "\\"), '"', sep = "")
  str <- paste(str, "-q -a")
  str <- paste(str, paste('"', paste(NETWORK_PATH, "ui.dbd", sep = "\\"), '"', sep = ""))
  str <- paste(str, "-ai", MACRO_NAME)
  system(str)  
}

checkUnconnectedZones <- function(zones, inputData){
  zoneList <- c(1:zones)
  
  #filter drive alone free links
  driveAloneFreeLinks <- inputData %>% filter(FFC == 0)
  
  centroidsInHwyData <- driveAloneFreeLinks %>% filter(AN <= zones) %>% select(AN)
  centroidsInHwyData <- unique(centroidsInHwyData$AN)
  
  if(length(centroidsInHwyData) == zones) return(HTML("ALL OK."))
  else{
    #unconnected_zones <- c(5001:5020)
    unconnected_zones <- zoneList[!(zoneList %in% centroidsInHwyData)]
    return(HTML(paste("Zone unconected via drive alone free facilities: ", paste(unconnected_zones, collapse = ", "), sep = '<br/>')))
  }
}

createLaneMileSummaryByFC <- function(hwyData){
  hwyData$LANES <- sum(hwyData$ABLNO, hwyData$BALNO)
  hwyData$LaneMiles <- hwyData$LANES * hwyData$Length.1
  data <- hwyData %>% group_by(IFC) %>% summarise(LaneMiles = round(sum(LaneMiles),2))
  data <- data %>% mutate(Pct = round(100*LaneMiles/sum(LaneMiles),2))
  data$LaneMiles <- prettyNum(data$LaneMiles, big.mark=",")
  names(data) <- c("IFC", "Lane_Miles", "Percent")
  ifc_def <- read.csv("C:\\Projects\\SANDAG\\ABM Support, 2016\\inputs\\IFC_Definition.csv")
  data <- left_join(data, ifc_def, by=c("IFC"))
  data <- subset(data, select=c(1,4,2:3))
  return(data)
}

createLaneMileSummaryByJUR <- function(hwyData){
  hwyData$LANES <- sum(hwyData$ABLNO, hwyData$BALNO)
  hwyData$LaneMiles <- hwyData$LANES * hwyData$Length.1
  data <- hwyData %>% group_by(IJUR) %>% summarise(LaneMiles = round(sum(LaneMiles),2))
  data <- data %>% mutate(Pct = round(100*LaneMiles/sum(LaneMiles),2))
  data$LaneMiles <- prettyNum(data$LaneMiles, big.mark=",")
  names(data) <- c("IJUR", "Lane_Miles", "Percent")
  ijur_def <- read.csv("C:\\Projects\\SANDAG\\ABM Support, 2016\\inputs\\IJUR_Definition.csv")
  data <- left_join(data, ijur_def, by=c("IJUR"))
  data <- subset(data, select=c(1,4,2:3))
  return(data)
}

createSpeedSummary <- function(hwyData){
  data <- select(hwyData, IFC, ISPD)
  data <- data %>% group_by(IFC) %>% summarise(min = min(ISPD), max = max(ISPD))
  names(data) <- c("IFC", "Min_Speed", "Max_Speed")
  ifc_def <- read.csv("C:\\Projects\\SANDAG\\ABM Support, 2016\\inputs\\IFC_Definition.csv")
  data <- left_join(data, ifc_def, by=c("IFC"))
  data <- subset(data, select=c(1,4,2:3))
  return(data)
}

createLanesSummary <- function(hwyData){
  data <- select(hwyData, IFC, ABLNA, ABLNO, ABLNP, BALNA, BALNO, BALNP)
  data <- data %>% group_by(IFC) %>% summarise(maxABA = max(ABLNA), maxABO = max(ABLNO), maxABP = max(ABLNP), maxBAA = max(BALNA), maxBAO = max(BALNO), maxBAP = max(BALNP))
  names(data) <- c("IFC", "Max AB Lanes (AM)", "Max AB Lanes (OP)", "Max AB Lanes (PM)", "Max BA Lanes (AM)", "Max BA Lanes (OP)", "Max BA Lanes (PM)")
  ifc_def <- read.csv("C:\\Projects\\SANDAG\\ABM Support, 2016\\inputs\\IFC_Definition.csv")
  data <- left_join(data, ifc_def, by=c("IFC"))
  data <- subset(data, select=c(1,8,2:7))
  return(data)
}

createIHOVTollSummary <- function(hwyData){
  data <- select(hwyData, IHOV, ITOLLO, ITOLLA, ITOLLP)
  data <- data %>% group_by(IHOV) %>% summarise(mino = min(ITOLLO), maxo = max(ITOLLO), mina = min(ITOLLA), maxa = max(ITOLLA), minp = min(ITOLLP), maxp = max(ITOLLP))
  names(data) <- c("IHOV", "Min Toll (OP)", "Max Toll (OP)", "Min Toll (AM)", "Max Toll (AM)", "Min Toll (PM)", "Max Toll (PM)")
  return(data)
}

createIFCIHOVTollSummary <- function(hwyData){
  data <- select(hwyData, IFC, IHOV)
  data <- data %>% group_by(IFC, IHOV) %>% summarise(count = n())
  data <- recast(data, IFC ~ IHOV + variable, id.var=c("IFC", "IHOV"))
  data[is.na(data)] <- 0
  names(data) <- c("IFC", "IHOV_1", "IHOV_2", "IHOV_3", "IHOV_4")
  data$IHOV_1<- prettyNum(data$IHOV_1, digits = 0)
  data$IHOV_2<- prettyNum(data$IHOV_2, digits = 0)
  data$IHOV_3<- prettyNum(data$IHOV_3, digits = 0)
  data$IHOV_4<- prettyNum(data$IHOV_4, digits = 0)
  ifc_def <- read.csv("C:\\Projects\\SANDAG\\ABM Support, 2016\\inputs\\IFC_Definition.csv")
  data <- left_join(data, ifc_def, by=c("IFC"))
  data <- subset(data, select=c(1,6,2:5))
  return(data)
}

createIFCTollSummary <- function(hwyData){
  data <- select(hwyData, IFC, ITOLLO, ITOLLA, ITOLLP)
  data <- data %>% group_by(IFC) %>% summarise(mino = min(ITOLLO), maxo = max(ITOLLO), mina = min(ITOLLA), maxa = max(ITOLLA), minp = min(ITOLLP), maxp = max(ITOLLP))
  names(data) <- c("IFC", "Min Toll (OP)", "Max Toll (OP)", "Min Toll (AM)", "Max Toll (AM)", "Min Toll (PM)", "Max Toll (PM)")
  return(data)
}
