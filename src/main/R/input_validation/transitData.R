# Ashish Kulshrestha | kulshresthaa@pbworld.com | Parsons Brinckerhoff
# Last Edited: June 23, 2016
# Script for Transit Network Data

library(tcadr)

# get transit data -
getData <- function(path, buildNetwork){
  # build the network first, if checked by user
  if(buildNetwork == TRUE)
    buildTransitNetwork()
  
  # to test function 
  #path <- "C:/Projects/SANDAG/ABM Support, 2016/network_building"
  
  # file names
  
  # file names
  transit_routes_bin_file_name <- paste(path, "transitrtR.bin", sep = "/")
  transit_stops_bin_file_name <- paste(path, "transitrtS.bin", sep = "/")
  
  # read bin files
  routeData <- read_tcad(transit_routes_bin_file_name)
  stopData <- read_tcad(transit_stops_bin_file_name)
  
  data <- list(routeData, stopData)
  return(data)
}

buildTransitNetwork <- function(){
  TC_PATH <- "C:\\Program Files\\TransCAD 6.0"                                     # transcad path
  NETWORK_PATH <- "C:\\Projects\\SANDAG\\ABM Support, 2016\\network_building"      # save transit network here
  GISDK_CODE_PATH <- "C:\\Projects\\SANDAG\\ABM Support, 2016\\ABM\\src\\main\\gisdk" # path for gisdk resource files
  LSTFILE <- "sandag_abm_generic_ak.lst"                                           # gisdk listfile name
  MACRO_NAME <- "buildTransitNetwork"                                              # gisdk macro name to build network
  
  # compile rsc to create ui database
  str <- paste('"', paste(TC_PATH, "rscc.exe", sep = "\\"), '"', sep = "")
  str <- paste(str, "-c -u")
  str <- paste(str, paste('"', paste(NETWORK_PATH, "ui.dbd", sep = "\\"), '"', sep = ""))  
  str <- paste(str, paste('"', "@", paste(GISDK_CODE_PATH, LSTFILE, sep = "\\"), '"', sep = ""))
  system(str)
  
  # run macro to build transit network
  str <- paste('"', paste(TC_PATH, "tcw.exe", sep = "\\"), '"', sep = "")
  str <- paste(str, "-q -a")
  str <- paste(str, paste('"', paste(NETWORK_PATH, "ui.dbd", sep = "\\"), '"', sep = ""))
  str <- paste(str, "-ai", MACRO_NAME)
  system(str)  
}

