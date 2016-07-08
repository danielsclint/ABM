# Ashish Kulshrestha | kulshresthaa@pbworld.com | Parsons Brinckerhoff
# Last Edited: June 27, 2016
# Script for manipulating/plotting socioeconomic data

library(readr)
library(ggplot2)
library(RColorBrewer)

# get se data - merging mgra, person and hh files 
getData <- function(path){
  # to test function 
  #path <- "C:/Projects/SANDAG/ABM Support, 2016/socioeconomic"
  
  # file names
  mgra_file <- paste(path, "mgra13_based_input2012.csv", sep = "/")
  per_file <- paste(path, "persons.csv", sep = "/")
  hh_file <- paste(path, "households.csv", sep = "/")
  acreage_file <- paste(path, "mgra_acreage.csv", sep = "/")
  
  # read data
  mgra_data <- read_csv(mgra_file)
  per_data <- read_csv(per_file)
  hh_data <- read_csv(hh_file)
  acreage_data <- read_csv(acreage_file)
  
  pop_data <- per_data %>% mutate(COLLEGESTUDENT = ifelse(PSTUDENT==2, 1, 0), SCHOOLSTUDENT = ifelse(PSTUDENT==1, 1, 0))
  
  pop_data <- pop_data %>% group_by(HHID) %>%
                summarise(COLLEGESTUDENT = sum(COLLEGESTUDENT), SCHOOLSTUDENT = sum(SCHOOLSTUDENT)) %>%
                  select(HHID, COLLEGESTUDENT, SCHOOLSTUDENT)
  
  pop_data <- left_join(pop_data, hh_data, by = "HHID") %>% select(MGRA, HWORKERS, COLLEGESTUDENT, SCHOOLSTUDENT)

  pop_data <- pop_data %>% group_by(MGRA) %>% summarise(workers = sum(HWORKERS), college_students = sum(COLLEGESTUDENT), school_students = sum(SCHOOLSTUDENT))
  
  inc_data <- hh_data %>% filter(UNITTYPE == 0) %>% group_by(MGRA) %>% summarise(income = mean(HINC))
  
  data <- left_join(mgra_data, inc_data, by = c("mgra"= "MGRA"))
  data <- left_join(data, pop_data, by = c("mgra"= "MGRA"))
  data <- left_join(data, acreage_data, by = c("mgra"= "MGRA"))
  
  data[is.na(data)] <- 0
  
  rm(mgra_data, hh_data, per_data, acreage_data, pop_data, inc_data)
  return(data)
}

# create the required plots (static)
getStaticPlots <- function(inputData, taz_shape){
  # to test function 
  #mgra_shape <- readOGR("C:/Projects/SANDAG/Military_Trips/inputs/shapefiles/mgra/mgra_13.shp", layer = "mgra_13")
  #mgra_shape@data <- left_join(mgra_shape@data, data, by = c("MGRA" = "mgra"))
  
  mgra_shape <- taz_shape
  #mgra_shape <- readOGR(paste(shapeFilePath, "mgra_13.shp", sep = "/"), layer = "mgra_13", verbose = FALSE)
  #mgra_shape@data <- left_join(mgra_shape@data, inputData, by = c("MGRA" = "mgra"))
  
  #mgra_shape$mil_emp_cat <- cut(mgra_shape$emp_fed_mil, c(0,1,1000,5000,10000,50000), labels=c("None", "< 1000", "1000 - 5000", "5000 - 10000", "> 10000"), include.lowest=FALSE, right=FALSE)
  #P1 <- tm_shape(mgra_shape) + tm_fill("mil_emp_cat", title = "Employment", palette='YlOrBr')
  #P1 <- P1 + tm_layout(title = "Military Employment", title.size = 1.8, title.position = c("center", "top"), legend.title.size = 1.2, legend.text.size = 1.2, legend.position = c("left", "bottom"))
  #P1 <- P1 + tm_compass(type = "4star", size = 2, show.labels = 2, position = c("right", "top"))
  
  mgra_shape$mil_emp_density <- ifelse(mgra_shape$acres > 0, mgra_shape$emp_fed_mil/mgra_shape$acres, 0)
  mgra_shape$mil_emp_cat <- cut(mgra_shape$mil_emp_density, c(0,0.0000001,1,5,10,20,1000), labels=c("None", "< 1.0", "1.0 - 5.0", "5.0 - 10.0", "10.0 - 20.0", "> 20.0"), include.lowest=FALSE, right=FALSE)
  pal <- c("#F1F1F1", brewer.pal(5, "YlOrBr"))
  P1 <- tm_shape(mgra_shape) + tm_fill("mil_emp_cat", title = "Employment / Acre", palette=pal)
  P1 <- P1 + tm_layout(title = "Military Employment", title.size = 1.8, title.position = c("center", "top"), legend.title.size = 1.2, legend.text.size = 1.2, legend.position = c("left", "bottom"))
  P1 <- P1 + tm_compass(type = "4star", size = 2, show.labels = 2, position = c("right", "top"))
  
  mgra_shape$mil_gq_cat <- cut(mgra_shape$gq_mil, c(0,1,4000,6000,10000,50000), labels=c("None", "< 4000", "4000 - 6000", "6000 - 10000", "> 10000"), include.lowest=FALSE, right=FALSE)
  pal <- c("#F1F1F1", brewer.pal(4, "YlOrBr"))
  P2 <- tm_shape(mgra_shape) + tm_fill("mil_gq_cat", title = "GQ", palette=pal)
  P2 <- P2 + tm_layout(title = "Military Group Quarters", title.size = 1.8, title.position = c("center", "top"), legend.title.size = 1.2, legend.text.size = 1.2, legend.position = c("left", "bottom"))
  P2 <- P2 + tm_compass(type = "4star", size = 2, show.labels = 2, position = c("right", "top"))
  
  mgra_shape$enroll_kto8_cat <- cut(mgra_shape$EnrollGradeKto8, c(0,1,300,800,1000,50000), labels=c("None", "< 300", "300 - 800", "800 - 1000", "> 1000"), include.lowest=FALSE, right=FALSE)
  pal <- c("#F1F1F1", brewer.pal(4, "YlOrBr"))
  P3 <- tm_shape(mgra_shape) + tm_fill("enroll_kto8_cat", title = "Enrollment", palette=pal)
  P3 <- P3 + tm_layout(title = "School Enrollment (K to 8)", title.size = 1.8, title.position = c("center", "top"), legend.title.size = 1.20, legend.text.size = 1.2, legend.position = c("left", "bottom"))
  P3 <- P3 + tm_compass(type = "4star", size = 2, show.labels = 2, position = c("right", "top"))
  
  mgra_shape$enroll_9to12_cat <- cut(mgra_shape$EnrollGrade9to12, c(0,1,200,500,1000,2000,50000), labels=c("None", "< 200", "200 - 500", "500 - 1000", "1000 - 2000", "> 2000"), include.lowest=FALSE, right=FALSE)
  pal <- c("#F1F1F1", brewer.pal(5, "YlOrBr"))
  P4 <- tm_shape(mgra_shape) + tm_fill("enroll_9to12_cat", title = "Enrollment", palette=pal)
  P4 <- P4 + tm_layout(title = "School Enrollment (9 to 12)", title.size = 1.8, title.position = c("center", "top"), legend.title.size = 1.2, legend.text.size = 1.2, legend.position = c("left", "bottom"))
  P4 <- P4 + tm_compass(type = "4star", size = 2, show.labels = 2, position = c("right", "top"))
  
  mgra_shape$enroll_college_cat <- cut(mgra_shape$collegeEnroll, c(0,1,5000,50000), labels=c("None", "< 5000", ">= 5000"), include.lowest=FALSE, right=FALSE)
  pal <- c("#F1F1F1", brewer.pal(2, "YlOrBr"))
  P5 <- tm_shape(mgra_shape) + tm_fill("enroll_college_cat", title = "Enrollment", palette=pal)
  P5 <- P5 + tm_layout(title = "College Enrollment", title.size = 1.8, title.position = c("center", "top"), legend.title.size = 1.2, legend.text.size = 1.2, legend.position = c("left", "bottom"))
  P5 <- P5 + tm_compass(type = "4star", size = 2, show.labels = 2, position = c("right", "top"))
  
  mgra_shape$enroll_other_college_cat <- cut(mgra_shape$otherCollegeEnroll, c(0,1,5000,50000), labels=c("None", "< 5000", ">= 5000"), include.lowest=FALSE, right=FALSE)
  P6 <- tm_shape(mgra_shape) + tm_fill("enroll_other_college_cat", title = "Enrollment", palette=pal)
  P6 <- P6 + tm_layout(title = "College (Other) Enrollment", title.size = 1.8, title.position = c("center", "top"), legend.title.size = 1.2, legend.text.size = 1.2, legend.position = c("left", "bottom"))
  P6 <- P6 + tm_compass(type = "4star", size = 2, show.labels = 2, position = c("right", "top"))
  
  mgra_shape$college_student_cat <- cut(mgra_shape$college_students, c(0,1,100,500,1000,50000), labels=c("None", "< 100", "100 - 500", "500 - 1000", " > 1000"), include.lowest=FALSE, right=FALSE)
  pal <- c("#F1F1F1", brewer.pal(4, "YlOrBr"))
  P7 <- tm_shape(mgra_shape) + tm_fill("college_student_cat", title = "College Students", palette=pal)
  P7 <- P7 + tm_layout(title = "College Students", title.size = 1.8, title.position = c("center", "top"), legend.title.size = 1.2, legend.text.size = 1.2, legend.position = c("left", "bottom"))
  P7 <- P7 + tm_compass(type = "4star", size = 2, show.labels = 2, position = c("right", "top"))
  
  mgra_shape$pop_density_cat <- cut(mgra_shape$PopDen, c(0,0.00001,10,20,40,1000), labels=c("None", "< 10", "10 - 20", "20 - 40", "> 40"), include.lowest=FALSE, right=FALSE)
  pal <- c("#F1F1F1", brewer.pal(4, "YlOrBr"))
  P8 <- tm_shape(mgra_shape) + tm_fill("pop_density_cat", title = "Density", palette=pal)
  P8 <- P8 + tm_layout(title = "Population Density", title.size = 1.8, title.position = c("center", "top"), legend.title.size = 1.2, legend.text.size = 1.2, legend.position = c("left", "bottom"))
  P8 <- P8 + tm_compass(type = "4star", size = 2, show.labels = 2, position = c("right", "top"))
  
  mgra_shape$income_cat <- cut(mgra_shape$income, c(0,1,30000,60000,100000,150000,10000000), labels=c("None", "< 30K", "30K - 60K", "60K - 100K", "100K - 150K", "> 150K"), include.lowest=FALSE, right=FALSE)
  pal <- c("#F1F1F1", brewer.pal(5, "YlOrBr"))
  P9 <- tm_shape(mgra_shape) + tm_fill("income_cat", title = "Income", palette=pal)
  P9 <- P9 + tm_layout(title = "Average Income", title.size = 1.8, title.position = c("center", "top"), legend.title.size = 1.2, legend.text.size = 1.2, legend.position = c("left", "bottom"))
  P9 <- P9 + tm_compass(type = "4star", size = 2, show.labels = 2, position = c("right", "top"))
  
  return(list(P1, P2, P3, P4, P5, P6, P7, P8, P9))
}

createTazData <- function(mgraData){
  tazData <- mgraData %>% group_by(TAZ) %>% summarise(emp_fed_mil = sum(emp_fed_mil), emp_total = round(sum(emp_total),0),
                                                      gq_mil = sum(gq_mil), acres = sum(acres),
                                                      EnrollGradeKto8 = sum(EnrollGradeKto8), EnrollGrade9to12 = sum(EnrollGrade9to12),
                                                      collegeEnroll = sum(collegeEnroll), otherCollegeEnroll = sum(otherCollegeEnroll),
                                                      college_students = sum(college_students), income = round(mean(income),0),
                                                      PopDen = round(weighted.mean(PopDen, pop),3)
  )
  tazData[is.na(tazData)] <- 0
  
  return(tazData)
}

compareSBData <- function(sb_data, base_data, mgra_list){
  i <- 1
  for(m in mgra_list){
    b_data <- base_data %>% filter(mgra %in% m) %>% mutate(data = "BASE")    # base data for that mgra
    s_data <- sb_data %>% filter(mgra %in% m) %>% mutate(data = "SB")        # sb data for that mgra
    
    baseSelectFields <- c("mgra", "data", "hh", "hhp", "gq_civ", "gq_mil", "emp_total", "EnrollGradeKto8", "EnrollGrade9to12", "collegeEnroll", "HotelRoomTotal")
    sbSelectFields <- c("mgra", "data", "hh", "hhp", "gq_civ", "gq_mil", "emp_total", "enrollgradekto8", "enrollgrade9to12", "collegeenroll", "hotelroomtotal")
    
    b_data <- b_data[,baseSelectFields]
    s_data <- s_data[,sbSelectFields]
    
    names(b_data) <- c("MGRA", "DATA", "HH", "POP_REG", "POP_GQ_CIV", "POP_GQ_MIL", "EMP", "ENROLL_KTO8", "ENROLL_9TO12", "ENROLL_COLL", "HOTEL_ROOMS")
    names(s_data) <- c("MGRA", "DATA", "HH", "POP_REG", "POP_GQ_CIV", "POP_GQ_MIL", "EMP", "ENROLL_KTO8", "ENROLL_9TO12", "ENROLL_COLL", "HOTEL_ROOMS")
    
    d_data <- cbind(data.frame("MGRA" = m, DATA = "DIFF"), data.frame(s_data[3:11]-b_data[3:11]))
    names(d_data) <- c("MGRA", "DATA", "HH", "POP_REG", "POP_GQ_CIV", "POP_GQ_MIL", "EMP", "ENROLL_KTO8", "ENROLL_9TO12", "ENROLL_COLL", "HOTEL_ROOMS")
    if(i==1) data <- rbind(b_data, s_data, d_data)
    if(i>1) data <- rbind(data, b_data, s_data, d_data)
    i <- i + 1
  }
  data$EMP <- round(data$EMP, 0)
  data <- data[order(data$MGRA),]
  return(data)
}

createMilEmpLeaflet <- function(taz_shape){
  taz_popup <- paste0("<strong>TAZ: </strong>", taz_shape$TAZ,  
                      "<br><strong>Military Emp: </strong>", taz_shape$emp_fed_mil,
                      "<br><strong>Total Emp: </strong>", taz_shape$emp_total)
  pal <- c("#F1F1F1", brewer.pal(5, "YlOrBr"))
  opts <- providerTileOptions(opacity = 0)
  
  taz_shape$mil_emp_density <- ifelse(taz_shape$acres > 0, taz_shape$emp_fed_mil/taz_shape$acres, 0)
  taz_shape$mil_emp_cat <- cut(taz_shape$mil_emp_density, c(0,0.0000001,1,5,10,20,1000), labels=c("None", "< 1.0", "1.0 - 5.0", "5.0 - 10.0", "10.0 - 20.0", "> 20.0"), include.lowest=FALSE, right=FALSE)
  
  taz_shape@data <- taz_shape@data %>% select(OBJECTID, TAZ, mil_emp_cat)
  map <- leaflet(taz_shape) %>% addProviderTiles("CartoDB.PositronNoLabels", options = opts)
  map <- map %>% addPolygons(fillColor = ~colorFactor(pal, taz_shape$mil_emp_cat)(mil_emp_cat), fillOpacity = 0.8, 
                             weight = 0.5, popup = taz_popup, color = "#d7d7db")
  
  map <- map %>% addLegend("bottomleft", title = "Employment/Acre", pal = colorFactor(pal, NULL), values = ~mil_emp_cat)
  map <- map %>% setView(-116.88, 32.98, 9)
  map
}

createMilGQLeaflet <- function(taz_shape){
  taz_popup <- paste0("<strong>TAZ: </strong>", taz_shape$TAZ, 
                      "<br><strong>Military GQ: </strong>", taz_shape$gq_mil)
  pal <- c("#F1F1F1", brewer.pal(4, "YlOrBr"))
  opts <- providerTileOptions(opacity = 0)
  
  taz_shape$mil_gq_cat <- cut(taz_shape$gq_mil, c(0,1,4000,6000,10000,50000), labels=c("None", "< 4000", "4000 - 6000", "6000 - 10000", "> 10000"), include.lowest=FALSE, right=FALSE)
  taz_shape@data <- taz_shape@data %>% select(OBJECTID, TAZ, mil_gq_cat)
  #CartoDB.PositronNoLabels, 
  map <- leaflet(taz_shape) %>% addProviderTiles("CartoDB.PositronNoLabels", options = opts)
  map <- map %>% addPolygons(fillColor = ~colorFactor(pal, taz_shape$mil_gq_cat)(mil_gq_cat), fillOpacity = 0.8, 
                             weight = 0.5, popup = taz_popup, color = "#d7d7db")
  
  map <- map %>% addLegend("bottomleft", title = "Military GQ", pal = colorFactor(pal, NULL), values = ~mil_gq_cat)
  map <- map %>% setView(-116.88, 32.98, 9)
  #map <- map %>% fitBounds(-116.014252, 32.520691, -117.714043, 33.462219)
  #map <- map %>% setMaxBounds(-116.014252, 32.520691, -117.714043, 33.462219)
  map
}

createSchoolEnrollLeaflet <- function(taz_shape){
  taz_popup <- paste0("<strong>TAZ: </strong>", taz_shape$TAZ, 
                      "<br><strong>Enrollment (K to 8): </strong>", taz_shape$EnrollGradeKto8,
                      "<br><strong>Enrollment (9 to 12): </strong>", taz_shape$EnrollGrade9to12)
  pal <- c("#F1F1F1", brewer.pal(4, "YlOrBr"))
  opts <- providerTileOptions(opacity = 0)
  
  taz_shape$enroll_kto8_cat <- cut(taz_shape$EnrollGradeKto8, c(0,1,300,800,1000,50000), labels=c("None", "< 300", "300 - 800", "800 - 1000", "> 1000"), include.lowest=FALSE, right=FALSE)
  taz_shape$enroll_9to12_cat <- cut(taz_shape$EnrollGrade9to12, c(0,1,300,800,1000,50000), labels=c("None", "< 300", "300 - 800", "800 - 1000", "> 1000"), include.lowest=FALSE, right=FALSE)
  taz_shape@data <- taz_shape@data %>% select(OBJECTID, TAZ, enroll_kto8_cat, enroll_9to12_cat)
  
  map <- leaflet(taz_shape) %>% addProviderTiles("CartoDB.PositronNoLabels", options = opts)
  map <- map %>% addPolygons(fillColor = ~colorFactor(pal, taz_shape$enroll_kto8_cat)(enroll_kto8_cat), fillOpacity = 0.8, 
                             weight = 0.5, popup = taz_popup, color = "#d7d7db", group = "K to 8")
  
  map <- map %>% addPolygons(fillColor = ~colorFactor(pal, taz_shape$enroll_9to12_cat)(enroll_9to12_cat), fillOpacity = 0.8, 
                             weight = 0.5, popup = taz_popup, color = "#d7d7db", group = "9 to 12")
  
  map <- map %>% addLegend("bottomleft", title = "Enrollment", pal = colorFactor(pal, NULL), values = ~enroll_kto8_cat)
  
  map <- map %>% addLayersControl(overlayGroups = c("K to 8", "9 to 12"), position = c("topright"), options = layersControlOptions(collapsed = FALSE))
  map <- map %>% hideGroup("9 to 12")
  
  map <- map %>% setView(-116.88, 32.98, 9)
  map
}


createSchoolKto8EnrollLeaflet <- function(taz_shape){
  taz_popup <- paste0("<strong>TAZ: </strong>", taz_shape$TAZ, 
                      "<br><strong>Enrollment (K to 8): </strong>", taz_shape$EnrollGradeKto8)
  pal <- c("#F1F1F1", brewer.pal(4, "YlOrBr"))
  opts <- providerTileOptions(opacity = 0)
  
  taz_shape$enroll_kto8_cat <- cut(taz_shape$EnrollGradeKto8, c(0,1,300,800,1000,50000), labels=c("None", "< 300", "300 - 800", "800 - 1000", "> 1000"), include.lowest=FALSE, right=FALSE)
  taz_shape@data <- taz_shape@data %>% select(OBJECTID, TAZ, enroll_kto8_cat)
  
  map <- leaflet(taz_shape) %>% addProviderTiles("CartoDB.PositronNoLabels", options = opts)
  map <- map %>% addPolygons(fillColor = ~colorFactor(pal, taz_shape$enroll_kto8_cat)(enroll_kto8_cat), fillOpacity = 0.8, 
                             weight = 0.5, popup = taz_popup, color = "#d7d7db")
  
  map <- map %>% addLegend("bottomleft", title = "Enrollment", pal = colorFactor(pal, NULL), values = ~enroll_kto8_cat)
  map <- map %>% setView(-116.88, 32.98, 9)
  map
}

createSchool9to12EnrollLeaflet <- function(taz_shape){
  taz_popup <- paste0("<strong>TAZ: </strong>", taz_shape$TAZ,
                      "<br><strong>Enrollment (9 to 12): </strong>", taz_shape$EnrollGrade9to12)
  pal <- c("#F1F1F1", brewer.pal(4, "YlOrBr"))
  opts <- providerTileOptions(opacity = 0)
  
  taz_shape$enroll_9to12_cat <- cut(taz_shape$EnrollGrade9to12, c(0,1,300,800,1000,50000), labels=c("None", "< 300", "300 - 800", "800 - 1000", "> 1000"), include.lowest=FALSE, right=FALSE)
  taz_shape@data <- taz_shape@data %>% select(OBJECTID, TAZ, enroll_9to12_cat)
  
  map <- leaflet(taz_shape) %>% addProviderTiles("CartoDB.PositronNoLabels", options = opts)
  
  map <- map %>% addPolygons(fillColor = ~colorFactor(pal, taz_shape$enroll_9to12_cat)(enroll_9to12_cat), fillOpacity = 0.8, 
                             weight = 0.5, popup = taz_popup, color = "#d7d7db")
  
  map <- map %>% addLegend("bottomleft", title = "Enrollment", pal = colorFactor(pal, NULL), values = ~enroll_9to12_cat)
  map <- map %>% setView(-116.88, 32.98, 9)
  map
}

createCollegeEnrollLeaflet <- function(taz_shape){
  taz_popup <- paste0("<strong>TAZ: </strong>", taz_shape$TAZ, 
                      "<br><strong>College Enrollment: </strong>", taz_shape$collegeEnroll)
  pal <- c("#F1F1F1", brewer.pal(2, "YlOrBr"))
  opts <- providerTileOptions(opacity = 0)
  
  taz_shape$enroll_college_cat <- cut(taz_shape$collegeEnroll, c(0,1,5000,50000), labels=c("None", "< 5000", ">= 5000"), include.lowest=FALSE, right=FALSE)
  taz_shape@data <- taz_shape@data %>% select(OBJECTID, TAZ, enroll_college_cat)
  
  map <- leaflet(taz_shape) %>% addProviderTiles("CartoDB.PositronNoLabels", options = opts)
  map <- map %>% addPolygons(fillColor = ~colorFactor(pal, taz_shape$enroll_college_cat)(enroll_college_cat), fillOpacity = 0.8, 
                             weight = 0.5, popup = taz_popup, color = "#d7d7db")
  
  map <- map %>% addLegend("bottomleft", title = "Enrollment", pal = colorFactor(pal, NULL), values = ~enroll_college_cat)
  map <- map %>% setView(-116.88, 32.98, 9)
  map
}

createCollegeOtherEnrollLeaflet <- function(taz_shape){
  taz_popup <- paste0("<strong>TAZ: </strong>", taz_shape$TAZ, 
                      "<br><strong>Other College Enrollment: </strong>", taz_shape$otherCollegeEnroll)
  pal <- c("#F1F1F1", brewer.pal(2, "YlOrBr"))
  opts <- providerTileOptions(opacity = 0)
  
  taz_shape$enroll_other_college_cat <- cut(taz_shape$otherCollegeEnroll, c(0,1,5000,50000), labels=c("None", "< 5000", ">= 5000"), include.lowest=FALSE, right=FALSE)
  taz_shape@data <- taz_shape@data %>% select(OBJECTID, TAZ, enroll_other_college_cat)
  
  map <- leaflet(taz_shape) %>% addProviderTiles("CartoDB.PositronNoLabels", options = opts)
  
  map <- map %>% addPolygons(fillColor = ~colorFactor(pal, taz_shape$enroll_other_college_cat)(enroll_other_college_cat), fillOpacity = 0.8, 
                             weight = 0.5, popup = taz_popup, color = "#d7d7db")
  
  map <- map %>% addLegend("bottomleft", title = "Enrollment", pal = colorFactor(pal, NULL), values = ~enroll_other_college_cat)
  
  map <- map %>% setView(-116.88, 32.98, 9)
  map
}

createCollegeStudentsLeaflet <- function(taz_shape){
  taz_popup <- paste0("<strong>TAZ: </strong>", taz_shape$TAZ, 
                      "<br><strong>College Students: </strong>", taz_shape$college_students)
  pal <- c("#F1F1F1", brewer.pal(4, "YlOrBr"))
  opts <- providerTileOptions(opacity = 0)
  
  taz_shape$college_student_cat <- cut(taz_shape$college_students, c(0,1,100,500,1000,50000), labels=c("None", "< 100", "100 - 500", "500 - 1000", " > 1000"), include.lowest=FALSE, right=FALSE)
  taz_shape@data <- taz_shape@data %>% select(OBJECTID, TAZ, college_student_cat)
  
  map <- leaflet(taz_shape) %>% addProviderTiles("CartoDB.PositronNoLabels", options = opts)
  map <- map %>% addPolygons(fillColor = ~colorFactor(pal, taz_shape$college_student_cat)(college_student_cat), fillOpacity = 0.8, 
                             weight = 0.5, popup = taz_popup, color = "#d7d7db")
  
  map <- map %>% addLegend("bottomleft", title = "College Students", pal = colorFactor(pal, NULL), values = ~college_student_cat)
  map <- map %>% setView(-116.88, 32.98, 9)
  map
}

createPopDenLeaflet <- function(taz_shape){
  taz_popup <- paste0("<strong>TAZ: </strong>", taz_shape$TAZ, 
                      "<br><strong>Population Density: </strong>", taz_shape$PopDen)
  pal <- c("#F1F1F1", brewer.pal(4, "YlOrBr"))
  opts <- providerTileOptions(opacity = 0)
  
  taz_shape$pop_density_cat <- cut(taz_shape$PopDen, c(0,0.00001,10,20,40,1000), labels=c("None", "< 10", "10 - 20", "20 - 40", "> 40"), include.lowest=FALSE, right=FALSE)
  taz_shape@data <- taz_shape@data %>% select(OBJECTID, TAZ, pop_density_cat)
  
  map <- leaflet(taz_shape) %>% addProviderTiles("CartoDB.PositronNoLabels", options = opts)
  map <- map %>% addPolygons(fillColor = ~colorFactor(pal, taz_shape$pop_density_cat)(pop_density_cat), weight = 0, popup = taz_popup, fillOpacity = 0.8)
  map <- map %>% addLegend("bottomleft", title = "Pop Density", pal = colorFactor(pal, NULL), values = ~pop_density_cat)
  map <- map %>% setView(-116.88, 32.98, 9)
  map
}

createAverageIncomeLeaflet <- function(taz_shape){
  taz_popup <- paste0("<strong>TAZ: </strong>", taz_shape$TAZ, 
                      "<br><strong>Average Income: </strong>", taz_shape$income)
  pal <- c("#F1F1F1", brewer.pal(5, "YlOrBr"))
  opts <- providerTileOptions(opacity = 0)
  
  taz_shape$income_cat <- cut(taz_shape$income, c(0,1,30000,60000,100000,150000,10000000), labels=c("None", "< 30K", "30K - 60K", "60K - 100K", "100K - 150K", "> 150K"), include.lowest=FALSE, right=FALSE)
  taz_shape@data <- taz_shape@data %>% select(OBJECTID, TAZ, income_cat)
  
  map <- leaflet(taz_shape) %>% addProviderTiles("CartoDB.PositronNoLabels", options = opts)
  map <- map %>% addPolygons(fillColor = ~colorFactor(pal, taz_shape$income_cat)(income_cat), fillOpacity = 0.8, 
                             weight = 0.5, popup = taz_popup, color = "")
  
  map <- map %>% addLegend("bottomleft", title = "Average Income", pal = colorFactor(pal, NULL), values = ~income_cat)
  map <- map %>% setView(-116.88, 32.98, 9)
  map
}

# create the required growth plots (Enplanements and Cross-Border Tours)
getGrowthPlots <- function(growthData){
  #growthData <- read_csv("C:\\Projects\\SANDAG\\ABM Support, 2016\\growthFiles\\parameters_by_years.csv")
  
  G1 <- ggplot(growthData, aes(x = year, y = airport_enplanements_passnegers)) + geom_line(color="royalblue", size=0.75, linetype=2) + geom_point(color="blue", size = 3)
  G1 <- G1 + labs(y='Enplanements', x='Year', title='Enplanements by Year')
  G1 <- G1 + scale_x_continuous(breaks=growthData$year) + scale_y_continuous(labels = scales::comma)
  G1 <- G1 + theme_new()

  G2 <- ggplot(growthData, aes(x = year, y = crossBorder_tours)) + geom_line(color="royalblue", size=0.75, linetype=2) + geom_point(color="blue", size = 3)
  G2 <- G2 + labs(y='Tours', x='Year', title='Cross-Border Tours by Year')
  G2 <- G2 + scale_x_continuous(breaks=growthData$year) + scale_y_continuous(labels = scales::comma)
  G2 <- G2 + theme_new()
  
  return(list(G1, G2))
}

# create ext-ext volume growth plot for a given origin-destination cordon
getExtExtGrowthPlot <- function(data, oCordon, dCordon){
  p <- ggplot(data, aes(x = year, y = Trips)) + geom_line(color="royalblue", size=0.75, linetype=2) + geom_point(color="blue", size = 3)
  p <- p + labs(y='Trips', x='Year', title=paste(oCordon, " - ", dCordon, sep = ""))
  p <- p + scale_x_continuous(breaks=data$year) + scale_y_continuous(labels = scales::comma)
  p <- p + theme_new()
  p
}

# create ext-int volume growth plot for a given origin (cordon name)
getExtIntGrowthPlot <- function(data, cname){
  p <- ggplot()
  p <- p + geom_line(aes(x=year, y=work, color="Work"), data, size=0.75, linetype=2) 
  p <- p + geom_point(aes(x=year, y=work), data, colour="blue", size = 3)
  p <- p + geom_line(aes(x=year, y=nonwork, color="Non-Work"), data,  size=0.75, linetype=2)
  p <- p + geom_point(aes(x=year, y=nonwork), data, colour="brown", size = 3)
  p <- p + labs(y='Trips', x='Year', title=cname, sep = "")
  p <- p + scale_color_manual("", values=c("Work"="blue", "Non-Work"="brown"))
  p <- p + scale_x_continuous(breaks=data$year) + scale_y_continuous(labels = scales::comma)
  p <- p + theme_new()
  p
}

theme_new <- function(){
  theme(
    title=element_text(size=16, face="bold"),
    axis.title=element_text(size=14,face="bold"),
    axis.text.x=element_text(angle=45,hjust=1),
    axis.text = element_text(size=14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line.x = element_line(color = "black", size = 1),
    axis.line.y = element_line(color = "black", size = 1),
    legend.position = "top",
    legend.text = element_text(size=20)
  )
}