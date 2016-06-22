# Ashish Kulshrestha | kulshresthaa@pbworld.com | Parsons Brinckerhoff
# Last Edited: April 27, 2016
# Generic functions to perform variable/attributes consistency checks in data

checkMissingAttribute <- function(attrName, attrDesc, key, inputData) {
  if(!attrName %in% colnames(inputData))
    return(data.frame(CHECK = "Missing Attribute Check", ATTRIBUTE = attrName, DESC = attrDesc, RESULT=paste("ERROR:", attrName, "does not exists.")))
  
  call <- substitute(filter(inputData, !complete.cases(var)), list(var = as.name(attrName)))
  sub_data <- eval(call)
  
  if(nrow(sub_data) == 0)
    return(data.frame(CHECK = "Missing Attribute Check", ATTRIBUTE = attrName, DESC = attrDesc, RESULT=paste("All OK.")))
  else{
    call <- substitute(select(sub_data, var), list(var = as.name(key)))
    sub_data <- eval(call)
    
    result <- data.frame(CHECK = "Missing Attribute Check", ATTRIBUTE = attrName, DESC = attrDesc, RESULT="Missing !")
    
    rows_to_add <- nrow(sub_data) - 1

    for(r in 1:rows_to_add){
      result <- rbind(result, data.frame(CHECK = "", ATTRIBUTE = "", DESC = "", RESULT=""))
    }
    
    result <- cbind(result, sub_data)
    
    return(result)
  }
}

checkMissingAttributeNew <- function(attrNames, attrDescriptions, key, inputData) {
  result <- data.frame(var2=character(), var3=character(), var4=character(), var5=character())

  for(i in 1:length(attrNames)){
    attr<- attrNames[i]
    desc <- attrDescriptions[i]
    
    if(!attr %in% colnames(inputData))
      result <- rbind(result,data.frame(var2=attr, var3=desc, var4=paste("ERROR:", attr, "does not exists."), var5="")) 
    else{
      call <- substitute(filter(inputData, !complete.cases(var)), list(var = as.name(attr)))
      sub_data <- eval(call)
      
      if(nrow(sub_data) == 0)
        result <- rbind(result,data.frame(var2=attr, var3=desc, var4="All OK.", var5="")) 
      else{
        call <- substitute(transform(sub_data, var5 = as.character(var)), list(var = as.name(key)))
        sub_data <- eval(call) 
        sub_data <- select(sub_data, var5)
                                    
        #sub_data <- sapply(sub_data[,], as.character)
        
        sub_result <- data.frame(var2=attr, var3=desc, var4="Missing!")
        rows_to_add <- nrow(sub_data) - 1

        for(r in 1:rows_to_add){
          sub_result <- rbind(sub_result, data.frame(var2 = "", var3 = "", var4=""))
        }
        sub_result <- cbind(sub_result, sub_data)
        result <- rbind(result, sub_result)
      }
    }
  }
  names(result) <- c("ATTRIBUTE", "DESC", "RESULT", key)
  return(result)
}

checkDuplicate <- function(attrName, data) {
  if(!attrName %in% colnames(data))
    return(paste("Error:", attrName, "does not exists."))

  sub_data <- data[names(data) == attrName]
  sub_data$rownum <- c(1:nrow(sub_data))
  sub_data <- sub_data[complete.cases(sub_data),]
  duplicate_data <- sub_data[duplicated(sub_data[[attrName]]) | duplicated(sub_data[[attrName]], fromLast=TRUE) ,]
  
  if(nrow(duplicate_data) > 0){
    rNum <- paste(as.character(duplicate_data$rownum), collapse=", ")
    return(paste("Row # with Duplicate", attrName, ":", rNum))
  } else {
    return("ALL OK!")
  }
}

checkWithinRange <- function(attrName, data, lowerRange, upperRange) {
  if(!attrName %in% colnames(data))
    return(paste("Error:", attrName, "does not exists."))
  
  sub_data <- data[names(data) == attrName]
  sub_data$rownum <- c(1:nrow(sub_data))
  sub_data <- sub_data[complete.cases(sub_data),]
  
  l_data <- data.frame()
  u_data <- data.frame()
  range <- paste("(", lowerRange, "-" , upperRange, ")", sep = "")
  
  # checking for lower range, if provided
  if(!is.null(lowerRange))
    l_data <- sub_data[sub_data[[attrName]] < lowerRange, ]

  # checking for upper range, if provided
  if(!is.null(upperRange))
    u_data <- sub_data[sub_data[[attrName]] > upperRange, ]
  
  if(nrow(l_data) == 0 & nrow(u_data) == 0)
    return("ALL OK!")
  
  if(nrow(l_data) == 0 & nrow(u_data) > 0)
    rNum <- paste(as.character(u_data$rownum), collapse = ", ")
  
  if(nrow(l_data) > 0 & nrow(u_data) == 0)
    rNum <- paste(as.character(l_data$rownum), collapse = ", ")
  
  if(nrow(l_data) > 0 & nrow(u_data) > 0)
    rNum <- paste(paste(as.character(l_data$rownum), collapse = ", "), paste(as.character(u_data$rownum), collapse = ", "), sep = ", ")
  
  return(paste("Row # with", attrName, "not within Specified Range", range, ":", rNum))
}

checkTotalValue <- function(attrName, subAttrNames, data) {
  if(!attrName %in% colnames(data))
    return(paste("Error:", attrName, "does not exists."))
  
  for(i in 1:length(subAttrNames)) {
    if(!subAttrNames[i] %in% colnames(data))
      return(paste("Error:", subAttrNames[i], "does not exists."))
  }
  
  data$diff <- data[[attrName]]
  
  for(i in 1:length(subAttrNames)) {
    data$diff <- data$diff - data[[subAttrNames[i]]]
  }
  
  sub_data <- data[names(data) == "diff"]
  sub_data$rownum <- c(1:nrow(sub_data))
  sub_data <- filter(sub_data, diff != 0)
  
  rNum <- ""
  
  if(nrow(sub_data) > 0){
    rNum <- paste(as.character(duplicate_data$rownum), collapse=", ")
    return(paste("Row # with sum of", paste(as.character(subAttrNames), collapse = ", "), "different than", attrName, ":", rNum))
  } else {
    return("ALL OK!")
  }
}

checkRatio <- function(attr1, attr2, key, data, lowerRange, upperRange) {
  if(!attr1 %in% colnames(data))
    return(data.frame(CHECK = paste("Ratio check of", attr1, "and", attr2), ERROR=paste("Attribute", attr1, "does not exists.")))

  if(!attr2 %in% colnames(data))
    return(data.frame(CHECK = paste("Ratio check of", attr1, "and", attr2), ERROR=paste("Attribute", attr2, "does not exists.")))
  
  result <- data.frame("a" = numeric(), "b" = numeric(), "c" = numeric())
  
  for(i in 1:nrow(data)){
    #check if both of them are zero
    #if((data[i, attr1] > 0 & data[i, attr2] == 0) | (data[i, attr1] == 0 & data[i, attr2] > 0))
      #result <- rbind(result, data.frame("a" = data[i, key], "b" = data[i, attr1], "c" = data[i, attr2]))

    #if both are > 0
    if(data[i, attr1] > 0 & data[i, attr2] > 0){
      ratio <- data[i, attr1]/data[i, attr2]
      if(ratio > upperRange | ratio < lowerRange)
        result <- rbind(result, data.frame("a" = data[i, key], "b" = data[i, attr1], "c" = data[i, attr2]))
    }
  }

  names(result) = c(key, attr1, attr2)
  
  #if the ratio is within the specified range for all
  if(nrow(result) == 0)
    return(data.frame(CHECK = paste("Ratio check of", attr1, "and", attr2), RESULT="ALL OK!"))
  
  return(result)
}