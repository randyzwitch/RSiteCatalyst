#### GetRealTimeReport
#https://developer.omniture.com/en_US/documentation/sitecatalyst-reporting/r-getrealtimereport#reference_15F98487006A4E529E6B83CA7652A1FC

GetRealTimeReport <- function(report_suite, metrics, elements = c(), periodMinutes = 1, periodCount = 15, periodOffset = 0, algorithm = "most popular", algorithmArgument= "linear", 
                              firstRankPeriod="0", floorSensitivity=.25) {

  #Loop over the metrics list, appending proper curly braces
  metrics_conv <- lapply(metrics, function(x) paste('{"id":', '"', x, '"', '}', sep=""))

  #Collapse the list into a proper comma separated string
  metrics_final <- paste(metrics_conv, collapse=", ") 
  
  #With no elements list, equivalent to requesting overtime report
  if(length(elements) == 0) {
  
  body <- sprintf('{
    "reportDescription": {
                          "reportSuiteID": "%s",
                          "metrics": [%s],
                          "periodMinutes":  %s,
                          "periodCount":    %s,
                          "periodOffset":   %s,
                          "algorithm": "%s",
                          "algorithmArgument":  "%s",
                          "firstRankPeriod": "%s",
                          "floorSensitivity": %s
                          }
}', report_suite, metrics_final, periodMinutes, periodCount, periodOffset, algorithm, algorithmArgument, firstRankPeriod, floorSensitivity)
  
  #Make API request
  json <- postRequest("Report.GetRealTimeReport", body)
  #Get result from simplest request
  result <- content(json)[[1]]
  
  #Convert results to DataFrame
  result_parsed <- ldply(result$data, quickdf)
  result_parsed$counts <- as.numeric(result_parsed$counts) #Convert to numeric
  
  names(result_parsed)[names(result_parsed) == 'counts'] <- metrics
  
  return(result_parsed) #End if(element == "") logic
  
} else if(length(elements) == 1) {
 
#Element list of length 1 passed
  body <- sprintf('{
    "reportDescription": {
                          "reportSuiteID": "%s",
                          "metrics": [%s],
                          "periodMinutes":  %s,
                          "periodCount":    %s,
                          "periodOffset":   %s,
                          "algorithm": "%s",
                          "algorithmArgument":  "%s",
                          "firstRankPeriod": "%s",
                          "floorSensitivity": %s,
                          "elements": [{ "id": "%s" }]
                          }
}', report_suite, metrics_final, periodMinutes, periodCount, periodOffset, algorithm, algorithmArgument, firstRankPeriod, floorSensitivity, elements)

  #Make API request
  json <- postRequest("Report.GetRealTimeReport", body)
  #Get result from request
  result <- content(json)[[1]]
  
  metrics_requested <- sapply(result[[4]], "[[", "id") #get metrics
  elements_requested <- sapply(result[[3]], "[[", "name") #get elements
  rows_df <- ldply(result$data, "[[", "name")  #get element as rows
  
  accum = data.frame()
  for(i in 1:nrow(rows_df)){
    temp <- ldply(result$data[[i]]$breakdown, quickdf)
    temp <- cbind(rows_df[i,], temp)
    accum <- rbind(accum, temp)
  }
  #Set names on accum data frame
  names(accum) <- c(elements, "name", "year", "month", "day", "hour", "minute", metrics_requested, paste(metrics_requested, "_total", sep=''))
  accum[[8]] <- as.numeric(accum[[8]]) #Convert to numeric from character
  accum[[9]] <- as.numeric(accum[[9]]) #Convert to numeric from character
  
  return(accum) #End if(length(element) == 1) logic
  
} else if(length(elements) > 1) {
  stop("Currently, only one element breakdown supported by RSiteCatalyst")
}
  
} #End function bracket