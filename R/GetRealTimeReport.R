#### GetRealTimeReport
#https://developer.omniture.com/en_US/documentation/sitecatalyst-reporting/r-getrealtimereport#reference_15F98487006A4E529E6B83CA7652A1FC

GetRealTimeReport <- function(report_suite, metrics, elements = "", periodMinutes = 1, periodCount = 15, periodOffset = 0, algorithm = "most popular", algorithmArgument= "linear", firstRankPeriod="0", floorSensitivity=.25) {

  #Loop over the metrics list, appending proper curly braces
  metrics_conv <- lapply(metrics, function(x) paste('{"id":', '"', x, '"', '}', sep=""))

  #Collapse the list into a proper comma separated string
  metrics_final <- paste(metrics_conv, collapse=", ") 
  
  if(elements == "") {
  
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
  
  return(ldply(result$data, quickdf)) #End if(element == "") logic
  
} else {
  
#Element list passed
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
                          "elements": [{ "id": "page" }]
                          }
}', report_suite, metrics_final, periodMinutes, periodCount, periodOffset, algorithm, algorithmArgument, firstRankPeriod, floorSensitivity)

} #End else bracket
  
} #End function bracket



