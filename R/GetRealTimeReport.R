#### GetRealTimeReport
#https://developer.omniture.com/en_US/documentation/sitecatalyst-reporting/r-getrealtimereport#reference_15F98487006A4E529E6B83CA7652A1FC



#' Get Real-Time Report
#' 
#' Get real-time report
#' 
#' 
#' GetRealTimeReport returns a Data Frame with the results from the real-time
#' report specified. To find out valid values for the inputs of this function,
#' run GetRealTimeConfiguration to get the current report setup. To change the
#' real-time report setup, use SaveRealTimeConfiguration.
#' 
#' Currently, RSiteCatalyst only supports up to one element in a realtime
#' report. This limitation will be removed in a future release.
#' 
#' @param report_suite Report Suite ID
#' @param metrics Metric to include in the Realtime report. The metric must be
#' configured previously using SaveRealTimeConfiguration
#' @param elements A list of elements that breaks down (organizes) the metrics
#' data in the report. Each element must be configured previously using
#' SaveRealTimeConfiguration.
#' 
#' If no elements are provided, an overtime report is generated for the
#' provided metric.
#' @param periodMinutes Number of minutes for one period. Default is 1.
#' @param periodCount The number of periods of data to return. Default is 15.
#' @param periodOffset Number of minutes before the current minute to run the
#' report. If set to 10, the most recent result will be from ten minutes prior
#' to the request. Default is 0.
#' @param algorithm Type of dimensions to return, one of the following three
#' values: 'gainers', 'losers', 'most popular'. Default is 'most popular'.
#' @param algorithmArgument Specifies how to order the values for Most Popular,
#' Gainers or Losers. Specify either percent, or linear. Default is linear.
#' @param firstRankPeriod Computes the ranking of elements by considering the
#' element's counts from the firstRankPeriod to the final period. With this
#' argument you can rank from the first period (0) to periodCount - 1 (most
#' popular) or periodCount - 3 (gainers/losers) or anywhere in between. The
#' firstRankPeriod is 0 based.
#' @param floorSensitivity A factor between 0 and 1 that is used to cut off
#' low-count items from percentage ranking. Relative only to gainers/losers by
#' percent. Default is .25.
#' @return Data Frame
#' @seealso \code{\link{GetRealTimeConfiguration}}\cr
#' \code{\link{SaveRealTimeConfiguration}}\cr
#' @references See documentation for in-depth documentation:
#' 
#' https://developer.omniture.com/en_US/documentation/sitecatalyst-reporting/r-realtimereportdescription
#' @keywords GetRealTimeConfiguration
#' @examples
#' 
#' \dontrun{
#'   
#'   #Minimum example - Equivalent to Overtime report
#'   GetRealTimeReport("keystonerandy", "instances")
#' 
#'    }
#' 
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
