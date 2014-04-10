#Get existing real-time configuration 



#' Get Current Configuration for Real-Time Reports
#' 
#' Get current configuration for real-time reports
#' 
#' 
#' GetRealTimeConfiguration returns a Data Frame with the current set up of
#' real-time reports within the Adobe Analytics Real-Time API. To change
#' configuration settings, use SaveRealTimeConfiguration function.
#' 
#' @param report_suite Report Suite ID
#' @return Data Frame
#' @seealso \code{\link{SaveRealTimeConfiguration}} \cr
#' @keywords GetRealTimeConfiguration
#' @examples
#' 
#' \dontrun{
#'   
#'   GetRealTimeConfiguration("keystonerandy")
#' 
#'    }
#' 
GetRealTimeConfiguration<- function (report_suite) {
  
  #API request
  json <- postRequest("ReportSuite.GetRealTimeConfiguration",sprintf('{"rsid":"%s"}', report_suite))
  
  if(json$status == 200){
    #Convert JSON to list
    results <- content(json)
  } else {
    stop(jsonResponseError(json$status))
  }
  
  if(length(results$correlations) == 0){
    return(print("No Real-Time Configuration Previously Set"))
  }
  
  results <- results[[1]] #Get the results in a cleaner manner by removing one level of nesting

  temp <- data.frame()
  for(i in 1:length(results)){
    min_granularity <- results[[i]]$min_granularity
    metric <- results[[i]]$metric
    #Pre-allocate so that all are always available
    element1 <- ""
    element2 <- ""
    element3 <- ""
    try(element1 <-results[[i]]$elements[1], silent = TRUE)
    try(element2 <-results[[i]]$elements[2], silent = TRUE)
    try(element3 <-results[[i]]$elements[3], silent = TRUE)
    temp <- rbind(temp, cbind(paste(i), min_granularity, metric, element1, element2, element3))
  }
  names(temp) <- c("report_number", "min_granularity", "metric", "element1", "element2", "element3")
  
    
  return(temp)
} #Ending bracket for function



