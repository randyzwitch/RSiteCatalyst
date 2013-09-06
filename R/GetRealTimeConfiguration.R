#Get existing real-time configuration 

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
    element1 <-results[[i]]$elements[1]
    element2 <-results[[i]]$elements[2]
    temp <- rbind(temp, cbind(paste(i), min_granularity, metric, element1, element2))
  }
  names(temp) <- c("report_number", "min_granularity", "metric", "element1", "element2")
  
    
  return(temp)
} #Ending bracket for function



