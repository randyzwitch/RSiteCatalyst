#QueueRanked report
#Corresponds to pulling a single ranked report, no correlation/subrelation
#This API method seems to be most complicated to return a valid result


QueueRanked <- function(reportSuiteID, dateFrom, dateTo, metrics, elements, top="", startingWith="", segment_id="", selected=""){
  
  #1.  Send API request to build report- QueueRanked
  
  #Error check to see if function call using both parameters
  if(top!= "" && selected != "") {
    
    return(print("Error:  Use 'top' or 'startingWith' arguments, not both"))
  }
  
  
  #Loop over the metrics list, appending proper curly braces
  metrics_conv <- lapply(metrics, function(x) paste('{"id":', '"', x, '"', '}', sep=""))
  #Collapse the list into a proper comma separated string
  metrics_final <- paste(metrics_conv, collapse=", ") 

  if(top != "") {
  json_request <-sprintf(
    '{"reportDescription":
    {"reportSuiteID" :"%s",
     "dateFrom":"%s",
     "dateTo":"%s",
     "metrics": [%s],
     "elements" : [{"id":"%s", "top": "%s", "startingWith":"%s"}],
     "segment_id": "%s"
    }
}', reportSuiteID, dateFrom, dateTo, metrics_final, elements, top, startingWith, segment_id)
  } else {
    
    selected <- toJSON(selected)
    
    json_request <- sprintf(
  '{"reportDescription":
    {"reportSuiteID" :"%s",
     "dateFrom":"%s",
     "dateTo":"%s",
     "metrics": [%s],
     "elements" : [{"id":"%s", "selected":%s }],
     "segment_id": "%s"
    }
}', reportSuiteID, dateFrom, dateTo, metrics_final, elements, selected, segment_id)
    
  }  
  
  
  json_queue <- postRequest("Report.QueueRanked", json_request)
  
  if(json_queue$status == 200) {
    #Convert JSON to list
    queue_resp <- content(json_queue)
  } else {
    return(jsonResponseError(json_queue$status))
  }
  
  #If response returns an error, return error message. Else, continue with
  #capturing report ID
  if(queue_resp[1] != "queued" ) {
    return(print("Error: Likely a syntax error in arguments to QueueRanked function"))
  } else {
    reportID <- queue_resp[[3]] 
  }
  
  #Check to see whether report is done. while loop with 
  #Sys.sleep waits 10 seconds before trying again
  print("Checking report status: Attempt Number 1")
  reportDone <- GetStatus(reportID)
  
  if(reportDone == "failed") {
    return(print("Report Failed: Check for json_request syntax error"))
  }
  
  num_tries <- 1
  while(reportDone != "done" && num_tries < 10){
    num_tries <- num_tries + 1
    Sys.sleep(10)
    print(paste("Checking report status: Attempt Number", num_tries))
    reportDone <- GetStatus(reportID)
    
  }
  
  #If reportDone still not done, return an error. Else, continue to GetReport
  if(reportDone !="done"){
    return(print("Error: Number of Tries Exceeded"))
  } else {
    
    #Write formatted JSON string to a 5-item list
    result <- getReport(reportID)
    metrics_requested <- sapply(result[[5]][[4]], "[[", "id") #get metrics
    elements_requested <- sapply(result[[5]][[3]], "[[", "name") #get elements
    segment_requested <- result[[5]][[5]] #get segment
    
  } #End of else statement testing reportDone = "done"
  #Convert from JSON to data frame, not currently working for multiple metrics

  data <- result[[5]]$data #Just the data portion of the JSON result
  
  rows_df <- ldply(data, "[[", "name")  #get element as rows
  names(rows_df) <- elements_requested
  
  counts <- lapply(data, "[[", "counts") # Just the "counts" column
  counts_df <- ldply(counts, quickdf) # counts as DF
  names(counts_df) <- metrics_requested
  
  return(cbind(rows_df, segment=segment_requested, counts_df)) #append rows info with counts
  
} #End function bracket
