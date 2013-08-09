#QueueRanked report
#Corresponds to pulling a ranked report
#This API method seems to be most complicated to return a valid result


QueueRanked <- function(reportSuiteID, dateFrom, dateTo, metrics, elements, top="", startingWith="", segment_id="", selected="", currentData=""){
  
  #1.  Send API request to build report- QueueRanked
  
  #Error check to see if function call using both parameters
  if(top!= "" && selected != "") {
    stop("Use 'top' or 'startingWith' arguments, not both")
  }
  
  #Error check to see if elements list has more than two elements
  if(length(elements) > 2) {
    
    stop("API only supports a maximum of two elements")
  }
  
  #Loop over the metrics list, appending proper curly braces
  metrics_conv <- lapply(metrics, function(x) paste('{"id":', '"', x, '"', '}', sep=""))
  #Collapse the list into a proper comma separated string
  metrics_final <- paste(metrics_conv, collapse=", ") 

  
  if(top != "") {
    
  #Modify element list based on whether it has one or two values   
  
      if(length(elements) == 1) {
        elements_list = sprintf('{"id":"%s", "top": "%s", "startingWith":"%s"}', elements,top, startingWith)
      } else {
        elements_list = sprintf('{"id":"%s", "top": "%s", "startingWith":"%s"}, {"id":"%s", "top":"1000"}', elements[1],top, startingWith, elements[2])
      }
    
  json_request <-sprintf(
    '{"reportDescription":
    {"reportSuiteID" :"%s",
     "dateFrom":"%s",
     "dateTo":"%s",
     "metrics": [%s],
     "elements" : [%s],
     "segment_id": "%s",
     "currentData": "%s"
    }
}', reportSuiteID, dateFrom, dateTo, metrics_final, elements_list,segment_id, currentData)
  } else {
    
    selected <- toJSON(selected)
    
    #Modify element list based on whether it has one or two values   
    
    if(length(elements) == 1) {
      elements_list = sprintf('{"id":"%s", "selected":%s }', elements, selected)
    } else {
      elements_list = sprintf('{"id":"%s", "selected":%s }, {"id":"%s", "top":"1000"}', elements[1],selected, elements[2])
    }
    
    json_request <- sprintf(
  '{"reportDescription":
    {"reportSuiteID" :"%s",
     "dateFrom":"%s",
     "dateTo":"%s",
     "metrics": [%s],
     "elements" : [%s],
     "segment_id": "%s",
     "currentData": "%s"
    }
}', reportSuiteID, dateFrom, dateTo, metrics_final, elements_list, segment_id, currentData)
    
  }  
  
  #Send post request to Omniture API
  json_queue <- postRequest("Report.QueueRanked", json_request)
  
  if(json_queue$status == 200) {
    #Convert JSON to list
    queue_resp <- content(json_queue)
  } else {
    stop(jsonResponseError(json_queue$status))
  }
  
  #If response returns an error, return error message. Else, continue with
  #capturing report ID
  if(queue_resp[1] != "queued" ) {
    stop("Error: Likely a syntax error in arguments to QueueRanked function")
  } else {
    reportID <- queue_resp[[3]] 
  }
  
  #Check to see whether report is done. while loop with 
  #Sys.sleep waits 10 seconds before trying again
  print("Checking report status: Attempt Number 1")
  reportDone <- GetStatus(reportID)
  
  if(reportDone == "failed") {
    stop("Report Failed: Check for json_request syntax error")
  }
  
  num_tries <- 1
  while(reportDone != "done" && num_tries < 120){
    num_tries <- num_tries + 1
    Sys.sleep(5)
    print(paste("Checking report status: Attempt Number", num_tries))
    reportDone <- GetStatus(reportID)
    
  }
  
  #If reportDone still not done, return an error. Else, continue to GetReport
  if(reportDone !="done"){
    stop("Error: Number of Tries Exceeded")
  } else {
    
    #Write formatted JSON string to a 5-item list
    result <- getReport(reportID)
    metrics_requested <- sapply(result[[5]][[4]], "[[", "id") #get metrics
    elements_requested <- sapply(result[[5]][[3]], "[[", "name") #get elements
    segment_requested <- result[[5]][[5]] #get segment
    
  } #End of else statement testing reportDone = "done"
  
  
  
  #Convert from JSON to data frame
  data <- result[[5]]$data #Just the data portion of the JSON result
  
  if(length(elements) == 1){
  
  rows_df <- ldply(data, "[[", "name")  #get element as rows
  names(rows_df) <- elements_requested
  
  counts <- lapply(data, "[[", "counts") # Just the "counts" column
  counts_df <- ldply(counts, quickdf) # counts as DF
  names(counts_df) <- metrics_requested
  
  return(cbind(rows_df, segment=segment_requested, counts_df)) #append rows info with counts, End JSON parsing for single element case 
  } else {
    
  accumulator <- data.frame()
  
  for(i in 1:length(data)){
  #Get outer element name
      outer_element <- as.data.frame(data[[i]]["name"])
      names(outer_element) <- elements_requested[1]
  #Get all breakdowns for outer element    
      inner_element <- ldply((data[[i]][["breakdown"]]), "[[", "name")
      names(inner_element) <- elements_requested[2]
  #Get metrics that go along with breakdown rows    
      inner_metrics <- ldply((data[[i]][["breakdown"]]), "[[", "counts")
    names(inner_metrics) <- metrics[1:ncol(inner_metrics)]     
  #Join all datasets together horizontally
      temp <- cbind(outer_element, inner_element, inner_metrics)
  #Append vertically to accumulator    
      accumulator <- rbind(accumulator, temp)
      if(i == length(data) && ncol(inner_metrics) < length(metrics)){
        warning("Number of metrics returned by API fewer than requested. Labels assigned in order of metrics list, verify results for accuracy.")
      }
      
  } #End of for loop (don't hate me Hadley!)
  return(accumulator)
  } #End JSON parsing of two element case
} #End function bracket
