#QueueRanked report
#Corresponds to pulling a ranked report
#This API method seems to be most complicated to return a valid result
  


#' Run a QueueRanked Report
#' 
#' A QueueRanked report is a report that shows the ranking of values for one or
#' more elements relative to a metric, aggregated over the time period
#' selected.
#' 
#' The QueueRanked function returns a data frame equivalent to pulling a Ranked
#' report in SiteCatalyst. Correlations & Sub-Relations are supported.
#' 
#' 
#' Because of the Reporting API structure, this function first requests the
#' report, then checks the reporting queue to see if the report is completed,
#' and when the report returns as "done" pulls the report from the API. This
#' checking process will occur up to the specified number of times (default
#' 120), with a delay between status checks (default 5 seconds). If the report
#' does not return as "done" after the number of tries have completed, the
#' function will return an error message.
#' 
#' Note: Because of the multiple argument types ("top" and "startingWith" OR
#' "selected"), keyword arguments are generally needed towards the end of the
#' function call instead of just positional arguments.
#' 
#' @param reportSuiteID Report Suite ID
#' @param dateFrom Report Start Date in "YYYY-MM-DD" format
#' @param dateTo Report End Date in "YYYY-MM-DD" format
#' @param metrics The metric(s) you want in the report
#' @param elements
#' 
#' The element(s) (page, browser, eVar, prop) for the report to be broken down
#' by.
#' 
#' @param top How many results you want trended. Used in combination with
#' "startingWith". Not used if "selected" argument used.
#' @param startingWith The first ranked number you want in the report. Used in
#' combination with "top". Not used if "selected" argument used.
#' @param selected List of selected values, such as specific pages or eVar
#' values. Not used if "top" and "startingWith" arguments are used
#' @param segment_id Optional. If no segment_id is specified, metrics will be
#' reported for all visitors.
#' @param currentData Optional. Use value of "1" to get current data results.
#' Only needed when dateTo is greater than or equal to the current day.
#' @param searchType Optional. An enumerated list of boolean values used to
#' link multiple search terms in a report search. Takes values of "AND", "OR"
#' or "NOT".
#' @param searchKW Optional. A list of keywords to include or exclude from the
#' search, based on the searchType. Keyword values can also leverage the
#' following special characters: '*' (Wild card), '^' (Starts With), '$' (Ends
#' With). "Top" argument required when using regex functionality.
#' @param maxTries Optional. Provide integer value for the max number of API
#' attempts you want to try retrieve the report before function errors out.
#' Defaults to 120.
#' @param waitTime Optional. Provide integer value for the number of seconds
#' between tries to API to try retrieve the report. Defaults to 5 seconds.
#' @return Data Frame
#' @seealso \code{\link{GetAvailableMetrics}} \cr \code{\link{GetSegments}} \cr
#' \code{\link{GetAvailableElements}}
#' @keywords QueueRanked
#' @examples
#' 
#' \dontrun{
#' 
#' #Top 100 pages viewed/visits for Loyal_Visitors segment, broken down by browser 
#' (correlation/subrelation)
#' top100_pages <- 
#' QueueRanked("keystonerandy", "2013-02-13","2013-02-28", c("pageviews"), c('page', 'browser'),
#' top= "100", startingWith= "1", segment_id= "Loyal_Visitors")
#' 
#' 
#' #Get just the elements you want from the ranked list, instead of "Top"
#' single_pages <- 
#' QueueRanked("keystonerandy", "2013-02-13","2013-02-19", c("pageviews", "visits"),'page',
#' selected = c("http://randyzwitch.com", "http://randyzwitch.com/about"))    
#' 
#'    }
#' 
#' @export QueueRanked
QueueRanked <- function(reportSuiteID, dateFrom, dateTo, metrics, elements, top="", startingWith="", segment_id="", selected="", currentData="", searchType="", searchKW="", maxTries= 120, waitTime= 5){
  
  #1.  Send API request to build report- QueueRanked
  
  #Error check to see if function call using both parameters
  if(top!= "" && selected != "") {
    stop("Use 'top' or 'startingWith' arguments, not both")
  }
  
  #Error check to see if elements list has more than two elements
  if(length(elements) > 2) {
    
    stop("API only supports a maximum of two elements")
  }
  
  if(searchKW != "" && top == "") {
    
    stop("Top argument required when using searchKW")
  }
  
  #Loop over the metrics list, appending proper curly braces
  metrics_conv <- lapply(metrics, function(x) paste('{"id":', '"', x, '"', '}', sep=""))
  #Collapse the list into a proper comma separated string
  metrics_final <- paste(metrics_conv, collapse=", ") 

  
  if(top != "") {
    
  #Modify element list based on whether it has one or two values   
  
    #Add quotes around regexes
    searchKW2 <- lapply(searchKW, function(x) paste('"', x, '"', sep=""))
    #Create string from quoted list above
    searchKW2 <- paste(searchKW2, collapse= ", ")

      if(length(elements) == 1) {
        elements_list = sprintf('{"id":"%s", 
                                  "top": "%s", 
                                  "startingWith":"%s",
                                  "search":{"type":"%s", "keywords":[%s]}
                                  }', elements, top, startingWith, searchType, searchKW2)
      } else {
        #Hard-coded value of 1000000 is to make sure "all" sub-relation values provided without needing another
        #parameter in the function call
        elements_list = sprintf('{"id":"%s", 
                                  "top": "%s", 
                                  "startingWith":"%s",
                                  "search":{"type":"%s", "keywords":[%s]}},
                                  {"id":"%s", "top":"1000000"}', elements[1],top, startingWith, searchType, searchKW2, elements[2])
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
    },"validate": true
}', reportSuiteID, dateFrom, dateTo, metrics_final, elements_list,segment_id, currentData)
  } else {
    
    selected <- toJSON(selected)
    
    #Modify element list based on whether it has one or two values   
    
    if(length(elements) == 1) {
      elements_list = sprintf('{"id":"%s", "selected":%s }', elements, selected)
    } else {
      
      #Hard-coded value of 1000000 is to make sure "all" sub-relation values provided without needing another
      #parameter in the function call
      
      elements_list = sprintf('{"id":"%s", "selected":%s }, {"id":"%s", "top":"1000000"}', elements[1],selected, elements[2])
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
    }, "validate": true
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
    stop(sprintf("API %s : %s", queue_resp$status, queue_resp$statusMsg))
  } else {
    reportID <- queue_resp[[3]] 
  }
  
  #Check to see whether report is done. while loop with 
  #Sys.sleep waits waitTime seconds before trying again
  print("Checking report status: Attempt Number 1")
  reportDone <- GetStatus(reportID)
  
  if(reportDone == "failed") {
    stop("Report Failed: Check for json_request syntax error")
  }
  
  num_tries <- 1
  while(reportDone != "done" && num_tries < maxTries){
    num_tries <- num_tries + 1
    Sys.sleep(waitTime)
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
  #counts_df <- as.data.frame(apply(counts_df, MARGIN=2, FUN= function(x) as.numeric(x))) #convert to numeric
  counts_df <- as.data.frame(lapply(counts_df, FUN= function(x) as.numeric(x))) #convert to numeric
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
      #inner_metrics <- as.data.frame(apply(inner_metrics, MARGIN=2, FUN= function(x) as.numeric(x))) #convert to numeric
      inner_metrics <- as.data.frame(lapply(inner_metrics, FUN= function(x) as.numeric(x))) #convert to numeric
  
      names(inner_metrics) <- metrics[1:ncol(inner_metrics)]     
  #Join all datasets together horizontally
      temp <- cbind(outer_element, inner_element, segment=segment_requested, inner_metrics)
  #Append vertically to accumulator    
      accumulator <- rbind(accumulator, temp)
      if(i == length(data) && ncol(inner_metrics) < length(metrics)){
        warning("Number of metrics returned by API fewer than requested. Labels assigned in order of metrics list, verify results for accuracy.")
      }
      
  } #End of for loop (don't hate me Hadley!)
  return(accumulator)
  } #End JSON parsing of two element case
} #End function bracket
