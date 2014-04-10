#QueueTrended report
#Submits a Trended report request. Trended reports display trends for a single metric 
#(revenue, orders, views, and so forth) and element 
#(product, category, page, and so forth).



#' Run a QueueTrended Report
#' 
#' A QueueTrended report is a report where a single metric is retrieved, broken
#' down by an element such as page, eVar, prop, etc. and with a time component.
#' Within the 'element' list, either the "Top X" number of elements can be
#' received or you can specify the specific elements you are interested in
#' (such as 3 specific page names).
#' 
#' The QueueTrended report is analogous to pulling a "trended" report within
#' SiteCatalyst, but without the limitation of only 5 elements as in the
#' SiteCatalyst interface.
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
#' Note: Because of the multiple argument type ("top" and "startingWith" OR
#' "selected"), keyword arguments are generally needed towards the end of the
#' function call instead of just positional arguments.
#' 
#' @param reportSuiteID Report Suite ID
#' @param dateFrom Report Start Date in "YYYY-MM-DD" format
#' @param dateTo Report End Date in "YYYY-MM-DD" format
#' @param dateGranularity "Day", "Week", "Month", "Quarter" or "Year"
#' (case-insensitive).
#' @param metric The metric you want to trend.
#' @param element The element (page, browser eVar, prop) for the report to be
#' broken down by.
#' @param top How many results you want trended. Used in combination with
#' "startingWith". Not used if "selected" argument used.
#' @param startingWith The first ranked number you want in the report. Used in
#' combination with "top". Not used if "selected" argument used.
#' @param selected List of selected values, such as specific pages or eVar
#' values. Not used if "top" and "startingWith" arguments are used
#' @param segment_id Optional. If no segment_id is specified, metrics will be
#' reported for all visitors.
#' @param anomalyDetection Optional. Use value of "1" to get anomaly detection
#' results. Results only returned by API for 'Day' granularity.
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
#' @keywords QueueTrended
#' @examples
#' 
#' \dontrun{
#' 
#' #Pageviews for the Top 100 pages by Day in the Loyal_Visitors segment
#' pages_by_day <-  
#' QueueTrended("keystonerandy", "2013-02-13", "2013-02-19", "day", 
#' "pageviews", "page", top="100", startingWith= "1", segment_id= "Loyal_Visitors")
#' 
#' 
#' #Specifying two specific pages, trended by hour for pageviews in the Loyal_Visitors segment
#' specific_pages_by_hour <-  
#' QueueTrended("keystonerandy", "2013-02-13", "2013-02-19", "hour", 
#' "pageviews", "page", selected = c("http://randyzwitch.com", 
#' "http://randyzwitch.com/about"), segment_id = "Loyal_Visitors")
#'   
#' )
#'    }
#' 
#' @export QueueTrended
QueueTrended <- function(reportSuiteID, dateFrom, dateTo, dateGranularity, metric, element, top="", startingWith="", selected= "", segment_id="", anomalyDetection="", currentData="", searchType="", searchKW="", maxTries= 120, waitTime= 5) {

  #Error check to see if function call using both parameters
if(top!= "" && selected != "") {
  
  stop("Use 'top' or 'startingWith' arguments, not both")
}

if(anomalyDetection == "1" & dateGranularity!="day") {
  stop("Error: Anomaly Detection only provided for day granularity")
}

if(searchKW != "" && top == "") {
  
  stop("Top argument required when using searchKW")
}
  
#Build JSON request for "Top" functionality

if(top != "") {
  
  #Add quotes around regexes
  searchKW2 <- lapply(searchKW, function(x) paste('"', x, '"', sep=""))
  #Create string from quoted list above
  searchKW2 <- paste(searchKW2, collapse= ", ")
  
    elements_list = sprintf('{"id":"%s", 
                                  "top": "%s", 
                                  "startingWith":"%s",
                                  "search":{"type":"%s", "keywords":[%s]}
                                  }', element, top, startingWith, searchType, searchKW2)

  json_request <- sprintf(
    '{"reportDescription":
    {"reportSuiteID" :"%s",
     "dateFrom":"%s",
     "dateTo":"%s",
     "dateGranularity":"%s",
     "metrics": [{"id":"%s"}],
     "elements" : [%s],
     "segment_id": "%s",
     "anomalyDetection": "%s",
     "currentData": "%s"
    }, "validate": true
}', reportSuiteID, dateFrom, dateTo, dateGranularity, metric, elements_list, segment_id, anomalyDetection, currentData)
  
}  else {
  
  #Build JSON request for selected elements
  
  selected <- toJSON(as.list(selected))
  
  json_request <- sprintf(
    '{"reportDescription":
    {"reportSuiteID" :"%s",
     "dateFrom":"%s",
     "dateTo":"%s",
     "dateGranularity":"%s",
     "metrics": [{"id":"%s"}],
     "elements" : [{"id":"%s", "selected": %s }],
     "segment_id": "%s",
     "anomalyDetection": "%s",
     "currentData": "%s"
    }, "validate": true
}', reportSuiteID, dateFrom, dateTo, dateGranularity, metric, element, selected, segment_id, anomalyDetection, currentData)
  
}

#1.  Send API request to build report- QueueOvertime
json_queue <- postRequest("Report.QueueTrended", json_request)

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
#Sys.sleep waits 2 seconds before trying again
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
  metric_requested <- result[[5]][[4]][[1]]$id
  element_requested <- result[[5]][[3]][[1]]$id
  segment_requested <- result[[5]][[5]]
  
} #End of else statement testing reportDone = "done"

#Convert from JSON to list
data <- result[[5]]$data #Just the data portion of the JSON result

#Returns total by element (e.g. pageviews by page)
totals_by_element <- ldply(lapply(data, "[", c("name", "counts")), quickdf)
totals_by_element$counts <- as.numeric(totals_by_element$counts)
names(totals_by_element) <- c("name", metric_requested) #add title to "counts"

#Create a table by page by day
breakdown <- lapply(data, "[[", "breakdown") #Just the in-page info

#initialize empty data frame, loop through and create table
granular_table <- data.frame()
for(element in 1:length(data)) {
    temp <- ldply(breakdown[[element]], quickdf)
    temp <- cbind(totals_by_element[element,"name"], temp) #add element as row name
    granular_table <- rbind(granular_table, temp) #append temp to results table
}


#Check to see if enough columns for hour
if(anomalyDetection== 1){
  for(i in 6:10){
    granular_table[[i]] <- as.numeric(granular_table[[i]])
  }
  names(granular_table) <- c(element_requested, "name", "year", "month", "day",metric_requested, paste(metric_requested, "_upper", sep=""), paste(metric_requested, "_lower", sep=""), paste(metric_requested, "_forecast", sep=""), paste(metric_requested, "_forselectedelements", sep=""))  
} else if(ncol(granular_table) == 8){
  for(i in 7:8){
    granular_table[[i]] <- as.numeric(granular_table[[i]])
  }  
  names(granular_table) <- c(element_requested, "name", "year", "month", "day","hour", metric_requested, paste(metric_requested, "_forselectedelements", sep=""))
} else {
  for(i in 6:7){
    granular_table[[i]] <- as.numeric(granular_table[[i]])
  }  
    names(granular_table) <- c(element_requested, "name", "year", "month", "day", metric_requested, paste(metric_requested, "_forselectedelements", sep=""))
}

granular_table$segment <- segment_requested #append segment to data frame

return(granular_table) #return table

} #End function bracket

