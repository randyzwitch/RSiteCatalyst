#QueueTrended report
#Submits a Trended report request. Trended reports display trends for a single metric 
#(revenue, orders, views, and so forth) and element 
#(product, category, page, and so forth).

QueueTrended <- function(reportSuiteID, dateFrom, dateTo, dateGranularity, metric, element, top="", startingWith="", selected= "", segment_id="") {

  #Error check to see if function call using both parameters
if(top!= "" && selected != "") {
  
  return(print("Error:  Use 'top' or 'startingWith' arguments, not both"))
}
  
#Build JSON request for "Top" functionality

if(top != "") {
  json_request <- sprintf(
    '{"reportDescription":
    {"reportSuiteID" :"%s",
     "dateFrom":"%s",
     "dateTo":"%s",
     "dateGranularity":"%s",
     "metrics": [{"id":"%s"}],
     "elements" : [{"id":"%s", "top": "%s", "startingWith": "%s" }],
     "segment_id": "%s"
    }
}', reportSuiteID, dateFrom, dateTo, dateGranularity, metric, element, top, startingWith, segment_id)
  
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
     "segment_id": "%s"
    }
}', reportSuiteID, dateFrom, dateTo, dateGranularity, metric, element, selected, segment_id)
  
}

#1.  Send API request to build report- QueueOvertime
json_queue <- postRequest("Report.QueueTrended", json_request)

if(json_queue$status == 200) {
  #Convert JSON to list
  queue_resp <- content(json_queue)
} else {
  return(jsonResponseError(json_queue$status))
}

#If response returns an error, return error message. Else, continue with
#capturing report ID
if(queue_resp[1] != "queued" ) {
  return(print("Error: Likely a syntax error in arguments to QueueTrended function"))
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
  metric_requested <- result[[5]][[4]][[1]]$id
  element_requested <- result[[5]][[3]][[1]]$id
  segment_requested <- result[[5]][[5]]
  
} #End of else statement testing reportDone = "done"

#Convert from JSON to list
data <- result[[5]]$data #Just the data portion of the JSON result

#Returns total by element (e.g. pageviews by page)
totals_by_element <- ldply(lapply(data, "[", c("name", "counts")), quickdf)
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
if(ncol(granular_table) == 8){
names(granular_table) <- c(element_requested, "name", "year", "month", "day","hour", metric_requested, paste(metric_requested, "_forselectedelements", sep=""))
} else {
names(granular_table) <- c(element_requested, "name", "year", "month", "day", metric_requested, paste(metric_requested, "_forselectedelements", sep=""))
}

granular_table$segment <- segment_requested #append segment to data frame

return(granular_table) #return table

} #End function bracket

