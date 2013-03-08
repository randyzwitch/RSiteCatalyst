#QueueOvertime report
#QueueOvertime supports one breakdown element with multiple element values, 
#multiple metrics, and single segment

QueueOvertime <- function(reportSuiteID, dateFrom, dateTo, metrics, dateGranularity="", segment_id="") {
  
  #Loop over the metrics list, appending proper curly braces
  metrics_conv <- lapply(metrics, function(x) paste('{"id":', '"', x, '"', '}', sep=""))
  #Collapse the list into a proper comma separated string
  metrics_final <- paste(metrics_conv, collapse=", ") 
  
  #Build json string to request report
  json_request <- sprintf('{"reportDescription":{
    "reportSuiteID":"%s",
    "dateFrom":"%s",
    "dateTo":"%s",
    "dateGranularity":"%s",
    "metrics": [%s],
    "segment_id": "%s"
  }
}', reportSuiteID, dateFrom, dateTo, dateGranularity, metrics_final, segment_id)

#1.  Send API request to build report- QueueOvertime
json_queue <- postRequest("Report.QueueOvertime", json_request)

if(json_queue$status == 200) {
#Convert JSON to list
queue_resp <- content(json_queue)
} else {
  return(jsonResponseError(json_queue$status))
}

#If response returns an error, return error message. Else, continue with
#capturing report ID
if(queue_resp[1] != "queued" ) {
  return(print("Error: Likely a syntax error or missing required argument to QueueOvertime function"))
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
  
} #End of else statement testing reportDone = "done"

#Convert from JSON to data frame, not currently working for multiple metrics
data <- result[[5]]$data #Just the data portion of the JSON result

hour_flag <- as.numeric(str_count(tolower(json_request), "hour")) #find out if hour granularity
if(hour_flag > 0){
rows <- lapply(data, "[", c("name", "year", "month", "day", "hour")) #Just the breakdowns
} else {
rows <- lapply(data, "[", c("name", "year", "month", "day")) #Just the breakdowns  
}

rows_df <- ldply(rows, quickdf) #breakdowns as DF
rows_df <- cbind(rows_df, segment=result[[5]]$segment_id) #add segment to df

counts <- lapply(data, "[[", "counts") # Just the "counts" column
counts_df <- ldply(counts, quickdf) # counts as DF
names(counts_df) <- lapply(result[[5]]$metrics, "[[", "id") #assign names to counts_df

return(cbind(rows_df, counts_df)) #append rows info with counts

} #End function bracket

