#QueueOvertime report
#QueueOvertime supports one breakdown element with multiple element values, 
#multiple metrics, and single segment

QueueOvertime <- function(reportSuiteID, dateFrom, dateTo, metrics, dateGranularity="", segment_id="", anomalyDetection="", currentData="", maxTries= 120, waitTime= 5) {

  if(anomalyDetection == "1" & dateGranularity!="day") {
    stop("Error: Anomaly Detection only provided for day granularity")
  }


  
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
    "segment_id": "%s",
    "anomalyDetection": "%s",
    "currentData": "%s"
  }, "validate": true
}', reportSuiteID, dateFrom, dateTo, dateGranularity, metrics_final, segment_id, anomalyDetection, currentData)

#1.  Send API request to build report- QueueOvertime
json_queue <- postRequest("Report.QueueOvertime", json_request)

if(json_queue$status == 200) {
#Convert JSON to list
queue_resp <- content(json_queue)
} else {
  stop(jsonResponseError(json_queue$status))
  
}

#If response returns an error, return error message. Else, continue with
#capturing report ID
if(queue_resp[1] != "queued" ) {
  stop("Error: Likely a syntax error or missing required argument to QueueOvertime function")
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
counts_df <- as.data.frame(apply(counts_df, MARGIN=2, FUN= function(x) as.numeric(x))) #convert to numeric
  
names(counts_df) <- lapply(result[[5]]$metrics, "[[", "id") #assign names to counts_df

#Parse anomalyDetection if requested  

if(anomalyDetection == "1" & dateGranularity == "day") {

  ub <- lapply(data, "[[", "upper_bounds") # Upper Bounds
  ub_df <- ldply(ub, quickdf) # upper bound as DF
  ub_df <- as.data.frame(apply(ub_df, MARGIN=2, FUN= function(x) as.numeric(x))) #convert to numeric
  names(ub_df) <- lapply(lapply(result[[5]]$metrics, "[[", "id"), function(x) paste(x, "_upper", sep=""))
  
  forecasts <- lapply(data, "[[", "forecasts") # forecasted value
  forecasts_df <- ldply(forecasts, quickdf) # forecast as DF
  forecasts_df <- as.data.frame(apply(forecasts_df, MARGIN=2, FUN= function(x) as.numeric(x))) #convert to numeric
  names(forecasts_df) <- lapply(lapply(result[[5]]$metrics, "[[", "id"), function(x) paste(x, "_forecast", sep=""))
  
  lb <- lapply(data, "[[", "lower_bounds") # lower Bounds
  lb_df <- ldply(lb, quickdf) # lower bound as DF
  lb_df <- as.data.frame(apply(lb_df, MARGIN=2, FUN= function(x) as.numeric(x))) #convert to numeric
  names(lb_df) <- lapply(lapply(result[[5]]$metrics, "[[", "id"), function(x) paste(x, "_lower", sep=""))
  
  return(cbind(rows_df, counts_df, ub_df, forecasts_df, lb_df)) #return after anomaly parsing
} #End parsing anomalyDetection

return(cbind(rows_df, counts_df)) #append rows info with counts if not anomaly parsing

} #End function bracket

