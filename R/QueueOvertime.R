#QueueOvertime report
#QueueOvertime supports one breakdown element with multiple element values, 
#multiple metrics, and single segment



#' Run a QueueOvertime Report
#' 
#' A QueueOvertime report is a report where the only granularity allowed is
#' time. This report allows for a single report suite, time granularity,
#' multiple metrics, and a single segment. It is similar to the "Key Metrics"
#' report or a Custom Event report within the SiteCatalyst interface.
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
#' @param reportSuiteID Report Suite ID
#' @param dateFrom Report Start Date in "YYYY-MM-DD" format
#' @param dateTo Report End Date in "YYYY-MM-DD" format. Should be less than or
#' equal to current date if using anomaly detection.
#' @param metrics One or more metrics
#' @param dateGranularity Optional. "Day", "Week", "Month", "Quarter" or "Year"
#' (case-insensitive).  If no granularity specified, a single row of data
#' returned as sum of metrics for entire time period.
#' @param segment_id Optional. If no segment_id is specified, metrics will be
#' reported for all visitors.
#' @param anomalyDetection Optional. Use value of "1" to get anomaly detection
#' results. Results only returned by API for 'Day' granularity.
#' @param currentData Optional. Use value of "1" to get current data results.
#' Only needed when dateTo is greater than or equal to the current day.
#' @param maxTries Optional. Provide integer value for the max number of API
#' attempts you want to try retrieve the report before function errors out.
#' Defaults to 120.
#' @param waitTime Optional. Provide integer value for the number of seconds
#' between tries to API to try retrieve the report. Defaults to 5 seconds.
#' @return Data Frame
#' @seealso \code{\link{GetAvailableMetrics}} \cr \code{\link{GetSegments}}
#' @keywords QueueOvertime
#' @examples
#' 
#' \dontrun{
#' 
#' #Daily granularity for Loyal_Visitors segment (all arguments used)
#' loyal_visitors_feb_daily <- 
#' QueueOvertime("keystonerandy", "2013-02-01", "2013-07-28",
#' metrics = c("pageviews", "visits", "event2"), "day", "Loyal_Visitors", "1")
#' 
#' 
#' #No granularity using empty string in dateGranularity position
#' loyal_visitors_feb_overall <- 
#' QueueOvertime("keystonerandy", "2013-02-01", "2013-02-28",
#' metrics = c("pageviews", "visits", "event2"), "", "Loyal_Visitors")
#' 
#' 
#' #Minimum number of arguments, single row containing sum of pageviews
#' pageviews_feb <- 
#' QueueOvertime("keystonerandy", "2013-02-01", "2013-02-28", "pageviews")
#'    }
#' 
#' @export QueueOvertime
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

counts_df <- as.data.frame(lapply(counts_df, FUN= function(x) as.numeric(x))) #convert to numeric



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
  
  end_result <- cbind(rows_df, counts_df, ub_df, forecasts_df, lb_df)
  
  return(end_result) #return after anomaly parsing
} #End parsing anomalyDetection

end_result <- cbind(rows_df, counts_df)

return(end_result) #append rows info with counts if not anomaly parsing

} #End function bracket

