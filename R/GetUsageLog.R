<<<<<<< HEAD
#' GetUsageLog
#'
#' Gets the Adobe Analytics usage log for all users within the specified date range.
#' 
#' @title Get Admin Actions, Logins, and Reports Accessed
#'
#' @param date.from Log start date (YYYY-MM-DD)
#' @param date.to Log end date (YYYY-MM-DD)
#' @param localtime Whether to change datetimes from UTC to local time
#'
#' @importFrom jsonlite toJSON unbox
#' @importFrom plyr rename
#'
#' @return data frame with: datetime, login, event_num, event_type, ip_address, report_suite, event_details
#'
#' @family internal
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' usagelog <- GetUsageLog("2014-01-01","2014-01-31")
#' }

GetUsageLog <- function(date.from=as.character(Sys.Date()-1),date.to=as.character(Sys.Date()),localtime=FALSE) {

  request.body <- c()
  request.body$date_from <- unbox(date.from)
  request.body$date_to <- unbox(date.to)

  usagelog <- ApiRequest(body=toJSON(request.body),func.name="Logs.GetUsageLog")

  if(localtime==TRUE) {
    usagelog$timestamp <- as.POSIXlt(as.numeric(usagelog$timestamp), origin="1970-01-01")
  } else {
    usagelog$timestamp <- as.POSIXlt(as.numeric(usagelog$timestamp), origin="1970-01-01", tz="UTC")
=======
#GetUsageLog - By report suite, get all user actions for a period of time

  


#' Admin Actions, Logins, and Reports Accessed
#' 
#' Creates data frame containing all Report Suite actions (Admin, Logins,
#' Reports Accessed) during a given time period.
#' 
#' This report is administrative in nature, for users interested in
#' understanding how internal constituents are using SiteCatalyst. This report
#' is also a record of all Admin changes to an account.
#' 
#' @param date_from Optional: Start date of report, in "YYYY-MM-DD" format. If
#' argument not set in function call, defaults to Sys.Date() - 1
#' @param date_to Optional: End date of report, in "YYYY-MM-DD" format. If
#' argument not set in function call, defaults to Sys.Date()
#' @param localtime Optional. If TRUE, timestamp converted to local time.
#' Otherwise, UNIX time in seconds.
#' @return Data Frame
#' @keywords admin
#' @examples
#' 
#' \dontrun{
#' 
#'     #Timestamp in local time
#'     GetUsageLog("2013-01-01", "2013-02-13", TRUE)
#'     
#'     #UNIX time in seconds for timestamp
#'     GetUsageLog("2013-01-01", "2013-02-13")
#'  }   
#' 
#' @export GetUsageLog
GetUsageLog <- function(date_from=as.character(Sys.Date()-1), date_to=as.character(Sys.Date()), localtime=FALSE) {

#Make sure dates are in right order
if(date_from > date_to) {
  stop("'date_from' is more recent than 'date_to'")
}
  
#Make API call - Numerous parameters left out, this returns all info for all users, instead of single user
json <- postRequest("Logs.GetUsageLog", paste('{"date_from":', toJSON(date_from), "," , '"date_to":', toJSON(date_to), '}'))


#Validate that API answer is Success
if(json$status != 200) {
  stop(jsonResponseError(json$status))
} else {
  #Convert JSON raw into list
  results <- content(json)
}

if(length(results) > 0) {
#Return formatted dataframe
  temp <- ldply(results, quickdf)
  
  #Convert to local time if flag set to TRUE
  if(localtime) {
  temp$timestamp <- as.POSIXct(as.numeric(temp$timestamp), origin="1970-01-01")
>>>>>>> master
  }
  usagelog <- rename(usagelog, c("timestamp"="datetime"))

  return(usagelog)

}
