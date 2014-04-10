#GetAdminConsoleLog  - By report suite, get all admin actions for a period of time
#There's a limit of 1000 records returned, so put in documentation to be careful
  


#' Admin Actions to Report Suite Settings
#' 
#' Get all Report Suite actions made by Admins during a given time period.
#' 
#' DEPRECATED. This method removed by Adobe, with no replacement.
#' 
#' This report is administrative in nature, for users interested in
#' understanding how changes have been made to the SiteCatalyst implementation.
#' 
#' Note that this API is limited to returning 1000 results per call. This is
#' important to remember when passing a list of report suites or long time
#' periods.
#' 
#' @param report_suites One or more Report Suites
#' @param start_date Optional: Start date of report, in "YYYY-MM-DD" format. If
#' argument not set in function call, defaults to Sys.Date() - 1
#' @param end_date Optional: End date of report, in "YYYY-MM-DD" format. If
#' argument not set in function call, defaults to Sys.Date()
#' @return Data Frame
#' @keywords admin
#' @examples
#' 
#' \dontrun{
#' 
#' admin_console <- GetAdminConsoleLog(c("keystonerandy", 
#' "keystonejowanza", "keystonejoel"), "2013-01-01", "2013-02-20")
#' 
#' #Be careful of 1000 limit when passing entire list of rsid's
#' admin_console <- GetAdminConsoleLog(report_suites$rsid, "2012-01-01", "2013-02-20")
#' 
#'    } 
#'     
#' 
GetAdminConsoleLog <- function(report_suites, start_date=as.character(Sys.Date()-1), end_date=as.character(Sys.Date())) {
  stop("GetAdminConsoleLog removed from API by Adobe; currently, there is no replacement.")
  
  #Make sure dates are in right order
  if(start_date > end_date) {
    stop("'start_date' is more recent than 'end_date'")
  }
  
  
  #A bit of slop to format report_suites into JSON
  if(length(report_suites)>1){
    report_suites <- toJSON(report_suites)
  } else {
    report_suites <- toJSON(list(report_suites))
  }
  

#Make API call - Numerous parameters left out, this returns all info for all users, instead of single user
json <- postRequest("Logs.GetAdminConsoleLog", paste('{"start_date":', toJSON(start_date), "," , '"end_date":', toJSON(end_date), "," , '"rsid_list":', report_suites, '}'))

if(json$status != 200) {
  stop(jsonResponseError(json$status))
} else {  
#Convert JSON raw into list
results <- content(json)
}

if(length(results) > 0) {

#Return formatted dataframe
return(ldply(results, quickdf))

} else {
  warning("There are no results for the timeperiod selected")
}

}

