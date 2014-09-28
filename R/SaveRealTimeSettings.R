#' Save Configuration for Real-Time Report
#' 
#' Sets the metrics and elements (dimensions) on which you want real time
#' reports enabled via list objects created by BuildRealTimeReportStructure. 
#' Realtime configuration changes take 15 minutes to be reflected in reports.
#' 
#' 
#' SaveRealTimeSettings should be called each time you want to modify the
#' structure of your real-time reports. If you are unsure of your current setup
#' of your real-time reports, use GetRealTimeSettings to find out your
#' current setup. 
#' 
#' WARNING: This function allows you to change the settings in your Adobe 
#' Analytics UI for all users, so be sure this is what you want to do. Additionally,
#' submitting this function with only one report will mean other reports are deleted,
#' you're NOT just changing a single report.
#' 
#' NOTE: If the ui_report parameter is set to false, you must save at least one 
#' element and one metric or the configuration will be invalid, even though an 
#' error does not occur. If the ui_report parameter is set to true, you must 
#' save three elements and one metric or you will receive an error.
#' 
#' Changes can take up to 15 minutes to be reflected.
#' 
#' @param reportsuite.ids Report Suite ID
#' @param report1 Real Time Report 1
#' @param report2 Real Time Report 2
#' @param report3 Real Time Report 3
#' 
#' @return Message returned to console
#' @seealso \code{\link{GetRealTimeSettings}} \cr
#' @seealso \code{\link{BuildRealTimeReportStructure}} \cr
#' @keywords SaveRealTimeSettings
#' @examples
#' 
#' \dontrun{
#' 
#' saverealtime <- SaveRealTimeSettings("your-report-suite", report1, report2, report3)
#' 
#'    }
#' 
#' @export
#' 
#' 
SaveRealTimeSettings <- function (reportsuite.ids="", report1=list(), report2 = list(), report3 = list()) {
  
  #Check to see if length(report1) > 0, report error otherwise
  if(length(report1) == 0){
    stop("report1 must be specified")
  } else {
    reports <- list(report1)
  }
  
  #If report2 not blank, add it to list. List can only be length 1 at this point
  if(length(report2) > 0){
    reports[[2]] <- report2
  }
  
  #If report3 not blank, add it to list. Need to determine length, since report2 might not have been
  #populated
  if(length(report3) > 0 && length(report2) > 0){
    reports[[3]] <- report3
  } else{
    reports[[2]] <- report3
  }
  
  
  #Convert to JSON
  request.body <- toJSON(list(real_time_settings=reports, rsid_list=list(unbox(reportsuite.ids))))

  #skip.queue parameter returns raw response
  results <- ApiRequest(body=request.body,func.name="ReportSuite.SaveRealTimeSettings", skip.queue=TRUE)
  
  #If a valid response returned, then print success; else, print code/message
  if(results$status_code == 200){
    print("Configuration Saved. Per API documentation, it can take up to 15mins for report to become active")
  } else{
    stop(sprintf("Status code %s: %s", results$status_code, results$statusmessage))
  }

} #End function bracket

