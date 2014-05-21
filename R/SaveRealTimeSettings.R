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
#' Changes can take up to 15 minutes to be reflected.
#' 
#' @param reportsuite.ids Report Suite ID
#' @param report1 Real Time Report 1
#' @param report2 Real Time Report 2
#' @param report3 Real Time Report 3
#' 
#' @return Message returned to console
#' @seealso \code{\link{GetRealTimeSettings}} \cr
#' @keywords SaveRealTimeSettings
#' @examples
#' 
#' \dontrun{
#' 
#'    }
#' 
#' @export
#' 
SaveRealTimeSettings <- function (reportsuite.ids="", report1=list(), report2 = list(), report3 = list()) {
  
  #Check to see if length(report1) > 0, report error otherwise
  if(length(report1) == 0){
    stop("report1 must be specified")
  }
  
  
  
  #Convert to JSON
  request.body <- toJSON(list(real_time_settings=list(report1, report2, report3), rsid_list=reportsuite.ids))

  #skip.queue parameter returns raw response
  results <- ApiRequest(body=request.body,func.name="ReportSuite.SaveRealTimeSettings", skip.queue=TRUE)
  
  #If a valid response returned, then print success; else, print code/message
  if(results$status_code == 200){
    print("Configuration Saved. Per API documentation, it can take up to 15mins for report to become active")
  } else{
    warning(sprintf("Status code %s: %s", results$status_code, results$statusmessage))
  }

} #End function bracket