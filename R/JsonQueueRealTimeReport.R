#' @name JsonQueueRealTimeReport
#' 
#' @title Create Real-Time Report from JSON
#'
#' @description Generic interface to validate, queue and retrieve a realtime report from the API
#' 
#' @details Used in GetRealTimeReport
#' 
#' @param report.description JSON report description
#'
#' @importFrom jsonlite toJSON
#'
#' @return Data frame
#' 
#' @examples
#' \dontrun{
#' 
#' custom_report <- JsonQueueRealTimeReport('valid Adobe Analytics API JSON string')
#'
#' }
#'
#' @export
#' @keywords internal

JsonQueueRealTimeReport <- function(report.description) {

   if(!ValidateReport(report.description)) {
     stop("ERROR: Invalid report description.")
   }
  
  report <- ApiRequest(body=report.description,func.name="Report.Run")
  
  #Should I just return report$report$data directly?
  #Single element returns a data frame where the breakdown column in a nested list
  #Multiple elements fails with error complaining about 'top' argument
  return(report)

}