#' @name JsonQueueRealTimeReport
#' 
#' @title Create Real-Time Report from JSON
#'
#' @description Generic interface to validate, queue and retrieve a realtime report from the API
#' 
#' @details This is a function for advanced users, for the case where a user feels
#' that submitting a JSON request would be easier than using one of the pre-defined
#' functions from RSiteCatalyst
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
#' @export
#'

JsonQueueRealTimeReport <- function(report.description) {

  if(!ValidateReport(report.description)) {
    stop("ERROR: Invalid report description.")
  }
  
  report <- ApiRequest(body=report.description,func.name="Report.GetRealTimeReport")

  return(report)

}