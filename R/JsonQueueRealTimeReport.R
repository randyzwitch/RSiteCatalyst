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

  print("WARNING: Realtime reports are not yet parsed, this returns raw data from the API.")

  report <- ApiRequest(body=report.description,func.name="Report.GetRealTimeReport")

  return(report)

  # report.type <- report.data$report$type
  # print(paste("Received realtime",report.type,"report."))

  # # Check if there is any data to parse
  # if(length(report.data$report$data)>0) {
  #   report.parsed = switch(report.type,
  #     ranked={ParseRanked(report.data)},
  #     trended={ParseTrended(report.data)},
  #     pathing={ParsePathing(report.data)},
  #     fallout={ParseFallout(report.data)},
  #     overtime={ParseOvertime(report.data)}
  #   )
  # } else {
  #   print("Warning: Your report definition returned an empty data set.")
  #   report.parsed = data.frame()
  # }

  # # check if we have a segment ID and append it to the frame for visibility
  # if(!is.null(report.data[["report"]][["segmentID"]])) {
  #   report.parsed$segment.id <- report.data$report$segmentID
  # }

  # return(report.parsed)

}