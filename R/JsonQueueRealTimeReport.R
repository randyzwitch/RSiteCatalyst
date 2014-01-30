#' JsonQueueRealTimeReport
#'
#' Generic interface to validate, queue and retrieve a realtime report from the API
#'
#' @param report.description JSON report description
#'
#' @importFrom jsonlite toJSON
#'
#' @return Formatted data frame
#'
#' @export

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