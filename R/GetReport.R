#' @name GetReport
#'
#' @title Get EnQueued Report by report ID
#'
#' @details This is a function for advanced users, after you've enqueued multiple reports
#' and want to get one of them when it's ready.
#'
#' @description Get a single report by report id, this allow asynchronous way of getting reports.
#'
#' @param report.id report id that's returned by QueueTrended and other functions while used with enqueueOnly parameter set to TRUE
#' @param interval.seconds How long to wait between attempts
#' @param max.attempts Number of API attempts before stopping
#' @param print.attempts Print each attempt for fetching data
#' @param format "csv" or "json"
#' @param page Page Number of Resuts (QueueDataWarehouse only)
#'
#' @importFrom jsonlite toJSON unbox
#'
#' @return Data frame
#'
#' @examples
#' \dontrun{
#'
#' custom_report <- GetReport(12345678)
#'
#' }
#'
#' @export
#'

GetReport <- function(report.id,interval.seconds=10,max.attempts=3,print.attempts=TRUE,format="json", page = 0) {

  request.body <- c()
  request.body$reportID <- unbox(report.id)
  request.body$format <- unbox(format)
  if(page > 0){
    request.body$page <- unbox(page)
  }

  report.data <- ApiRequest(body=toJSON(request.body),func.name="Report.Get",interval.seconds=interval.seconds,max.attempts=max.attempts,print.attempts=print.attempts,format=format)

  if(format!="csv") {
    report.type <- report.data$report$type
  } else {
      report.type <- NULL
  }
  print(paste("Received",report.type,"report."))
  #return (ParseOvertime(report.data))
  # Check if there is any data to parse
  # if(is.null(report.data$report$type)){
  #   
  #   report.parsed = ParseRanked(report.data)
  #     
  # } else 
    if(is.null(report.type)){
      report.parsed = ParseDW(report.data, format)
    } else if(length(report.data$report$data)>0) {
    report.parsed = switch(report.type,
                           ranked={ParseRanked(report.data)},
                           trended={ParseTrended(report.data)},
                           pathing={ParsePathing(report.data)},
                           fallout={ParseFallout(report.data)},
                           overtime={ParseOvertime(report.data)},
                           summary={ParseSummary(report.data)}
    )
  } else {
    print("Warning: Your report definition returned an empty data set.")
    report.parsed = data.frame()
  }

  # check if we have a segment ID and append it to the frame for visibility
  if(format!="csv") {
    if(!is.null(report.data[["report"]][["segmentID"]])) {
      report.parsed$segment.id <- report.data$report$segmentID
    }
  }

  return(report.parsed)
}
