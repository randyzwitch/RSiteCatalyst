#' @name SubmitJsonQueueReport
#' 
#' @title Create Queue Report from JSON
#' 
#' @details This is a function for advanced users, for the case where a user feels
#' that submitting a JSON request would be easier than using one of the pre-defined
#' functions from RSiteCatalyst
#'
#' @description Generic interface to validate, queue and retrieve a report from the API
#'
#' @param report.description JSON report description
#' @param interval.seconds How long to wait between attempts
#' @param max.attempts Number of API attempts before stopping
#' @param validate Weather to submit report definition for validation before requesting the data.
#'
#' @importFrom jsonlite toJSON unbox
#'
#' @return Data frame
#' 
#' @examples
#' \dontrun{
#' 
#' custom_report <- SubmitJsonQueueReport('valid Adobe Analytics API JSON string')
#'
#' }
#' 
#' @export
#'

SubmitJsonQueueReport <- function(report.description,interval.seconds=5,max.attempts=120,validate=TRUE) {

  # Determine if we should validate the definition
  if(validate) {
    if(!ValidateReport(report.description)) {
      stop("ERROR: Invalid report description.")
    }
  }
  
  response <- ApiRequest(body=report.description,func.name="Report.Queue")
  
  #If response returns an error, return error message. Else, continue with capturing report ID
  report.id <- as.numeric(response$reportID)
  if(!is.numeric(report.id)) {
    stop("ERROR: the API validated the report, but did not return a report ID")
  }

  request.body <- c()
  request.body$reportID <- unbox(report.id)
  report.data <- ApiRequest(body=toJSON(request.body),func.name="Report.Get",interval.seconds=interval.seconds,max.attempts=max.attempts,print.attempts=TRUE)

  report.type <- report.data$report$type
  print(paste("Received",report.type,"report."))

  # Check if there is any data to parse
  if(length(report.data$report$data)>0) {
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
  if(!is.null(report.data[["report"]][["segmentID"]])) {
    report.parsed$segment.id <- report.data$report$segmentID
  }

  return(report.parsed)

}
