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
#' @param validate whether to submit report definition for validation before requesting the data.
#' @param enqueueOnly only enqueue the report, don't get the data. returns report id, which you can later use to get the data
#' @param format "csv" or "json"
#'
#' @importFrom jsonlite toJSON unbox
#'
#' @return Data frame or report id, if enqueueOnly is TRUE
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

SubmitJsonQueueReport <-
  function(report.description,
           interval.seconds = 5,
           max.attempts = 120,
           validate = TRUE,
           enqueueOnly = FALSE,
           format = "json") {
    # Determine if we should validate the definition
    if (validate) {
      if (!ValidateReport(report.description)) {
        stop("ERROR: Invalid report description.")
      }
    }

    response <-
      ApiRequest(body = report.description, func.name = "Report.Queue")

    #If response returns an error, return error message. Else, continue with capturing report ID
    report.id <- as.numeric(response$reportID)
    if (!is.numeric(report.id)) {
      stop("ERROR: the API validated the report, but did not return a report ID")
    }

    if (enqueueOnly) {
      return(report.id)
    } else{
      return(
        GetReport(
          report.id,
          interval.seconds = interval.seconds,
          max.attempts = max.attempts,
          print.attempts = TRUE,
          format = format
        )
      )
    }
  }
