#' ValidateReport
#'
#' Internal function - Calls the API and attempts to validate a report description.
#'
#' @param report.description JSON report description
#' @param interval.seconds Time to wait between attempts to validate the report (defaults to 2 seconds)
#' @param max.attempts Max number of attempts to make to validate the report (defaults to 1)
#'
#' @return TRUE/FALSE depending on whether the report is valid or not
#'
#' @export
#' @keywords internal

ValidateReport <- function(report.description,interval.seconds=0,max.attempts=1) {

  response <- ApiRequest(body=report.description,func.name="Report.Validate")

  if(isTRUE(response$valid)) {
    return(TRUE)
  } else {
    print(paste("ERROR:",response$error_description))
    return(FALSE)
  }
}