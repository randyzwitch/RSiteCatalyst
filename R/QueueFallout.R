#' QueueFallout
#'
#' Helper function to run a QueueFallout Report
#'
#' @param reportsuite.id report suite id
#' @param date.from start date for the report (YYYY-MM-DD)
#' @param date.to end date for the report (YYYY-MM-DD)
#' @param metrics list of metrics to include in the report
#' @param element single pathed element (usually 'page')
#' @param checkpoints character vector of checkpoints in the fallout path (e.g. c("Home","Contact","Thank You"))
#' @param segment.id id of Adobe Analytics segment to retrieve the report for
#' @param expedite set to TRUE to expedite the processing of this report
#'
#' @importFrom jsonlite toJSON unbox
#'
#' @return Flat data frame containing checkpoints and metrics for each step
#'
#' @export

QueueFallout <- function(reportsuite.id, date.from, date.to, metrics, element, checkpoints,
                        segment.id='', expedite=FALSE) {
  
  # build JSON description
  # we have to use unbox to force jsonlist not put strings into single-element arrays
  report.description <- c()
  report.description$reportDescription <- c(data.frame(matrix(ncol=0, nrow=1)))
  report.description$reportDescription$dateFrom <- unbox(date.from)
  report.description$reportDescription$dateTo <- unbox(date.to)
  report.description$reportDescription$reportSuiteID <- unbox(reportsuite.id)
  if(segment.id!="") { 
    report.description$reportDescription$segment_id <- unbox(segment.id) 
  }
  if(expedite!=FALSE) { 
    report.description$reportDescription$expedite <- unbox(expedite) 
  }
  report.description$reportDescription$metrics = data.frame(id = metrics)
  report.description$reportDescription$elements = list(list(id = unbox(element), checkpoints = checkpoints))

  report.data <- JsonQueueReport(toJSON(report.description))

  return(report.data) 

}  
