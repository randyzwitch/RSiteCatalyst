#' QueuePathing
#'
#' Helper function to run a pathed Report
#'
#' @param reportsuite.id report suite id
#' @param date.from start date for the report (YYYY-MM-DD)
#' @param date.to end date for the report (YYYY-MM-DD)
#' @param metric single metric to include in the report (usually 'pageviews')
#' @param element single pathed element (usually 'page')
#' @param pattern character vector of items in the path (up to 3)
#' use "::anything::" as a wildcard. For example c("Home","::anything::","::anything::") will return all paths that start with the home page,
#' c("::anything::","Home","::anything::") will return the previous and next pages from the home page, and
#' c("::anything::","::anything::","Home") will return the two previous pages leading to the home page. 
#' @param top number of rows to return (defaults to 1000)
#' @param start start row if you do not want to start at #1
#' @param segment.id id of Adobe Analytics segment to retrieve the report for
#' @param expedite set to TRUE to expedite the processing of this report
#'
#' @return Flat data frame containing checkpoints and metrics for each step
#'
#' @export

QueuePathing <- function(reportsuite.id, date.from, date.to, metric, element, pattern,
                        top=1000, start=1,
                        segment.id='', expedite=FALSE) {
  
  # build JSON description
  # we have to use jsonlite:::as.scalar to force jsonlist not put strings into single-element arrays
  report.description <- c()
  report.description$reportDescription <- c(data.frame(matrix(ncol=0, nrow=1)))
  report.description$reportDescription$dateFrom <- jsonlite:::as.scalar(date.from)
  report.description$reportDescription$dateTo <- jsonlite:::as.scalar(date.to)
  report.description$reportDescription$reportSuiteID <- jsonlite:::as.scalar(reportsuite.id)
  if(segment.id!="") { 
    report.description$reportDescription$segment_id <- jsonlite:::as.scalar(segment.id) 
  }
  if(expedite!=FALSE) { 
    report.description$reportDescription$expedite <- jsonlite:::as.scalar(expedite) 
  }
  report.description$reportDescription$metrics = data.frame(id = metric)
  report.description$reportDescription$elements = list(list(id = jsonlite:::as.scalar(element), 
                                                            top = jsonlite:::as.scalar(top), 
                                                            startingWith = jsonlite:::as.scalar(start), 
                                                            pattern = as.list(pattern)))

  report.data <- JsonQueueReport(toJSON(report.description))

  return(report.data) 

}  
