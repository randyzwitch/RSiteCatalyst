#' QueueTrended
#'
#' Helper function to run a Trended Report
#'
#' @param reportsuite.id report suite id
#' @param date.from start date for the report (YYYY-MM-DD)
#' @param date.to end date for the report (YYYY-MM-DD)
#' @param metrics list of metrics to include in the report
#' @param elements list of elements to include in the report
#' @param top number of rows to return
#' @param start start row if you do not want to start at #1
#' @param selected list of specific items to include in the report - e.g. list(page=c("Home","Search","About"))
#' @param date.granularity time granularity of the report (year/month/week/day/hour), default to 'day'
#' @param segment.id id of Adobe Analytics segment to retrieve the report for
#' @param segment.inline inline segment definition
#' @param anomaly.dection  set to TRUE to include forecast data (only valid for day granularity with small date ranges)
#' @param data.current TRUE or FALSE - whether to include current data for reports that include today's date
#' @param expedite set to TRUE to expedite the processing of this report
#'
#' @return Flat data frame containing datetimes and metric values
#'
#' @export

QueueTrended <- function(reportsuite.id, date.from, date.to, metrics, elements,
                        top=0,start=0,selected=list(),
                        date.granularity='day', segment.id='', segment.inline='', anomaly.detection=FALSE,
                        data.current=FALSE, expedite=FALSE) {
  
  if(anomaly.detection==TRUE && length(elements)>1) {
    print("Warning: Anomaly detection will not be used, as it only works for a single element.")
    anomaly.detection <- FALSE
  }

  if(anomaly.detection==TRUE && date.granularity!='day') {
    print("Warning: Anomaly detection will not be used, as it only works with 'day' date granularity.")
    anomaly.detection <- FALSE
  }

  # build JSON description
  # we have to use jsonlite:::as.scalar to force jsonlist not put strings into single-element arrays
  report.description <- c()
  report.description$reportDescription <- c(data.frame(matrix(ncol=0, nrow=1)))
  report.description$reportDescription$dateFrom <- jsonlite:::as.scalar(date.from)
  report.description$reportDescription$dateTo <- jsonlite:::as.scalar(date.to)
  report.description$reportDescription$reportSuiteID <- jsonlite:::as.scalar(reportsuite.id)
  report.description$reportDescription$dateGranularity <- jsonlite:::as.scalar(date.granularity)
  if(segment.inline!="") {
    report.description$reportDescription$segments <- list(segment.inline)
  }
  if(top>0) { 
    report.description$reportDescription$top <- jsonlite:::as.scalar(top) 
  }
  if(start>0) { 
    report.description$reportDescription$start <- jsonlite:::as.scalar(start) 
  }
  if(segment.id!="") { 
    report.description$reportDescription$segment_id <- jsonlite:::as.scalar(segment.id) 
  }
  if(anomaly.detection==TRUE) { 
    report.description$reportDescription$anomalyDetection <- jsonlite:::as.scalar(anomaly.detection) 
  }
  if(data.current==TRUE) { 
    report.description$reportDescription$currentData <- jsonlite:::as.scalar(data.current) 
  }
  if(expedite==TRUE) { 
    report.description$reportDescription$expedite <- jsonlite:::as.scalar(expedite)
  }
  report.description$reportDescription$metrics = data.frame(id = metrics)

  if(length(selected)>0) {
    # build up each element with selections
    elements.formatted <- list()
    for(element in elements) {
      if(length(selected[element])){
        working.element <- list(id = jsonlite:::as.scalar(element), selected=selected[element][1][[1]])
      }
      if(length(elements.formatted)>0) {
        elements.formatted <- rbind(elements.formatted,working.element)
      } else {
        elements.formatted <- working.element
      }
    }
    report.description$reportDescription$elements <- list(elements.formatted)
  } else {
    # just plug in the elements
    report.description$reportDescription$elements <- data.frame(id = elements)
  }

  report.data <- JsonQueueReport(toJSON(report.description))

  return(report.data) 

}  
