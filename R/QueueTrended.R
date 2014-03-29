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
#' @param selected list of specific items (of the first element) to include in the report - e.g. c("www:home","www:search","www:about")
#' @param date.granularity time granularity of the report (year/month/week/day/hour), default to 'day'
#' @param segment.id id of Adobe Analytics segment to retrieve the report for
#' @param segment.inline inline segment definition
#' @param anomaly.detection  set to TRUE to include forecast data (only valid for day granularity with small date ranges)
#' @param data.current TRUE or FALSE - whether to include current data for reports that include today's date
#' @param expedite set to TRUE to expedite the processing of this report
#'
#' @importFrom jsonlite toJSON unbox
#'
#' @return Flat data frame containing datetimes and metric values
#'
#' @export

QueueTrended <- function(reportsuite.id, date.from, date.to, metrics, elements,
                        top=0,start=0,selected=c(),
                        date.granularity='day', segment.id='', segment.inline='', anomaly.detection=FALSE,
                        data.current=FALSE, expedite=FALSE,interval.seconds=5,max.attempts=120) {
  
  if(anomaly.detection==TRUE && length(elements)>1) {
    print("Warning: Anomaly detection will not be used, as it only works for a single element.")
    anomaly.detection <- FALSE
  }

  if(anomaly.detection==TRUE && date.granularity!='day') {
    print("Warning: Anomaly detection will not be used, as it only works with 'day' date granularity.")
    anomaly.detection <- FALSE
  }

  # build JSON description
  # we have to use unbox to force jsonlist not put strings into single-element arrays
  report.description <- c()
  report.description$reportDescription <- c(data.frame(matrix(ncol=0, nrow=1)))
  report.description$reportDescription$dateFrom <- unbox(date.from)
  report.description$reportDescription$dateTo <- unbox(date.to)
  report.description$reportDescription$reportSuiteID <- unbox(reportsuite.id)
  report.description$reportDescription$dateGranularity <- unbox(date.granularity)
  if(segment.inline!="") {
    report.description$reportDescription$segments <- list(segment.inline)
  }
  if(segment.id!="") { 
    report.description$reportDescription$segment_id <- unbox(segment.id) 
  }
  if(anomaly.detection==TRUE) { 
    report.description$reportDescription$anomalyDetection <- unbox(anomaly.detection) 
  }
  if(data.current==TRUE) { 
    report.description$reportDescription$currentData <- unbox(data.current) 
  }
  if(expedite==TRUE) { 
    report.description$reportDescription$expedite <- unbox(expedite)
  }
  report.description$reportDescription$metrics = data.frame(id = metrics)

  # build up each element with selections
  elements.formatted <- list()
  for(i in 1:length(elements)) {
    element <- elements[[i]]
    if(length(selected)>0&&i==1){
      working.element <- list(id = unbox(element), selected=selected,top=unbox(top),startingWith=unbox(start))
    } else {
      working.element <- list(id = unbox(element),top=unbox(top),startingWith=unbox(start))
    }
    if(length(elements.formatted)>0) {
      elements.formatted <- rbind(elements.formatted,working.element)
    } else {
      elements.formatted <- working.element
    }
  }
  report.description$reportDescription$elements <- list(elements.formatted)

  report.data <- JsonQueueReport(toJSON(report.description),interval.seconds=interval.seconds,max.attempts=max.attempts)

  return(report.data) 

}  
