#' @details The QueueTrended report is analogous to pulling a "trended"
#' report within Adobe Reports & Analytics, but without the limitation of
#' only 5 elements as in the Adobe Reports & Analytics interface.
#'
#' Because of the Reporting API structure, this function first requests the
#' report, then checks the reporting queue to see if the report is completed,
#' and when the report returns as "done" pulls the report from the API. This
#' checking process will occur up to the specified number of times (default 120),
#' with a delay between status checks (default 5 seconds). If the report does not
#' return as "done" after the number of tries have completed, the function will return
#' an error message.
#'
#' Note: Because of the multiple argument type ("top" and "start" OR "selected"),
#' keyword arguments are generally needed towards the end of the function call
#' instead of just positional arguments.
#'
#' @description A QueueTrended report is a report where a single metric is
#' retrieved, broken down by an element such as page, eVar, prop, etc. and with
#' a time component. Within the 'element' list, either the "Top X" number of
#' elements can be received or you can specify the specific elements you are
#' interested in (such as 3 specific page names).
#'
#' @title Run a Trended Report
#'
#' @param reportsuite.id Report suite id
#' @param date.from Start date for the report (YYYY-MM-DD)
#' @param date.to End date for the report (YYYY-MM-DD)
#' @param metrics List of metrics to include in the report
#' @param elements List of elements to include in the report
#' @param top List of numbers to limit the number of rows to include (top X). eg. c(10,5)
#' @param start Start row if you do not want to start at #1
#' @param selected List of specific items (of the first element) to include in the report - e.g. c("www:home","www:search","www:about")
#' @param search List of keywords for the first specified element - e.g. c("contact","about","shop").
#' search overrides anything specified using selected
#' @param search.type String specifying the search type: 'and', or, 'or' 'not' (defaults to 'or')
#' @param date.granularity Time granularity of the report (year/month/week/day/hour), default to 'day'
#' @param segment.id Id(s) of Adobe Analytics segment to retrieve the report for
#' @param segment.inline Inline segment definition
#' @param classification SAINT classification to use in place of first element. Need to specify element AND classification.
#' @param anomaly.detection Set to TRUE to include forecast data (only valid for day granularity with small date ranges)
#' @param data.current TRUE or FALSE - whether to include current data for reports that include today's date
#' @param expedite Set to TRUE to expedite the processing of this report
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
#' report.data <- QueueTrended("your_report_suite",
#'                             "2014-01-01",
#'                             "2014-01-07",
#'                             c("visits","uniquevisitors","pageviews","event10"),
#'                             c("page","geoCountry","geoCity")
#'                             )
#'}
#' @export

QueueTrended <- function(reportsuite.id, date.from, date.to, metrics, elements,
                        top=0,start=0,selected=c(),search=c(),search.type='or',
                        date.granularity='day', segment.id='', segment.inline='', classification = character(0),
                        anomaly.detection=FALSE, data.current=FALSE, expedite=FALSE,
                        interval.seconds=5,max.attempts=120,validate=TRUE) {

  if(anomaly.detection==TRUE && length(elements)>1) {
    print("Warning: Anomaly detection will not be used, as it only works for a single element.")
    anomaly.detection <- FALSE
  }

  if(anomaly.detection==TRUE && date.granularity!='day') {
    print("Warning: Anomaly detection will not be used, as it only works with 'day' date granularity.")
    anomaly.detection <- FALSE
  }

  # build JSON description
  # we have to use unbox to force jsonlite not put strings into single-element arrays
  report.description <- c()
  report.description$reportDescription <- c(data.frame(matrix(ncol=0, nrow=1)))
  report.description$reportDescription$dateFrom <- unbox(date.from)
  report.description$reportDescription$dateTo <- unbox(date.to)
  report.description$reportDescription$reportSuiteID <- unbox(reportsuite.id)
  report.description$reportDescription$dateGranularity <- unbox(date.granularity)

  #Hack in locale, every method calls ApiRequest so this hopefully works
  #Set encoding to utf-8 as well; if someone wanted to do base64 they are out of luck
  report.description$reportDescription$locale <- unbox(AdobeAnalytics$SC.Credentials$locale)
  report.description$reportDescription$elementDataEncoding <- unbox("utf8")

  if(segment.inline!="") {
    report.description$reportDescription$segments <- list(segment.inline)
  }
  #If segment is null, apply the standard segment unbox function
    if(as.list(segment.id)[1]==''){
    report.description$reportDescription$segment_id <- unbox(segment.id)
      }
  #If segment is not null, treat it like a list of metrics.
    else{
    report.description$reportDescription$segments <- data.frame( id = segment.id)

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

  elements.formatted <- list()
  # build up each element with selections
  i <- 0
  for(element in elements) {
    i <- i + 1
    # we only put selected, search, top and startingWith for the first element
    if(i==1){
      working.element <- list(id = unbox(element),
                              top = unbox(top[1]),
                              startingWith = unbox(start))

      if(length(selected)!=0){
        working.element[["selected"]] <- selected
      }
      if(length(search)!=0){
        working.element[["search"]] <- list(type = unbox(search.type),
                                            keywords = search)
      }

    } else {
   	  # Check if the input is a vector with more than 1 element
      if(length(top)>=i){
      	# Use the matching limit value from vector
      	working.element <- list(id = unbox(element), top = unbox( top[i] ))
      } else {
      	working.element <- list(id = unbox(element), top = unbox("50000"))
      }
    }

    if(length(classification)>=i){
      working.element[["classification"]] <- unbox(classification[i])
    }

    if(length(elements.formatted)>0) {
      elements.formatted <- append(elements.formatted,list(working.element))
    } else {
      elements.formatted <- list(working.element)
    }
  }
  report.description$reportDescription$elements <- elements.formatted

  report.data <- SubmitJsonQueueReport(toJSON(report.description),interval.seconds=interval.seconds,max.attempts=max.attempts,validate=validate)

  return(report.data)

}
