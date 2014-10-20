#' @details The QueueRanked function returns a data frame equivalent to pulling 
#' a Ranked report in Adobe Reports & Analytics. Correlations & Sub-Relations are
#' supported.
#' 
#' Because of the Reporting API structure, this function first requests the 
#' report, then checks the reporting queue to see if the report is completed, and
#' when the report returns as "done" pulls the report from the API. This checking 
#' process will occur up to the specified number of times (default 120), with a
#' delay between status checks (default 5 seconds). If the report does not return
#' as "done" after the number of tries have completed, the function will return an
#' error message.
#' 
#' Note: Because of the multiple argument types ("top" and "start" OR "selected"), 
#' keyword arguments are generally needed towards the end of the function call instead
#' of just positional arguments.
#'
#' @description A QueueRanked report is a report that shows the ranking of values for 
#' one or more elements relative to a metric, aggregated over the time period selected.
#' 
#' @title Run a Ranked Report
#'
#' @param reportsuite.id Report suite id
#' @param date.from Start date for the report (YYYY-MM-DD)
#' @param date.to End date for the report (YYYY-MM-DD)
#' @param metrics List of metrics to include in the report
#' @param elements List of elements to include in the report
#' @param top Number of elements to include (top X) - only applies to the first element.
#' @param start Start row if you do not want to start at #1 - only applies to the first element.
#' @param selected List of specific items (of the first element) to include in the report - e.g. c("www:home","www:search","www:about").
#' this only works for the first element (API limitation).
#' @param search List of keywords for the first specified element - e.g. c("contact","about","shop").
#' search overrides anything specified using selected
#' @param search.type String specifying the search type: 'and', or, 'or' 'not' (defaults to 'or')
#' @param segment.id Id of Adobe Analytics segment to retrieve the report for
#' @param segment.inline Inline segment definition
#' @param classification SAINT classification to use in place of first element. Need to specify element AND classification.
#' @param data.current TRUE or FALSE - whether to include current data for reports that include today's date
#' @param expedite Set to TRUE to expedite the processing of this report
#' @param interval.seconds How long to wait between attempts
#' @param max.attempts Number of API attempts before stopping
#'
#' @importFrom jsonlite toJSON unbox
#' @importFrom plyr rbind.fill
#'
#' @return Data frame
#' 
#' @examples
#' \dontrun{
#' 
#' ranked1 <- QueueRanked("your_report_suite", 
#'                        date.from = "2014-04-01", 
#'                        date.to = "2014-04-20", 
#'                        metrics = "pageviews", 
#'                        elements = c("sitesection", "page") 
#'                        )
#' 
#' }
#'
#' @export

QueueRanked <- function(reportsuite.id, date.from, date.to, metrics, elements,
                        top=0,start=0,selected=c(), search=c(),search.type='or',
                        segment.id='', segment.inline='', classification=character(0),data.current=FALSE, 
                        expedite=FALSE,interval.seconds=5,max.attempts=120) {

  # build JSON description
  # we have to use unbox to force jsonlist not put strings into single-element arrays
  # new release of jsonlite will let us use jsonlite::singleton() (function is actually exported)
  report.description <- c()
  report.description$reportDescription <- c(data.frame(matrix(ncol=0, nrow=1)))
  report.description$reportDescription$dateFrom <- unbox(date.from)
  report.description$reportDescription$dateTo <- unbox(date.to)
  report.description$reportDescription$reportSuiteID <- unbox(reportsuite.id)
  if(segment.inline!="") {
    report.description$reportDescription$segments <- list(segment.inline)
  }
  if(start>0) { 
    report.description$reportDescription$startingWith <- unbox(start) 
  }
  if(segment.id!="") { 
    report.description$reportDescription$segment_id <- unbox(segment.id) 
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
                              top = unbox(top), 
                              startingWith = unbox(start))

      if(length(selected)!=0){
        working.element[["selected"]] <- selected
      }
      if(length(search)!=0){
        working.element[["search"]] <- list(type = unbox(search.type), 
                                            keywords = search)
      }
      if(length(classification)!=0){
        working.element[["classification"]] <- unbox(classification)
      }
    } else {
      working.element <- list(id = unbox(element), top = unbox("50000"))
    }

    if(length(elements.formatted)>0) {
      elements.formatted <- append(elements.formatted,list(working.element))
    } else {
      elements.formatted <- list(working.element)
    }
  }
  report.description$reportDescription$elements <- elements.formatted

  report.data <- JsonQueueReport(toJSON(report.description),interval.seconds=interval.seconds,max.attempts=max.attempts)

  return(report.data) 

}  
