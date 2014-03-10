#' QueueRanked
#'
#' Helper function to run a Ranked Report
#'
#' @param reportsuite.id report suite id
#' @param date.from start date for the report (YYYY-MM-DD)
#' @param date.to end date for the report (YYYY-MM-DD)
#' @param metrics list of metrics to include in the report
#' @param elements list of elements to include in the report
#' @param top number of elements to include (top X) - only applies to the first element.
#' @param start start row if you do not want to start at #1 - only applies to the first element.
#' @param selected list of specific items to include in the report - e.g. list(page=c("Home","Search","About")). 
#' this only works for the first element (API limitation).
#' @param search list of keywords for a specified element - e.g. list(page=c("contact","about","shop")). 
#' search overrides anything specified using selected
#' @param segment.id id of Adobe Analytics segment to retrieve the report for
#' @param segment.inline inline segment definition
#' @param data.current TRUE or FALSE - whether to include current data for reports that include today's date
#' @param expedite set to TRUE to expedite the processing of this report
#'
#' @importFrom jsonlite toJSON unbox
#'
#' @return Flat data frame containing datetimes and metric values
#'
#' @export

QueueRanked <- function(reportsuite.id, date.from, date.to, metrics, elements,
                        top=0,start=0,selected=list(), search=list(),
                        segment.id='', segment.inline='', data.current=FALSE, expedite=FALSE,interval.seconds=5,max.attempts=120) {

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
    report.description$reportDescription$start <- unbox(start) 
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
    if(length(selected[[element]])!=0 && i==1){
      # put in top and startingWith for the first element only
      working.element <- list(id = unbox(element), 
                                  top = unbox(top), 
                                  startingWith = unbox(start), 
                                  selected = selected[element][1][[1]])
    } else {
      working.element <- list(id = unbox(element), 
                              top = unbox(top), 
                              startingWith = unbox(start),
                              selected=NULL)
    }
    if(length(search)!=0){
      working.element[["search"]] <- list(type = unbox('or'), 
                                  keywords = search[element][1][[1]])
    }
    if(length(elements.formatted)>0) {
      elements.formatted <- rbind(elements.formatted,working.element)
    } else {
      elements.formatted <- working.element
    }
  }

  if(length(elements)==1) {
    report.description$reportDescription$elements <- list(elements.formatted)
  } else {
    report.description$reportDescription$elements <- elements.formatted
  }

  report.data <- JsonQueueReport(toJSON(report.description),interval.seconds=interval.seconds,max.attempts=max.attempts)

  return(report.data) 

}  
