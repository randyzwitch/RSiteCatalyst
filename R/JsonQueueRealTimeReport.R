#' @name JsonQueueRealTimeReport
#' 
#' @title Create Real-Time Report from JSON
#'
#' @description Generic interface to validate, queue and retrieve a realtime report from the API
#' 
#' @details This is a function for advanced users, for the case where a user feels
#' that submitting a JSON request would be easier than using one of the pre-defined
#' functions from RSiteCatalyst
#' 
#' @param report.description JSON report description
#'
#' @importFrom jsonlite toJSON
#'
#' @return Data frame
#' 
#' @examples
#' \dontrun{
#' 
#' custom_report <- JsonQueueRealTimeReport('valid Adobe Analytics API JSON string')
#'
#' }
#' @export
#'

JsonQueueRealTimeReport <- function(report.description) {

#   if(!ValidateReport(report.description)) {
#     stop("ERROR: Invalid report description.")
#   }
  
  #Document that relative time being used
  #http://www.php.net/manual/en/datetime.formats.relative.php
  
  reportsuite.ids <- "zwitchdev"
  metrics <- "instances"
  date.granularity <- "7"
  date.from <- "6 hours ago"
  date.to <- "2 hours ago"
  elements <- c("geocountry")
  sort.algorithm <- "mostpopular"
  floor.sensitivity <- .25
  first.rank.period <- 0
  algorithm.argument <- "linear"
  
  #Make container for report description
  rd <- list() #empty container
  rd$source <- unbox("realtime") #hardcoded, requirement for API call
  rd$reportSuiteID <- unbox(reportsuite.ids) #report suite
  rd$metrics <- list(list(id=unbox(metrics))) #metric specified during Save
  rd$dateGranularity <- unbox(sprintf("minute:%s", date.granularity))
  rd$dateFrom <- unbox(date.from)
  rd$dateTo <- unbox(date.to)
  rd$sortMethod <- unbox(sprintf("%s:%s:%s:%s",sort.algorithm, floor.sensitivity, first.rank.period, algorithm.argument)) 
  
  #Needs improvement: this only works for single element
  #Doesn't incorporate everythingElse argument either
  #"elements":[{"id":"product","everythingElse": true}]
  rd$elements <- list(list(id=unbox(elements)))
    
    
  
  #Create JSON string
  report.description <- toJSON(list(reportDescription = rd))
  
  report <- ApiRequest(body=report.description,func.name="Report.Run")
  
  #Can I just return report$report$data directly?
  #Single element returns a data frame where the breakdown column in a nested list
  return(report)

}