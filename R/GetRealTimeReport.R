#' @name GetRealTimeReport
#' 
#' @title Get Real-Time report
#'
#' @description Fill in later
#' 
#' @details Fill in later
#' 
#' @param reportsuite.ids Report Suite
#' @param metrics Report metric
#' @param elements Report breakdowns
#' @param date.granularity Report Granularity. Defaults to 5 minutes
#' @param date.from Report starting time. Defaults to "1 hour ago"
#' @param date.to Report end time. Defaults to "now"
#' @param sort.algorithm Sorting algorithm. Defaults to "mostpopular"
#' @param floor.sensitivity Floor sensitivity. Defaults to .25
#' @param first.rank.period First Ranking Period. Defaults to 0
#' @param algorithm.argument Ranking algorithm. Defaults to "linear"
#' @param everything.else Provide counts for elements not returned as 'top'
#'
#' @importFrom jsonlite toJSON
#'
#' @return Data frame
#' 
#' @examples
#' \dontrun{
#' 
#' custom_report <- GetRealTimeReport('')
#'
#' }
#' @export
#'
#'


GetRealTimeReport <- function(reportsuite.ids, metrics, elements=c(), date.granularity=5, 
                              date.from="1 hour ago", date.to="now", sort.algorithm="mostpopular",
                              floor.sensitivity=.25, first.rank.period=0, 
                              algorithm.argument="linear", everything.else=TRUE){
  
  #Make container for report description
  rd <- list() #empty container
  rd$source <- unbox("realtime") #hardcoded, requirement for API call
  rd$reportSuiteID <- unbox(reportsuite.ids) #report suite
  rd$metrics <- list(list(id=unbox(metrics))) #metric specified during Save
  rd$dateGranularity <- unbox(sprintf("minute:%s", date.granularity))
  rd$dateFrom <- unbox(date.from)
  rd$dateTo <- unbox(date.to)
  rd$sortMethod <- unbox(sprintf("%s:%s:%s:%s",sort.algorithm, floor.sensitivity, first.rank.period, algorithm.argument)) 
  
  if(length(elements) > 0){
    rd$elements <- lapply(elements, function (x) list(id=unbox(x), everythingElse=unbox(everything.else)))
  }
  
  #Create report description as JSON string
  report.description <- toJSON(list(reportDescription = rd))
  
  #Make API call, get response immediately
  report_raw <- JsonQueueRealTimeReport(report.description)
  
  #Return raw data - Still need parser for elements > 0
  if(length(elements) == 0) {
    df <- report_raw$report$data
    names(df) <- c("name", "year", "month", "day", "hour", "minute", report_raw$report$metrics$id)
    return(df)
  } else{
    return(report_raw$report$data)
  }
  
} #End function