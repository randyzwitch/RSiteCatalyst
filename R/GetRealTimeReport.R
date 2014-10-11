#' @name GetRealTimeReport
#' 
#' @title Get Real-Time report
#'
#' @description Function to access the Adobe Analytics Real-Time API v1.4. This API
#' provides the ability for reporting up to the most recent minute. This API is best
#' used at 15-30 second intervals (or longer).
#' 
#' @details The Real-Time API uses a concept of "relative dates". To get a feeling for
#' what's possible for submitting to date.from and date.to parameters, see link at:
#' 
#' http://php.net/manual/en/datetime.formats.relative.php
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
#' @param selected Selected items for a given element (only works for a single element)
#'
#' @importFrom plyr rename
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
                              algorithm.argument="linear", everything.else=TRUE,
                              selected=c()){
  
  #Temporary hopefully.
  if(length(elements) > 1){
    stop("RSiteCatalyst currently only supports real-time reporting with zero or 1 element")
  }
  
  #Make container for report description
  rd <- list() #empty container
  rd$source <- unbox("realtime") #hardcoded, requirement for API call
  rd$reportSuiteID <- unbox(reportsuite.ids) #report suite
  rd$metrics <- list(list(id=unbox(metrics))) #metric specified during Save
  rd$dateGranularity <- unbox(sprintf("minute:%s", date.granularity))
  rd$dateFrom <- unbox(date.from)
  rd$dateTo <- unbox(date.to)
  rd$sortMethod <- unbox(sprintf("%s:%s:%s:%s",sort.algorithm, floor.sensitivity, first.rank.period, algorithm.argument)) 
  
  #Build in ability to select specific elements
  if(length(elements) == 1 && length(selected) > 0){
    rd$elements <- lapply(elements, function (x) list(id=unbox(x), everythingElse=unbox(everything.else), selected=selected))
  } else if(length(elements) > 0){
    rd$elements <- lapply(elements, function (x) list(id=unbox(x), everythingElse=unbox(everything.else)))
  }
  
  #Create report description as JSON string
  report.description <- toJSON(list(reportDescription = rd))
  
  #Make API call, get response immediately
  report_raw <- JsonQueueRealTimeReport(report.description)
  
  if(length(elements) == 0) {
    
    df <- report_raw$report$data
    df <- rename(df, replace=c("counts" = report_raw$report$metrics$id))
    return(df)
    
  } else if(length(elements) == 1){
    
    df <- report_raw$report$data
    breakdown_list <- df$breakdown
    df$breakdown <- NULL
    
    parsed_df <- data.frame()
    for(i in 1:nrow(df)){
      right_df <-  breakdown_list[[i]]
      right_df <- rename(right_df, replace=c("name" = report_raw$report$elements$id[2]))
      temp <- cbind(df[i,],right_df, row.names = NULL)
      parsed_df <- rbind(parsed_df, temp)
    }
    parsed_df <- rename(parsed_df, replace=c("counts" = report_raw$report$metrics$id))
    return(parsed_df)
    
  } else{
    return(report_raw$report$data)
  }
  
} #End function
