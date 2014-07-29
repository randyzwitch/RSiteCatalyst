<<<<<<< HEAD
#' @details This function requires having a character vector with one or more valid Report Suites specified.
#'
#' @description Get a data frame of segments for the specified report suites. 
#' Useful to find segment IDs for use in reporting helper functions or JSON report definitions.
#' 
#' @title Get Segments Defined within a Report Suite
#' 
#' @param reportsuite.ids Report suite id (or list of report suite ids)
#'
#' @importFrom jsonlite toJSON
#' @importFrom plyr rbind.fill
#'
#' @return Data frame
#'
#' @export
#'
#' @examples
#' \dontrun{
#' segments <- GetSegments("your_report_suite")
#' 
#' segments2 <- GetSegments(report_suites$rsid)
#' }

GetSegments <- function(reportsuite.ids) {
=======
#GetSegments- Get segments for a single or multiple report suites
#This one could use minor cleanup




#' Get Segments Defined within a Report Suite
#' 
#' Get Segments defined within a Report Suite. Segments include Data Warehouse
#' segments, pre-defined segments, and user-defined segments
#' 
#' This function requires having a character vector with one or more valid
#' Report Suites specified.
#' 
#' @param report_suites Character vector containing one or more valid Report
#' Suite names
#' @return Data Frame
#' @keywords segments
#' @examples
#' 
#' \dontrun{    
#'     
#'     GetSegments("keystonejowanza")
#'     GetSegments(c("keystonejowanza", "keystonerandy", "keystonetraining"))
#' }    
#'     
#'     
#' 
#' @export GetSegments
GetSegments <- function (report_suites) {

#Converts report_suites to JSON
if(length(report_suites)>1){
  report_suites <- toJSON(report_suites)
} else {
  report_suites <- toJSON(list(report_suites))
}

#API request
json <- postRequest("ReportSuite.GetSegments",paste('{"rsid_list":', report_suites , '}'))

if(json$status== 200) {
#Convert JSON to list, clean through null values before list creation
results <- fromJSON(str_replace_all(content(json, as="text"), "null", 0))
} else {
  stop(jsonResponseError(json$status))
}

temp <- data.frame()
#Loop over report suite level 
for(report_suite in 1:length(results)){
>>>>>>> master
  
  request.body <- c()
  request.body$rsid_list <- reportsuite.ids
  
  valid.segments <- ApiRequest(body=toJSON(request.body),func.name="ReportSuite.GetSegments")

  segments.formatted <- data.frame()
  for (i in 1:length(valid.segments$rsid) ) {
    if(nrow(valid.segments$segments[[i]])>0) {
      valid.segments$segments[[i]]$report_suite <- valid.segments$rsid[[i]]
      if(nrow(segments.formatted)==0) {
        segments.formatted <- valid.segments$segments[[i]]
      } else {
        segments.formatted <- rbind.fill(segments.formatted,valid.segments$segments[[i]])
      }
    }
  }

  return(segments.formatted)

}
