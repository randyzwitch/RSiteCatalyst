#' @details This function requires having a character vector with one or more valid Report Suites specified.
#'
#' @description Get default page for the specified report suites. 
#' 
#' @title Get Default Page for a Report Suite(s)
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
#' defpage <- GetDefaultPage("your_report_suite")
#' 
#' defpage2 <- GetDefaultPage(report_suites$rsid)
#' }

GetDefaultPage <- function(reportsuite.ids) {
  
  request.body <- c()
  request.body$rsid_list <- reportsuite.ids
  
  response <- ApiRequest(body=toJSON(request.body),func.name="ReportSuite.GetDefaultPage")
  
  #Don't even know if this is possible, holdover from GetSegments code
  if(length(response$default_page[[1]]) == 0) {
      return(print("No Default Page For This Report Suite"))
    }

  return(response)

}