#' @details This function requires having a character vector with one or more valid Report Suites specified.
#'
#' @description Get the commerce level for each of the specified report suites. 
#' 
#' @title Get the Commerce Level for a Report Suite(s)
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
#' ecom <- GetEcommerce("your_report_suite")
#' 
#' ecom2 <- GetEcommerce(report_suites$rsid)
#' }

GetEcommerce <- function(reportsuite.ids) {
  
  request.body <- c()
  request.body$rsid_list <- reportsuite.ids
  
  response <- ApiRequest(body=toJSON(request.body),func.name="ReportSuite.GetEcommerce")
  
  #Don't even know if this is possible, holdover from GetSegments code
  if(length(response$ecommerce[[1]]) == 0) {
      return(print("Ecommerce Not Enabled For This Report Suite"))
    }

  return(response)

}