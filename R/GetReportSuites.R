#' @description Get Report Suites Associated with a Specific User/Company
#'
#' @details Returns a data frame containing the Report Suite ID and Site Title
#' 
#' @title Get Report Suites Associated with a Specific User/Company
#' 
#' @return Data frame
#'
#' @export
#'
#' @examples
#' \dontrun{
#' report_suites <- GetReportSuites()
#' }

<<<<<<< HEAD
=======


#' Get Report Suites Associated with a Specific User/Company
#' 
#' Get Report Suites Associated with a Specific User/Company
#' 
#' Returns a data frame containing the Report Suite ID and Site Title
#' 
#' @return Data Frame
#' @keywords suite
#' @examples
#' 
#' \dontrun{    
#'     
#'     GetReportSuites()
#'     }
#'     
#'     
#'     
#' 
#' @export GetReportSuites
GetReportSuites<-function(){
  
  #Get Report Suites
  json <- postRequest("Company.GetReportSuites")
>>>>>>> master

GetReportSuites <- function() {

  reportsuites <- ApiRequest(func.name="Company.GetReportSuites")

  return(reportsuites$report_suites)

}
