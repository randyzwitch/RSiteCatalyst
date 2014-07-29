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

>>>>>>> parent of 2831735... Pre-master commit
GetReportSuites <- function() {

  reportsuites <- ApiRequest(func.name="Company.GetReportSuites")

  return(reportsuites$report_suites)

}
