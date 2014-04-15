#' GetReportSuites
#'
#' Gets all available report suites for the company.
#' 
#' @title Get Report Suites Associated with a Specific User/Company
#' 
#' @return List of valid reportsuites
#'
#' @export
#'
#' @examples
#' \dontrun{
#' reportsuites <- GetReportSuites()
#' }


GetReportSuites <- function() {

  reportsuites <- ApiRequest(func.name="Company.GetReportSuites")

  return(reportsuites$report_suites)

}