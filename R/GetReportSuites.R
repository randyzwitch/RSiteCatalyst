#' GetReportSuites
#'
#' Gets all available report suites for the company.
#'
#' @return List of valid reportsuites
#'
#' @export
#'
#' @examples
#' reportsuites <- GetReportSuites()
#'


GetReportSuites <- function() {

  reportsuites <- ApiRequest(func.name="Company.GetReportSuites")

  return(reportsuites$report_suites)

}