#' Build Configuration for Real-Time Report
#' 
#' Selects the metrics and elements (dimensions) on which you want real time
#' reports enabled. Use individual reports as arguments in SaveRealTimeSettings
#' 
#' 
#' @param report.name Real-Time Report Name
#' @param metric Metric for Real-Time Report
#' @param elements Breakdowns for Real-Time Report
#' @param min.granularity min.granularity
#' @param ui.report Show report in Adobe Analytics interface
#' 
#' @return List
#' @seealso \code{\link{GetRealTimeSettings}} \cr
#' @seealso \code{\link{SaveRealTimeSettings}} \cr
#' @keywords BuildRealTimeReportStructure
#' @examples
#' 
#' \dontrun{
#' 
#'    }
#' 
#' @export
#' 

BuildRealTimeReportStructure <- function(report.name="",
                                         metric = "", 
                                         elements = c(), 
                                         min.granularity="1",
                                         ui.report=TRUE
                                         ) {
  report.body <- list()
  
  #Populate list for report 
  report.body$metric <- unbox(metric)
  report.body$elements <- elements
  report.body$min_granularity <- unbox(min.granularity)
  report.body$name <- unbox(report.name)
  report.body$ui_report <- unbox(ui.report)
  
  return(report.body)

}