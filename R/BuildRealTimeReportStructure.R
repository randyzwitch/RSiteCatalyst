#' Build Configuration for Real-Time Report
#' 
#' Selects the metrics and elements (dimensions) on which you want Real-Time
#' reports enabled. Use the returned list from this function as argument(s) in SaveRealTimeSettings.
#' 
#' 
#' @param report.name Real-Time Report Name
#' @param metric Metric for Real-Time Report
#' @param elements Breakdowns for Real-Time Report
#' @param min.granularity Minimum Granularity for Report. Defaults to 1 minute.
#' @param ui.report Show report in Adobe Analytics web interface
#' 
#' @return List
#' @seealso \code{\link{GetRealTimeSettings}} \cr
#' @seealso \code{\link{SaveRealTimeSettings}} \cr
#' @keywords BuildRealTimeReportStructure
#' @examples
#' 
#' \dontrun{
#'
#'report.test1 <- BuildRealTimeReportStructure(report.name="test123",
#'                metric="instances",
#'                elements = c("prop2", "searchenginekeyword", "geocountry"))
#'
#'report.test2 <- BuildRealTimeReportStructure(report.name="test456",
#'                metric="instances",
#'                elements = c("prop2", "searchenginekeyword", "geocountry"),
#'                min.granularity = "5")
#'
#'report.test3 <- BuildRealTimeReportStructure(report.name="test789",
#'                metric="instances",
#'                elements = c("prop2", "searchenginekeyword", "geocountry"),
#'                min.granularity = "5",
#'                ui.report=FALSE)
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