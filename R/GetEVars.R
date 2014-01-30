#' GetEvars
#'
#' Gets available report suite evars
#'
#' @param reportsuite.ids report suite id (or list of report suite ids)
#'
#' @return List of valid evars
#'
#' @export

GetEvars <- function(reportsuite.ids) {
  
  report.description <- c()
  report.description$rsid_list <- reportsuite.id

  valid.evars <- ApiRequest(body=toJSON(report.description),func.name="ReportSuite.GetEvars")

  evars.formatted <- data.frame()
  for (i in 1:length(valid.evars$rsid) ) {
    valid.evars$evars[[i]]$report_suite <- valid.evars$rsid[[i]]
    if(nrow(evars.formatted)==0) {
      evars.formatted <- valid.evars$evars[[i]]
    } else {
      evars.formatted <- rbind(evars.formatted,valid.evars$evars[[i]])
    }
  }

  return(evars.formatted)

}