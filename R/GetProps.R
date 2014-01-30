#' GetProps
#'
#' Gets sprop (traffic variable) definitions for the specified report suite(s). 
#' Useful to audit or document a report suite or company in Adobe Analytics.
#'
#' @param reportsuite.ids report suite id (or list of report suite ids)
#'
#' @return List of valid props
#'
#' @export
#'
#' @examples
#' props <- GetProps(c("your_prod_report_suite","your_dev_reportsuite"))
#'

GetProps <- function(reportsuite.ids) {
  
  request.body <- c()
  request.body$rsid_list <- reportsuite.id

  valid.props <- ApiRequest(body=toJSON(request.body),func.name="ReportSuite.GetProps")

  props.formatted <- data.frame()
  for (i in 1:length(valid.props$rsid) ) {
    valid.props$props[[i]]$report_suite <- valid.props$rsid[[i]]
    if(nrow(props.formatted)==0) {
      props.formatted <- valid.props$props[[i]]
    } else {
      props.formatted <- rbind(props.formatted,valid.props$props[[i]])
    }
  }

  return(props.formatted)

}