#' @details This function requires having a character vector with one or more valid Report Suites specified.
#'
#' @description Get Traffic Variables (props) Associated with a Report Suite(s).
#'
#'
#' @title Get Traffic Variables (props) Associated with a Report Suite
#'
#' @param reportsuite.ids report suite id (or list of report suite ids)
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
#' props <- GetProps("your_report_suite")
#'
#' props2 <- GetProps(report_suites$rsid)
#' }

GetProps <- function(reportsuite.ids) {

  request.body <- c()
  request.body$rsid_list <- reportsuite.ids

  #Hack in locale, every method calls ApiRequest so this hopefully works
  #Set encoding to utf-8 as well; if someone wanted to do base64 they are out of luck
  request.body$locale <- unbox(AdobeAnalytics$SC.Credentials$locale)
  request.body$elementDataEncoding <- unbox("utf8")

  valid.props <- ApiRequest(body=toJSON(request.body),func.name="ReportSuite.GetProps")

  props.formatted <- data.frame()
  for (i in 1:length(valid.props$rsid) ) {
    valid.props$props[[i]]$report_suite <- valid.props$rsid[[i]]
    if(nrow(props.formatted)==0) {
      props.formatted <- valid.props$props[[i]]
    } else {
      props.formatted <- rbind.fill(props.formatted,valid.props$props[[i]])
    }
  }

  return(props.formatted)

}