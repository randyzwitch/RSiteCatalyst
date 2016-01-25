#' @details This function requires having a character vector with one or more valid Report Suites specified.
#'
#' @description Get key visitors for the specified report suites.
#'
#' @title Get Key Visitors for a Report Suite(s)
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
#' keyvisit<- GetKeyVisitors("your_report_suite")
#'
#' keyvisit2 <- GetKeyVisitors(report_suites$rsid)
#' }

GetKeyVisitors<- function(reportsuite.ids) {

  request.body <- c()
  request.body$rsid_list <- reportsuite.ids

  #Hack in locale, every method calls ApiRequest so this hopefully works
  #Set encoding to utf-8 as well; if someone wanted to do base64 they are out of luck
  request.body$locale <- unbox(AdobeAnalytics$SC.Credentials$locale)
  request.body$elementDataEncoding <- unbox("utf8")

  response <- ApiRequest(body=toJSON(request.body),func.name="ReportSuite.GetKeyVisitors")

  #Don't even know if this is possible, holdover from GetSegments code
  if(length(response$rsid[[1]]) == 0) {
      return(print("No Key Visitors Defined For This Report Suite"))
    }

  #Parse first level of classification
  accumulator <- data.frame()
  key_visitors_list <- response$key_visitors
  response$key_visitors <- NULL

  for(i in 1:nrow(response)){
    #Split get element classifications out of report
    key_visitors_df <- as.data.frame(key_visitors_list[[i]])
    if(nrow(key_visitors_df) == 0){
      temp <- cbind(response[i,], "")
    } else {
      temp <- cbind(response[i,], key_visitors_df, row.names = NULL)
    }
    accumulator <- rbind.fill(accumulator, temp)
  }

  names(accumulator) <- c("rsid", "site_title", "key_visitors")
  return(accumulator)

}