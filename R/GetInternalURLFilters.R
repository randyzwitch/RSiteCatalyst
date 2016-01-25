#' @details This function requires having a character vector with one or more valid Report Suites specified.
#'
#' @description Get internal url fitlers for the specified report suites.
#'
#' @title Get Internal URL Filters for a Report Suite(s)
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
#' calc<- GetInternalURLFilters("your_report_suite")
#'
#' calc2 <- GetInternalURLFilters(report_suites$rsid)
#' }

GetInternalURLFilters<- function(reportsuite.ids) {

  request.body <- c()
  request.body$rsid_list <- reportsuite.ids

  #Hack in locale, every method calls ApiRequest so this hopefully works
  #Set encoding to utf-8 as well; if someone wanted to do base64 they are out of luck
  request.body$locale <- unbox(AdobeAnalytics$SC.Credentials$locale)
  request.body$elementDataEncoding <- unbox("utf8")

  response <- ApiRequest(body=toJSON(request.body),func.name="ReportSuite.GetInternalURLFilters")

  #Don't even know if this is possible, holdover from GetSegments code
  if(length(response$internal_url_filters[[1]]) == 0) {
    return(print("No Internal URL Filters Defined For This Report Suite"))
  }

  #Parse first level of classification
  accumulator <- data.frame()
  internal_url_filters_list <- response$internal_url_filters
  response$internal_url_filters <- NULL

  for(i in 1:nrow(response)){
    #Split get element classifications out of report
    internal_url_filters_df <- as.data.frame(internal_url_filters_list[[i]])
    if(nrow(internal_url_filters_df) == 0){
      temp <- response[i,]
    } else {
      temp <- cbind(response[i,], internal_url_filters_df, row.names = NULL)
    }
    accumulator <- rbind.fill(accumulator, temp)
  }

  accumulator <- rename(accumulator, c("internal_url_filters_list[[i]]" = "internal_url_filters"))

  return(accumulator)

}