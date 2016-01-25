#' @details This function requires having a character vector with one or more valid Report Suites specified.
#'
#' @description Get list variables for the specified report suites.
#'
#' @title Get List Variables for a Report Suite(s)
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
#' calc<- GetListVariables("your_report_suite")
#'
#' calc2 <- GetListVariables(report_suites$rsid)
#' }

GetListVariables<- function(reportsuite.ids) {

  request.body <- c()
  request.body$rsid_list <- reportsuite.ids

  #Hack in locale, every method calls ApiRequest so this hopefully works
  #Set encoding to utf-8 as well; if someone wanted to do base64 they are out of luck
  request.body$locale <- unbox(AdobeAnalytics$SC.Credentials$locale)
  request.body$elementDataEncoding <- unbox("utf8")

  response <- ApiRequest(body=toJSON(request.body),func.name="ReportSuite.GetListVariables")

  #Don't even know if this is possible, holdover from GetSegments code
  if(length(response$list_variables[[1]]) == 0) {
    return(print("No List Variables Defined For This Report Suite"))
  }

  #Parse first level of classification
  accumulator <- data.frame()
  list_variables_list <- response$list_variables
  response$list_variables <- NULL

  for(i in 1:nrow(response)){
    #Split get element classifications out of report
    list_variables_df <- as.data.frame(list_variables_list[[i]])
    if(nrow(list_variables_df) == 0){
      temp <- response[i,]
    } else {
      temp <- cbind(response[i,], list_variables_df, row.names = NULL)
    }
    accumulator <- rbind.fill(accumulator, temp)
  }

  #accumulator <- rename(accumulator, c("name" = "formula_name"))

  return(accumulator)

}