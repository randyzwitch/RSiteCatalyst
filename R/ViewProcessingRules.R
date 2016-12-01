#' @details This function requires having a character vector with one or more valid Report Suites specified.
#'
#' @description Get Processing Rules with title and actions.
#'
#' @title View Processing Rules
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
#' 
#'  pr <- ViewProcessingRules("your-report-suite")
#'  pr <- ViewProcessingRules(c("your-report-suite", "your-report-suite2"))
#'  
#' }

ViewProcessingRules <- function(reportsuite.ids) {

  request.body <- c()
  request.body$rsid_list <- reportsuite.ids

  #Hack in locale, every method calls ApiRequest so this hopefully works
  #Set encoding to utf-8 as well; if someone wanted to do base64 they are out of luck
  request.body$locale <- unbox(AdobeAnalytics$SC.Credentials$locale)
  request.body$elementDataEncoding <- unbox("utf8")

  response <- ApiRequest(body=toJSON(request.body),func.name="ReportSuite.ViewProcessingRules")

  df <- data.frame()
  for(i in 1:nrow(response)){
    temp <- response[i,]
    #Check for null case
    if(length(temp$processing_rules[[1]]) > 0){ 
      d_ <- cbind(as.data.frame(temp$rsid), temp$processing_rules)
      d_2 <- cbind(d_, as.data.frame(d_$actions), row.names = NULL)
      d_2$actions <- NULL
      names(d_2) <- c("rsid", "title", "comment", "rules", "actions")
      df <- rbind.fill(df, d_2)
    } else{
      temp$processing_rules <- NULL #Remove empty list
      temp$title <- '' #Build all the normal fields
      temp$comment <- ''
      temp$rules <- ''
      temp$actions <- ''
      df <- rbind.fill(df, temp)
    }
  }

  return(df)

}