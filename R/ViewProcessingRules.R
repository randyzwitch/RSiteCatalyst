#' @details This function requires having a character vector with one or more valid Report Suites specified.
#'
#' @description Get Processing Rules with title and actions.
#'
#' @title View Processing Rules
#'
#' @param reportsuite.ids Report suite id (or list of report suite ids)
#'
#' @importFrom jsonlite toJSON
#' @importFrom plyr rbind.fill llply
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
      
      #unnest first layer of dataframe
      rsid <- as.data.frame(temp$rsid)
      names(rsid) <- "rsid"
      d_ <- cbind(rsid, temp$processing_rules)
      d_$rules <- llply(d_$rules, function(x) paste(x, collapse = " AND ")[[1]])
      
      #Transpose actions from one row list to multiple rows of strings
      d_2 <- data.frame()
      for(j in 1:nrow(d_)){
        temp <- d_[j,]
        actions <- as.data.frame(temp$actions)
        names(actions) <- "actions"
        temp$actions <- NULL
        
        d_2 <- rbind.fill(d_2, cbind(temp, actions, row.names = NULL))
      }
      
      #Append to outer dataframe
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