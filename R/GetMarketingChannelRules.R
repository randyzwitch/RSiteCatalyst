#' @details This function requires having a character vector with one or more valid Report Suites specified.
#'
#' @description Get marketing channel rules for the specified report suites.
#'
#' @title Get Marketing Channel Rules for a Report Suite(s)
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
#' expire <- GetMarketingChannelRules("your_report_suite")
#'
#' expire2 <- GetMarketingChannelRules(report_suites$rsid)
#' }

GetMarketingChannelRules <- function(reportsuite.ids) {

  request.body <- c()
  request.body$rsid_list <- reportsuite.ids

  #Hack in locale, every method calls ApiRequest so this hopefully works
  #Set encoding to utf-8 as well; if someone wanted to do base64 they are out of luck
  request.body$locale <- unbox(AdobeAnalytics$SC.Credentials$locale)
  request.body$elementDataEncoding <- unbox("utf8")

  response <- ApiRequest(body=toJSON(request.body),func.name="ReportSuite.GetMarketingChannelRules")

  #Don't even know if this is possible, holdover from GetSegments code
  if(length(response$marketing_channel_rules[[1]]) == 0) {
      return(print("No Rules Defined For This Report Suite"))
    }

  ##Parsing this is a mess

  gmcr <- function(response_row){
    #Pull out first level
    mkt_channel_rules <- response_row$marketing_channel_rules[[1]]
    response_row$marketing_channel_rules <- NULL
    #Group together first level
    parsed <- cbind(response_row, mkt_channel_rules, row.names = NULL)
    
    #Pull out second level
    channel_value <- parsed$channel_value
    parsed$channel_value <- NULL
    #Group together second level
    parsed <- cbind(parsed, channel_value, row.names = NULL)
    
    #Pull out third level
    rules <- parsed$rules
    parsed$rules <- NULL
    
    parsed$i <- row.names(parsed)
    
    accumulator <- data.frame()
    for(i in 1:length(rules)){
      temp <- as.data.frame(rules[[i]]$rule_id)
      names(temp) <- c("rule_id")
      temp$i <- i
      temp$hit_attribute_type <- rules[[i]]$hit_attribute$type
      temp$hit_attribute_query_string_parameter <- rules[[i]]$hit_attribute$query_string_parameter
      temp$operator <- rules[[i]]$operator
      temp$matches <- rules[[i]]$matches
      accumulator <- rbind.fill(accumulator, temp, row.names = NULL)
      rm(temp)
    }
    
    parsed <- merge(parsed, accumulator, by = "i")
    parsed$i <- NULL
    
    return(parsed)
  }
  
  #With row-level parser defined above, iterate over rows
  df <- data.frame()
  for(i in 1:nrow(response)){
    row <- response[i,]
    parsedresult <- gmcr(row)
    df <- rbind.fill(df, parsedresult)
    rm(row, parsedresult)
  }
  
  #Such an ugly hack to format misc typed column
  clean <- function(x){
    if(length(x) == 0){
      ""
    } else if(length(x) > 1) {
      paste(x, collapse = ", ")
    } 
    else {
      x
    }
  }
  df$matches <- llply(df$matches, clean)

  return(df)

}