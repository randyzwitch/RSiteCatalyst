#' @details This function's arguments are both optional
#'
#' @description Get defined dashboards
#'
#' @title Get Defined Dashboards
#'
#' @param dashboard.limit Limit number of dashboards returned
#' @param dashboard.offset Offset of dashboards (i.e. start with other than first dashboard)
#'
#' @importFrom jsonlite toJSON
#' @importFrom plyr rbind.fill
#' @importFrom plyr quickdf
#'
#' @return List
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dash<- GetDashboards()
#'
#' dash2 <- GetBookmarks('5', '1')
#' }

GetDashboards<- function(dashboard.limit='', dashboard.offset='') {

  request.body <- c()

  #Hack in locale, every method calls ApiRequest so this hopefully works
  #Set encoding to utf-8 as well; if someone wanted to do base64 they are out of luck
  request.body$locale <- unbox(AdobeAnalytics$SC.Credentials$locale)
  request.body$elementDataEncoding <- unbox("utf8")

  if(dashboard.limit != ''){
    request.body$dashboard_limit <- dashboard.limit
  }

  if(dashboard.offset != ''){
    request.body$dashboard_offset <- dashboard.offset
  }


  response <- ApiRequest(body=toJSON(request.body),func.name="Bookmark.GetDashboards")

  #Don't even know if this is possible, holdover from GetSegments code
  if(length(response$dashboards[[1]]) == 0) {
      return(print("No Dashboards Defined For This Report Suite"))
    }

  #Get df out of single element list
  response_df<- response[[1]]
  pages <- response_df$pages
  response_df$pages <- NULL
  #Add Pages to parsed response
  response_df <- cbind(response_df, ldply(pages, quickdf))

  #Parse bookmarks
  bookmarks <- response_df$bookmarks
  response_df$bookmarks <- NULL


  accumulator<- data.frame()
  for(i in 1:nrow(response_df)){
    bkmk_parsed <- ldply(bookmarks[[i]],quickdf)

    temp <- cbind(response_df[i,], bkmk_parsed, row.names = NULL)
    accumulator <- rbind.fill(accumulator, temp)
  }



  return(accumulator)

}