#' @details Requires a single bookmark_id value, obtained from GetBookmarks()
#'
#' @description Get report description for a specific bookmark_id
#'
#' @title Get Report Description for a Specific bookmark_id
#'
#' @param bookmark.id Bookmark ID
#'
#' @importFrom jsonlite toJSON
#' @importFrom plyr rbind.fill
#'
#' @return List
#'
#' @export
#'
#' @examples
#' \dontrun{
#' reportdesc <- GetReportDescription("28473595")
#'
#'
#' }

GetReportDescription<- function(bookmark.id) {

  request.body <- c()
  request.body$bookmark_id  <- unbox(bookmark.id)

  #Hack in locale, every method calls ApiRequest so this hopefully works
  #Set encoding to utf-8 as well; if someone wanted to do base64 they are out of luck
  request.body$locale <- unbox(AdobeAnalytics$SC.Credentials$locale)
  request.body$elementDataEncoding <- unbox("utf8")

  response <- ApiRequest(body=toJSON(request.body),func.name="Bookmark.GetReportDescription")

  #Don't even know if this is possible, holdover from GetSegments code
  if(length(response$name[[1]]) == 0) {
      return(print("No Report Defined For This bookmark_id"))
    }

  accumulator <- as.data.frame(response[1])
  accumulator$reportType <- response[2]
  accumulator <- cbind(accumulator, response[[3]][1:4])
  accumulator$metrics <- paste(response$reportDescription$metrics[,1], collapse = ", ")

  return(accumulator)

}