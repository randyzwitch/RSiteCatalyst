#' @details This function requires a single data feed id (obtained from GetFeeds)
#'
#' @description Returns structure of a data feed, including column header names
#'
#'
#' @title Get Data Feed Detail for a specific feed
#'
#' @param feed.id Data Feed ID

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
#' #Get info for feed #110980
#' feed <- GetFeed("110980")
#'
#'
#' }

GetFeed <- function(feed.id) {

  request.body <- c()
  request.body$feed_id <- unbox(feed.id)

  #Hack in locale, every method calls ApiRequest so this hopefully works
  #Set encoding to utf-8 as well; if someone wanted to do base64 they are out of luck
  request.body$locale <- unbox(AdobeAnalytics$SC.Credentials$locale)
  request.body$elementDataEncoding <- unbox("utf8")

  response <- ApiRequest(body=toJSON(request.body),func.name="DataFeed.GetFeed")

  #Returns data frame mostly parsed
  return(as.data.frame(response))

}