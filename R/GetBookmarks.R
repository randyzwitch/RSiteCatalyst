#' @details This function's arguments are both optional
#'
#' @description Get defined bookmarks for a user.
#'
#' @title Get Defined Bookmarks for a user
#'
#' @param folder.limit Max number of folders to return
#' @param folder.offset Offset of folders (i.e. start with other than first folder)
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
#' bookmarks<- GetBookmarks()
#'
#' bookmarks2 <- GetBookmarks('5', '1')
#' }

GetBookmarks<- function(folder.limit='', folder.offset='') {

  request.body <- c()

  #Hack in locale, every method calls ApiRequest so this hopefully works
  #Set encoding to utf-8 as well; if someone wanted to do base64 they are out of luck
  request.body$locale <- unbox(AdobeAnalytics$SC.Credentials$locale)
  request.body$elementDataEncoding <- unbox("utf8")

  if(folder.limit != ''){
    request.body$folder_limit <- folder.limit
  }

  if(folder.offset != ''){
    request.body$folder_offset <- folder.offset
  }


  response <- ApiRequest(body=toJSON(request.body),func.name="Bookmark.GetBookmarks")

  #Don't even know if this is possible, holdover from GetSegments code
  if(length(response$folders[[1]]) == 0) {
      return(print("No Bookmarks Defined For This Report Suite"))
    }

  accumulator <- data.frame()
  response_ <- response[[1]]

  for(i in 1:nrow(response_)){

    #Make copy, delete bookmark list
    left <- response_[i, 1:3]
    left <- rename(left, replace = c("name" = "folder_name", "id" = "folder_id"))
    right <- response_[[i,4]]
    right <- rename(right, replace = c("name" = "bookmark_name", "id" = "bookmark_id"))

    temp <- cbind(left, right)

    accumulator <- rbind.fill(accumulator, temp)
  }


  return(accumulator)

}