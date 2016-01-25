#' @details This function requires having a character vector with one or more valid Report Suites specified.
#'
#' @description Get defined user groups for a company.
#'
#' @title Get Defined User Groups for a Company
#'
#' @param group_search_field Optional. Field to search for a specific value
#' @param group_search_value Optional. Use with group_search_field to search for a specific value
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
#' groups <- GetGroups()
#'
#'
#' }

GetGroups <- function(group_search_field="", group_search_value="") {

  request.body <- c()
  request.body$group_search_field <- unbox(group_search_field)
  request.body$group_search_value <- unbox(group_search_value)

  #Hack in locale, every method calls ApiRequest so this hopefully works
  #Set encoding to utf-8 as well; if someone wanted to do base64 they are out of luck
  request.body$locale <- unbox(AdobeAnalytics$SC.Credentials$locale)
  request.body$elementDataEncoding <- unbox("utf8")


  response <- ApiRequest(body=toJSON(request.body),func.name="Permissions.GetGroups")

  #Don't even know if this is possible, holdover from GetSegments code
  if(length(response$group_name[[1]]) == 0) {
      return(print("No User Groups Defined For This Report Suite"))
    }

  return(response)

}