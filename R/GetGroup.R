#' @details This function requires having a group name specified.
#'
#' @description Retrieves information about the specified permission group.
#'
#' @title Retrieves Information About The Specified Permission Group.
#'
#' @param group_name The name of the group that you want to retrieve.
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
#' group <- GetGroup("group_name")
#'
#'
#' }

GetGroup <- function(group_name) {

  request.body <- c()
  request.body$group_name <- unbox(group_name)

  #Hack in locale, every method calls ApiRequest so this hopefully works
  #Set encoding to utf-8 as well; if someone wanted to do base64 they are out of luck
  request.body$locale <- unbox(AdobeAnalytics$SC.Credentials$locale)
  request.body$elementDataEncoding <- unbox("utf8")


  response <- ApiRequest(body=toJSON(request.body),func.name="Permissions.GetGroup")
  
  response <- data.frame(
    group_id = response$group_id,
    group_name = response$group_name,
    group_description = response$group_description,
    all_report_suite_access = response$all_report_suite_access,
    rsid_list = paste(response$rsid_list, collapse = ","),
    user_list = paste(response$user_list, collapse = ",")
  )
  
  return(response)

}