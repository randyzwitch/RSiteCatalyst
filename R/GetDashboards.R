#' @details This function's arguments are both optional
#'
#' @description Get defined dashboards
#' 
#' @title Get Defined Dashboards
#' 
#' @param dashboard.limit 
#' @param dashboard.offset
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
#' dash<- GetDashboards()
#' 
#' dash2 <- GetBookmarks('5', '1')
#' }

GetDashboards<- function(dashboard.limit='', dashboard.offset='') {
  
  request.body <- c()
  
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
  
  #accumulator <- data.frame()
  #response_ <- response[[1]]
  
  #for(i in 1:nrow(response_)){
    
    #Make copy, delete bookmark list
  #  left <- response_[i, 1:3]
  #  left <- rename(left, replace = c("name" = "folder_name", "id" = "folder_id"))
  #  right <- response_[[i,4]]
  #  right <- rename(right, replace = c("name" = "bookmark_name", "id" = "bookmark_id"))
    
  #  temp <- cbind(left, right)
    
  #  accumulator <- rbind.fill(accumulator, temp)
  #}
  

  return(response)

}