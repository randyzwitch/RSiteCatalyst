#' @description Requests the number of reports in the Report Queue, as well as the Report ID.
#'
#' @details Returns either a message to the console that no reports are in the Queue or a list with the Report ID's.
#' 
#' @title Get Number/ID of Reports in Queue
#'
#' @return Console message and/or list
#'
#' @export
#'
#' @examples
#' \dontrun{
#' queue <- GetQueue()
#' }

GetQueue <- function() {

  request.body <- c()
  queue <- ApiRequest(body=toJSON(request.body),func.name="Report.GetQueue")

  if(length(queue) ==0) {
    print("There are no reports in the Report Queue")
    return(queue)
  } else {
    return(queue)
  }
  

}