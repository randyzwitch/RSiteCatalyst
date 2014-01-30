#' ApiRequest
#'
#' Internal function - Calls the API and gets valid evars for specified params
#'
#' @param report.description JSON report description
#' @param func.name the name of the Adobe Analytics API function that we are calling
#' @param interval.seconds Time to wait between request attempts (defaults to 2 seconds)
#' @param max.attempts Max number of attempts to make the request (defaults to 1, this is only increased for GetReport)
#' @param print.attempts if set to TRUE, this will print attempt numbers to the console
#'
#' @importFrom httr content
#' @importFrom httr add_headers
#' @importFrom httr POST
#' @importFrom jsonlite toJSON
#' @importFrom jsonlite fromJSON
#'
#' @return list of available evars
#'
#' @family internal
#'

ApiRequest <- function(body="",func.name="",interval.seconds=2,max.attempts=1,print.attempts=FALSE) {

  endpoint <- RAA.Credentials$endpoint
  if(RAA.Credentials$auth.method=="OAUTH2") {
    url <- paste(endpoint, "?method=",func.name,"&access_token=",RAA.Credentials$access_token, sep="")
  } else if(RAA.Credentials$auth.method=="legacy") {
    url <- paste(endpoint, "?method=",func.name, sep="")
  }

  if(RAA.Debug) {
    print(paste("Requesting URL: ",url))
    print(body)
  }

  result <- FALSE
  num.tries <- 0

  while(result==FALSE && num.tries < max.attempts){
    num.tries <- num.tries + 1
    if(print.attempts==TRUE) {
      print(paste("Requesting URL attempt #",num.tries,sep=""))
    }
    if(RAA.Credentials$auth.method=="OAUTH2") {
      response <- POST(url, body=body)
    } else if(RAA.Credentials$auth.method=="legacy") {
      response <- POST(url, add_headers(BuildHeader()), body=body)
    }
    if(response$status==200) {
      result <- TRUE
    } else {
      Sys.sleep(interval.seconds)
    }
  }

  if(!result){
    response.content <- content(response)
    if(nchar(response.content$error_description)) {
      stop(paste("ERROR:",response.content$error_description))
    } else {
      stop(paste("ERROR: max attempts exceeded for",url))
    }
  }

  # If we are in debug mode, save the output
  if(RAA.Debug==TRUE) {
    filename <- paste("PostRequest_",sub(":","-",Sys.time()),".json",sep="")
    print(paste("DEBUG: saving output as",filename))
    sink(filename)
    cat(toJSON(content(response)))
    sink()
  }

  data <- fromJSON(content(response,"text"))

  return(data)

}