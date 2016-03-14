#' ApiRequest
#'
#' Internal function - Calls the API and gets response for specified params
#'
#' @param report.description JSON report description
#' @param func.name the name of the Adobe Analytics API function that we are calling
#' @param interval.seconds Time to wait between request attempts (defaults to 2 seconds)
#' @param max.attempts Max number of attempts to make the request (defaults to 1, this is only increased for GetReport)
#' @param print.attempts if set to TRUE, this will print attempt numbers to the console
#' @param skip.queue If set to TRUE, return content instead of parsing via jsonlite
#' 
#'
#' @importFrom httr content add_headers POST
#' @importFrom jsonlite toJSON fromJSON
#'
#' @return json
#'
#' @export
#' @keywords internal

ApiRequest <- function(body='',func.name='',interval.seconds=2,max.attempts=1,print.attempts=FALSE,skip.queue=FALSE) {
  
  if(length(AdobeAnalytics$SC.Credentials$auth.method) == 0){
    stop("Please log in using SCAuth()")
  }
  
  #Set debug flag from global credentials
  SC.Debug <- AdobeAnalytics$SC.Credentials$debug
  
  endpoint <- AdobeAnalytics$SC.Credentials$endpoint
  if(AdobeAnalytics$SC.Credentials$auth.method=='OAUTH2') {
    url <- paste(endpoint, '?method=',func.name,'&access_token=',AdobeAnalytics$SC.Credentials$access_token, sep='')
  } else if(AdobeAnalytics$SC.Credentials$auth.method=='legacy') {
    url <- paste(endpoint, '?method=',func.name, sep='')
  }

  if(exists('SC.Debug')&&SC.Debug==TRUE) {
    print(paste('Requesting URL: ',url))
    print(body)
  }

  result <- FALSE
  num.tries <- 0

  while(result==FALSE && num.tries < max.attempts){
    num.tries <- num.tries + 1
    if(print.attempts==TRUE) {
      print(paste('Requesting URL attempt #',num.tries,sep=''))
    }
    if(AdobeAnalytics$SC.Credentials$auth.method=='OAUTH2') {
      response <- POST(url, body=body)
    } else if(AdobeAnalytics$SC.Credentials$auth.method=='legacy') {
      response <- POST(url, config=add_headers('',.headers=BuildHeader()), body=body)
    }
    
    #RZ: This is a hack for realtime; Adobe realtime API returns 'true' for save method
    #jsonlite from within httr content function doesn't like this
    
    if(skip.queue == TRUE){
      return(response) 
    }
    
    if(response$status==200 || response$status==400) {
      # we have a valid response or a bad request error
      response.content <- fromJSON(content(response,'text', encoding = "UTF-8"))
      if(response$status==400&&response.content$error=='report_not_ready') {
        result <- FALSE
        Sys.sleep(interval.seconds)
      } else {
        result <- TRUE
      }
    } else {
      print(response$status)
      Sys.sleep(interval.seconds)
    }
  }

  if(!result||response$status==400){
    response.content <- fromJSON(content(response,'text', encoding = "UTF-8"))
    if(response.content$error=='report_not_ready') {
      stop(paste('ERROR: max attempts exceeded for',url))
    } else {
      stop(paste('ERROR:',response.content$error," - ",response.content$error_description))
    }
  }

  # If we are in debug mode, save the output
  if(exists('SC.Debug')&&SC.Debug==TRUE) {
    #filename <- paste('PostRequest_',sub(':','-',Sys.time()),'.json',sep='')
    filename <- paste('PostRequest_',as.numeric(Sys.time()),'.json',sep='')
    print(paste('DEBUG: saving output as',filename))
    sink(filename)
    cat(content(response,'text', encoding = "UTF-8"))
    sink()
  }

  data <- fromJSON(content(response,'text', encoding = "UTF-8"))

  return(data)

}