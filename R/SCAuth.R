#' SCAuth
#'
#' Authorise and store credentials for the Adobe Analytics API
#' 
#' @title Store Credentials for the Adobe Analytics API
#'
#' @param key Client id from your app in the Adobe Marketing cloud Dev Center OR if you are using auth.method='legacy', then this is the API username (username:company)
#' @param secret Secret from your app in the Adobe Marketing cloud Dev Center OR if you are using auth.method='legacy', then this is the API shared secret
#' @param company Your company (only required if using OAUTH2 AUTH method)
#' @param token.file If you would like to save your OAUTH token and other auth details for use in 
#' future sessions, specify a file here. The method checks for the existence of the file and uses that if available.
#' @param auth.method Defaults to legacy, can be set to 'OAUTH2' to use the newer OAUTH method.
#' @param debug.mode Set global debug mode
#' @param endpoint Set Adobe Analytics API endpoint rather than let RSiteCatalyst decide (not recommended)
#'
#' @importFrom httr oauth_app oauth_endpoint oauth2.0_token
#' @importFrom stringr str_count str_split_fixed str_replace
#' 
#' @return Global credentials list 'SC.Credentials' in AdobeAnalytics (hidden) environment
#' 
#' @examples
#' \dontrun{
#' #Legacy authentication
#' SCAuth("key", "secret")
#' 
#' }
#'
#' @export

SCAuth <- function(key, secret, company='', token.file="", auth.method="legacy", debug.mode = FALSE, endpoint = ""){

  #Temporarily set SC.Credentials for GetEndpoint function call
  #SC.Credentials <<- list(key=key, secret=secret)
  
  assign("SC.Credentials", list(key=key, secret=secret),  envir = AdobeAnalytics)  
  
  if(company==''&&auth.method=='OAUTH2') {
    stop("ERROR: You must specify a company if using the OAUTH2 auth method.")
  } else {
    company <- str_split_fixed(key, ":", 2)[2]
  }
  
  endpoint.url <- GetEndpoint(company)
  
  #SC.Credentials <<- "" #This might be defensive overkill
  
  assign("SC.Credentials", "", envir = AdobeAnalytics)

  if(auth.method=="OAUTH2") {
    token.required = TRUE

    if(nchar(token.file)) {
      if(file.exists(token.file)) {
        load(token.file)
        #SC.Credentials <<- SC.storedcredentials
        
        assign("SC.Credentials", SC.storedcredentials, envir = AdobeAnalytics)
        
        #@TODO: check if token has expired, and whether the endpoint matches before deciding
        token.required = FALSE
      }
    }
    
    if(token.required) {
      sc.api<- oauth_endpoint("https://marketing.adobe.com",
                            "https://marketing.adobe.com/authorize",
                            "https://api.omniture.com/token")

      sc.app <- oauth_app("RSiteCatalyst", key, secret)
      sc.cred <- oauth2.0_token(sc.api, sc.app, scope="ReportSuite Report Company")

      if(!is.null(sc.cred$error)) {
        print(paste("ERROR:",sc.cred$error))
        stop(sc.cred$error_description)
      }

      scc <- list(endpoint.url=endpoint.url,
                               auth.method=auth.method,
                               access_token=sc.cred$access_token,
                               scope=sc.cred$scope,
                               client_id=sc.cred$client_id,
                               expires=sc.cred$expires,
                               debug = debug.mode
                               )
      
      assign("SC.Credentials", scc, envir = AdobeAnalytics)

      if(nchar(token.file)) {
        SC.storedcredentials <- AdobeAnalytics$SC.Credentials
        save(SC.storedcredentials,file=token.file)
      }
    }
  } else if (auth.method=="legacy") {

    error.flag = 0
    if(str_count(key, ":") != 1){
      warning("Check User Name. Must have 'username:company' pattern")
      error.flag = error.flag + 1
    }
    if(endpoint.url==""){
      stop("ERROR: No endpoint URL specified.")
      error.flag = error.flag + 1
    }
    if(nchar(secret) != 32){
      warning("Shared Secret does not have valid number of characters (32)")
      error.flag = error.flag + 1
    }
    
    if(error.flag >0){
      stop("Authentication failed due to errors")
    } else {
      
      #Allow for users to specify their own endpoint
      if(endpoint != ""){
        endpoint.url <- endpoint
      }
      
      assign("SC.Credentials", list(key=key,secret=secret,auth.method=auth.method,endpoint.url=endpoint.url,debug=debug.mode), envir = AdobeAnalytics)
      
      #save(SC.Credentials,file="~/SC.Credentials")
      #Assign endpoint to 3rd position in credentials
      #print("Legacy Auth Stored: This method is deprecated. If possible, use OAUTH.")
      print("Credentials Saved in RSiteCatalyst Namespace.")
    }

  }

}

#Create an environment to hold credentials
AdobeAnalytics <- new.env(parent = emptyenv())