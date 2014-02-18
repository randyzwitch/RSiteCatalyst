#' BuildHeader
#'
#' Internal function - Build Header for REST API call. This is deprecated, we should now be using OAUTH.
#'
#' @importFrom digest digest
#' @importFrom base64enc base64encode
#'
#' @return Header string for the old API auth method.
#'
#' @family internal
#' @keywords internal

BuildHeader <- function() {

  #Create nonce
  nonce <- as.character(as.numeric(Sys.time()))
  #Create timestamp
  created.date <- format(as.POSIXlt(Sys.time(), "GMT"), "%Y-%m-%dT%H:%M:%SZ")
  #Concatentate nonce, timestamp, shared secret, then sha1 then base64
  nonce.create.secret <- paste(nonce, created.date, SC.Credentials$secret, sep="")
  sha.object <- digest(nonce.create.secret, algo="sha1", serialize=FALSE)
  password.digest <- base64encode(charToRaw(sha.object))

  #Build & Return Header 
  return(paste('X-WSSE: UsernameToken Username=\"',SC.Credentials$key, '\"', ',', ' PasswordDigest=\"',password.digest, '\"', ',', ' Nonce=\"', nonce, '\"', ',', ' Created=\"', created.date, '\"', sep=""))
  
}
