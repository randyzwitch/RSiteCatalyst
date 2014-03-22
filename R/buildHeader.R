#Build Header for REST API call

buildHeader <- function() {
  
#Create nonce to avoid MITM attack
  nonce <- as.character(as.numeric(Sys.time()))

#Created timestamp
  created_date <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")

#Concatentate nonce, timestamp, shared secret, then sha1 then base64
  nonce_create_secret <- paste(nonce, created_date, SCCredentials[2], sep="")
  sha_object <- digest(nonce_create_secret, algo="sha1", serialize=FALSE)
  password_digest <- base64encode(charToRaw(sha_object))
  

#Build & Return Header 
 return(paste('UsernameToken Username=\"',SCCredentials[1], '\"', ',', ' PasswordDigest=\"',password_digest, '\"', ',', ' Nonce=\"', nonce, '\"', ',', ' Created=\"', created_date, '\"', sep=""))

}