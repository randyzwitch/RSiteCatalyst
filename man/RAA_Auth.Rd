\name{RAA_Auth}
\alias{RAA_Auth}
\title{RAA_Auth}
\usage{
RAA_Auth(key, secret, endpoint.url = "", token.file = "",
  auth.method = "OAUTH2")
}
\arguments{
  \item{key}{client id from your app in the Adobe Marketing
  cloud Dev Center OR if you are using
  auth.method='legacy', then this is the API username
  (username:company)}

  \item{secret}{secret from your app in the Adobe Marketing
  cloud Dev Center OR if you are using
  auth.method='legacy', then this is the API shared secret}

  \item{endpoint.url}{your Adobe Analytics API endpoint}

  \item{token.file}{if you would like to save your OAUTH
  token and other auth details for use in future sessions,
  specify a file here. The method checks for the existence
  of the file and uses that if available.}

  \item{auth.method}{defaults to OAUTH2, can be set to
  'legacy' to use the older () username:company and shared
  secret method.}
}
\description{
Authorise and store credentials for the Adobe Analytics API
}

