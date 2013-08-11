\name{SCAuth}
\alias{SCAuth}
\title{SiteCatalyst User Name & Shared Secret}
\description{
    Saves SiteCatalyst User Name and Shared Secret for use in authentication.
}
\usage{
    SCAuth(user_name, shared_secret)
}
\arguments{
    \item{user_name}{
        Username:Company
    }
    \item{shared_secret}{
        Shared Secret
    }
}
\details{
    This function is run once at the beginning of a session to save credentials. The credentials list is referenced each time an API call is sent, in order to create the proper authentication header. 
    
Note that while the Shared Secret is input in plain text in the R script and stored within an R session in a list, all user credentials are hashed using the SHA1 algorithm and converted to base64 as required by Adobe before making the API call.
    
In order to access Reporting API, user must be part of the "Web Service Access" user group (This is set in the Admin panel).

When credentials are set properly, a message of "Authentication succeeded" will be printed to the console.


}
\value{
    List named "SCCredentials"
}
\seealso{
   \code{\link{GetTokenCount}}
     
}

\examples{
\dontrun{

    
    SCAuth("username:company", "28fa10193b6006badb981f6d0c370688")
    
    }
}
\keyword{authentication}