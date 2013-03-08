\name{SCAuth}
\alias{SCAuth}
\title{SiteCatalyst User Name & Shared Secret}
\description{
    Saves SiteCatalyst User Name and Shared Secret for use in authentication.
}
\usage{
    SCAuth(user_name, shared_secret, datacenter)
}
\arguments{
    \item{user_name}{
        Username:Company
    }
    \item{shared_secret}{
        Shared Secret
    }
    \item{datacenter}{
       1= San Jose Production, 2= Dallas Production, 3= London Production, 4= Singapore Production
    }
}
\details{
    This function is run once at the beginning of a session to save credentials. The credentials list is referenced each time an API call is sent, in order to create the proper authentication header. 
    
Note that while the Shared Secret is input in plain text in the R script and stored within an R session in a list, all user credentials are hashed using the SHA1 algorithm and converted to base64 as required by Adobe before making the API call.
    
In order to access Reporting API, user must be part of the "Web Service Access" group (under 'User Groups' in the SiteCatalyst Admin panel).

To validate credentials are set properly, call GetTokenCount function to see if a valid result is returned.


}
\value{
    List named "SCCredentials"
}
\references{
Official Adobe Documentation about Data Centers \cr
http://microsite.omniture.com/t2/help/en_US/home/index.html#kb-determining-data-center
}
\seealso{
   \code{\link{GetTokenCount}}
     
}

\examples{
    #San Jose datacenter
    SCAuth("username:company", "28fa10193b6006badb981f6d0c370688", 1)
    
    #Dallas datacenter
    SCAuth("username:company", "bb41ba97f1350cba8487b253da46a992", 2)
    
    #London datacenter
    SCAuth("username:company", "bb41ba97f1350cba8487b253da46a992", 3)
    
    #Singapore datacenter
    SCAuth("username:company", "28fa10193b6006badb981f6d0c370688", 4)
    
}
\keyword{authentication}
\note{}