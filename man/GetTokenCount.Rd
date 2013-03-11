\name{GetTokenCount}
\alias{GetTokenCount}
\title{Get API Token Count}
\description{
    Requests number of API tokens remaining for the month.
}
\usage{
    GetTokenCount()
}
\arguments{
    None
}
\details{
    Returns the number of tokens remaining per month and provides percentage remaining.
    
    GetTokenCount can also be used as a test to validate credentials are properly specified when using the SCAuth function.
}
\value{
    Character string
}
\seealso{
   \code{\link{SCAuth}}

     
}
\examples{
\dontrun{

    GetTokenCount()
    
    #Example Result
    "Your company has 9965 tokens remaining this month (99.7\% remaining)"
 }   
    
}
\keyword{tokens}