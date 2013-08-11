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
    NOTE: There is no longer a token limit on the Adobe Analytics API. This function remains unchanged for historical purposes.
    
    Returns the number of tokens remaining per month and provides percentage remaining.
    
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