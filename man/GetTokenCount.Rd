\name{GetTokenCount}
\alias{GetTokenCount}
\title{Get API Token Count}
\usage{
GetTokenCount()
}
\value{
Character string
}
\description{
Requests number of API tokens remaining for the month.
}
\details{
NOTE: There is no longer a token limit on the Adobe
Analytics API. This function remains unchanged for
historical purposes.

Returns the number of tokens remaining per month and
provides percentage remaining.
}
\examples{
\dontrun{

    GetTokenCount()

    #Example Result
    "Your company has 9965 tokens remaining this month (99.7\% remaining)"
 }
}
\seealso{
\code{\link{SCAuth}}
}
\keyword{tokens}

