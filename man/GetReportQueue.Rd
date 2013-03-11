\name{GetReportQueue}
\alias{GetReportQueue}
\title{Get Number/ID of Reports in Queue}
\description{
    Requests the number of reports in the Report Queue, as well as the Report ID.
}
\usage{
    GetReportQueue()
}
\arguments{
    None
}
\details{
    Returns either a message to the console that no reports are in the Queue or a list with the Report ID's.
}
\value{
    Character string or List
}
\seealso{
\code{\link{CancelReport}}
}

\examples{
\dontrun{  
    GetReportQueue()
    
    #Example Result
    "There are no reports in the Report Queue"
    }
    
}
\keyword{report queue}