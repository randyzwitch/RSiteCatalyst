\name{GetReportQueue}
\alias{GetReportQueue}
\title{Get Number/ID of Reports in Queue}
\usage{
GetReportQueue()
}
\value{
Character string or List
}
\description{
Requests the number of reports in the Report Queue, as well
as the Report ID.
}
\details{
Returns either a message to the console that no reports are
in the Queue or a list with the Report ID's.
}
\examples{
\dontrun{
    GetReportQueue()

    #Example Result
    "There are no reports in the Report Queue"
    }
}
\seealso{
\code{\link{CancelReport}}
}
\keyword{queue}
\keyword{report}

