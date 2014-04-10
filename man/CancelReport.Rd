\name{CancelReport}
\alias{CancelReport}
\title{Cancel a Report in the Report Queue}
\usage{
CancelReport(reportID)
}
\arguments{
  \item{reportID}{Report ID obtained from GetReportQueue}
}
\value{
Character string or List
}
\description{
Cancels a report in the Report Queue
}
\details{
Returns either a console message that no reports are queued
or a list of reportID numbers
}
\examples{
\dontrun{

    CancelReport(12345678)

    #Example Result
    "Report 12345678 has been cancelled"

          }
}
\seealso{
\code{\link{GetReportQueue}}
}
\keyword{cancel}
\keyword{report}

