\name{GetQueue}
\alias{GetQueue}
\title{Get Number/ID of Reports in Queue}
\usage{
GetQueue()
}
\value{
data frame with: reportID, type, queueTime, status,
priority, estimate, reportSuiteID, user
}
\description{
GetQueue
}
\details{
Gets the report queue
}
\examples{
\dontrun{
queue <- GetQueue()
}
}

