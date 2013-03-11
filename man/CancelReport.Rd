\name{CancelReport}
\alias{CancelReport}
\title{Cancel a Report in the Report Queue}
\description{
    Cancels a report in the Report Queue
}
\usage{
    CancelReport(reportID)
}
\arguments{
      \item{reportID}{
        Report ID obtained from GetReportQueue
    }
}
\details{
    Returns either a console message that no reports are queued or a list of reportID numbers
}
\value{
    Character string or List
}
\seealso{
\code{\link{GetReportQueue}}
}

\examples{
  
\dontrun{  
  
    CancelReport(12345678)
    
    #Example Result
    "Report 12345678 has been cancelled"
    
          }
}
\keyword{cancel report}