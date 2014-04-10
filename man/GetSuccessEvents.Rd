\name{GetSuccessEvents}
\alias{GetSuccessEvents}
\title{Get Success Events Associated with a Report Suite}
\usage{
GetSuccessEvents(report_suites)
}
\arguments{
  \item{report_suites}{Character vector containing one or
  more valid Report Suite names}
}
\value{
Data Frame
}
\description{
Get Success Events associated with one or more Report
Suites.
}
\details{
This function requires having a character vector with one
or more valid Report Suites specified.
}
\examples{
\dontrun{

    GetSuccessEvents("keystonejowanza")
    GetSuccessEvents(c("keystonejowanza", "keystonerandy", "keystonetraining"))
    }
}
\keyword{events}

