\name{GetTrafficVars}
\alias{GetTrafficVars}
\title{Get Traffic Variables (props) Associated with a Report Suite}
\usage{
GetTrafficVars(report_suites)
}
\arguments{
  \item{report_suites}{Character vector containing one or
  more valid Report Suite names}
}
\value{
Data Frame
}
\description{
Get Traffic Variables (props) Associated with a Report
Suite.
}
\details{
This function requires having a character vector with one
or more valid Report Suites specified.
}
\examples{
\dontrun{

    GetTrafficVars("keystonejowanza")
    GetTrafficVars(c("keystonejowanza", "keystonerandy", "keystonetraining"))
}
}
\keyword{prop}

