\name{JsonQueueReport}
\alias{JsonQueueReport}
\title{Create Queue Report from JSON}
\usage{
JsonQueueReport(report.description, interval.seconds = 5,
  max.attempts = 120)
}
\arguments{
  \item{report.description}{JSON report description}

  \item{interval.seconds}{How long to wait between
  attempts}

  \item{max.attempts}{Number of API attempts before
  stopping}
}
\value{
Data frame
}
\description{
Generic interface to validate, queue and retrieve a report
from the API
}
\details{
This is a function for advanced users, for the case where a
user feels that submitting a JSON request would be easier
than using one of the pre-defined functions from
RSiteCatalyst
}
\examples{
\dontrun{

custom_report <- JsonQueueReport('valid Adobe Analytics API JSON string')

}
}

