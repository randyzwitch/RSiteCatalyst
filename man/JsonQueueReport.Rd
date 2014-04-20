\name{JsonQueueReport}
\alias{JsonQueueReport}
\title{JsonQueueReport}
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

