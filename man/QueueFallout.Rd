\name{QueueFallout}
\alias{QueueFallout}
\title{QueueFallout}
\usage{
QueueFallout(reportsuite.id, date.from, date.to, metrics, element, checkpoints,
  segment.id = "", expedite = FALSE)
}
\arguments{
  \item{reportsuite.id}{report suite id}

  \item{date.from}{start date for the report (YYYY-MM-DD)}

  \item{date.to}{end date for the report (YYYY-MM-DD)}

  \item{metrics}{list of metrics to include in the report}

  \item{element}{single pathed element (usually 'page')}

  \item{checkpoints}{character vector of checkpoints in the
  fallout path (e.g. c("Home","Contact","Thank You"))}

  \item{segment.id}{id of Adobe Analytics segment to
  retrieve the report for}

  \item{expedite}{set to TRUE to expedite the processing of
  this report}
}
\value{
Flat data frame containing checkpoints and metrics for each
step
}
\description{
Helper function to run a QueueFallout Report
}

