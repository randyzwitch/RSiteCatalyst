\name{QueuePathing}
\alias{QueuePathing}
\title{QueuePathing}
\usage{
QueuePathing(reportsuite.id, date.from, date.to, metric, element, pattern,
  top = 1000, start = 1, segment.id = "", expedite = FALSE)
}
\arguments{
  \item{reportsuite.id}{report suite id}

  \item{date.from}{start date for the report (YYYY-MM-DD)}

  \item{date.to}{end date for the report (YYYY-MM-DD)}

  \item{metric}{single metric to include in the report
  (usually 'pageviews')}

  \item{element}{single pathed element (usually 'page')}

  \item{pattern}{character vector of items in the path (up
  to 3) use "::anything::" as a wildcard. For example
  c("Home","::anything::","::anything::") will return all
  paths that start with the home page,
  c("::anything::","Home","::anything::") will return the
  previous and next pages from the home page, and
  c("::anything::","::anything::","Home") will return the
  two previous pages leading to the home page.}

  \item{top}{number of rows to return (defaults to 1000)}

  \item{start}{start row if you do not want to start at #1}

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
Helper function to run a pathed Report
}

