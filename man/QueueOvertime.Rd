\name{QueueOvertime}
\alias{QueueOvertime}
\title{QueueOvertime}
\usage{
QueueOvertime(reportsuite.id, date.from, date.to, metrics,
  date.granularity = "day", segment.id = "", segment.inline = "",
  anomaly.detection = FALSE, data.current = FALSE, expedite = FALSE)
}
\arguments{
  \item{reportsuite.id}{report suite id}

  \item{date.from}{start date for the report (YYYY-MM-DD)}

  \item{date.to}{end date for the report (YYYY-MM-DD)}

  \item{metrics}{list of metrics to include in the report}

  \item{date.granularity}{time granularity of the report
  (year/month/week/day/hour), default to 'day'}

  \item{segment.id}{id of Adobe Analytics segment to
  retrieve the report for}

  \item{segment.inline}{inline segment definition}

  \item{anomaly.dection}{set to TRUE to include forecast
  data (only valid for day granularity with small date
  ranges)}

  \item{data.current}{TRUE or FALSE - whether to include
  current data for reports that include today's date}

  \item{expedite}{set to TRUE to expedite the processing of
  this report}
}
\value{
Flat data frame containing datetimes and metric values
}
\description{
Helper function to run an OverTime Report
}

