\name{QueueTrended}
\alias{QueueTrended}
\title{QueueTrended}
\usage{
QueueTrended(reportsuite.id, date.from, date.to, metrics, elements, top = 0,
  start = 0, selected = c(), search = c(), search.type = "or",
  date.granularity = "day", segment.id = "", segment.inline = "",
  anomaly.detection = FALSE, data.current = FALSE, expedite = FALSE,
  interval.seconds = 5, max.attempts = 120)
}
\arguments{
  \item{reportsuite.id}{report suite id}

  \item{date.from}{start date for the report (YYYY-MM-DD)}

  \item{date.to}{end date for the report (YYYY-MM-DD)}

  \item{metrics}{list of metrics to include in the report}

  \item{elements}{list of elements to include in the
  report}

  \item{top}{number of rows to return}

  \item{start}{start row if you do not want to start at #1}

  \item{selected}{list of specific items (of the first
  element) to include in the report - e.g.
  c("www:home","www:search","www:about")}

  \item{search}{list of keywords for the first specified
  element - e.g. c("contact","about","shop"). search
  overrides anything specified using selected}

  \item{search.type}{string specifying the search type:
  'and', or, 'or' 'not' (defaults to 'or')}

  \item{date.granularity}{time granularity of the report
  (year/month/week/day/hour), default to 'day'}

  \item{segment.id}{id of Adobe Analytics segment to
  retrieve the report for}

  \item{segment.inline}{inline segment definition}

  \item{anomaly.detection}{set to TRUE to include forecast
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
Helper function to run a Trended Report
}
\examples{
report.data <- QueueTrended("your_report_suite", "2014-01-01", "2014-01-07",
                           c("visits","uniquevisitors","pageviews","event10"),
                           c("page","geoCountry","geoCity"))
}

