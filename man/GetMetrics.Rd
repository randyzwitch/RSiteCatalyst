\name{GetMetrics}
\alias{GetMetrics}
\title{GetMetrics}
\usage{
GetMetrics(reportsuite.id, metrics = c(), elements = c(),
  date.granularity = "")
}
\arguments{
  \item{reportsuite.id}{report suite id}

  \item{metrics}{list of existing metrics you want to use
  in combination with an additional metric}

  \item{elements}{list of existing elements you want to use
  in combination with an additional metric}

  \item{date.granularity}{granularity that you want to
  combine with an additional metric}
}
\value{
List of valid metrics
}
\description{
Gets valid metrics for current user, valid with optionally
specified existing metrics, elements and date granularity
}
\examples{
metrics.valid <- GetMetrics("your_report_suite",metrics=c('visitors','pageviews'),elements=c('page','geoCountry'),date.granularity='day')
}

