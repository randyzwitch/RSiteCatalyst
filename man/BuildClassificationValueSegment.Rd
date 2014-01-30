\name{BuildClassificationValueSegment}
\alias{BuildClassificationValueSegment}
\title{BuildClassificationValueSegment}
\usage{
BuildClassificationValueSegment(element, search.keywords, classification = "",
  search.type = "OR")
}
\arguments{
  \item{elements}{list of elements on which to vase the
  segment}

  \item{search.keywords}{list of search keyword vectorsfor
  each element (this can use ^ to pin to start, and $ to
  pin to end, or both to specify exact match)}

  \item{classification}{optional classification breakdown
  name for the element (defaults to the element name)}

  \item{search.type}{how to combine the keywords list. This
  defaults to 'OR' if it is not specified.}
}
\value{
segment definition for use with Queue* helper functions
}
\description{
Helper function to build a classification value segment for
use with other reports Multiple segments can be combined in
a list. Note that search can only be applied to a breakdown
classification and not an element value.
}

