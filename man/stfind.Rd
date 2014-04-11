\name{stfind}
\alias{stfind}
\title{Locate tables and other resources in the Japanese official
statistics open data access system}
\usage{
stfind(keywords = NULL, survey.name = NULL, survey.date = NULL)
}
\arguments{
  \item{keywords}{vector of keywords to search for
  (resources that contain the keywords in metadata will be
  listed). Place names should work as keywords.}

  \item{survey.name}{set this to limit the search to data
  from one survey. Use \code{stlistsurveys()} to get a
  list.}

  \item{survey.date}{year and optionally month
  \code{"yyyy-mm"}, or interval specified as starting and
  ending year and month \code{c(from="yyyy-mm",
  to="yyyy-mm")}.}
}
\description{
Locate tables and other resources in the Japanese official
statistics open data access system
}

