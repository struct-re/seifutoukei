\name{parse_metadata}
\alias{parse_metadata}
\title{Parse XML metadata information from the NSTAC open data
server into an R data frame (internal function)}
\usage{
parse_metadata(parent.node)
}
\arguments{
  \item{parent.node}{An xmlNode with TABLE_INF and
  CLASS_INF children}
}
\value{
an \code{st_metadata} list of \code{table.data} and
\code{classes}, both \code{data.frame}s
}
\description{
Parse XML metadata information from the NSTAC open data
server into an R data frame (internal function)
}

