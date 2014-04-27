\name{stgetmetadata}
\alias{stgetmetadata}
\title{Obtain the metadata of a table or other resource available
in the Japanese official statistics open-data access system.}
\usage{
stgetmetadata(resource.id, lang = NA)
}
\arguments{
  \item{resource.id}{Numerical code of the resource,
  typically a table, to look up metadata for.}

  \item{lang}{Language for labels and values (ISO-639-1
  code).}
}
\value{
A list of \code{table}, a list of information about the
table, and \code{classes}, a data frame of the class
(category) codes and names used in the table.
}
\description{
Obtain the metadata of a table or other resource available
in the Japanese official statistics open-data access
system.
}

