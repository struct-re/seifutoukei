\name{stgetmetadata}
\alias{stgetmetadata}
\title{Obtain the metadata of a table or other resource available
in the Japanese official statistics open-data access system.}
\usage{
stgetmetadata(resource.id, lang = NA)
}
\arguments{
  \item{resource.id}{Numerical code of the resource to look
  up metadata for.}

  \item{lang}{Language of labels and values (ISO-639-1
  code).}
}
\value{
An \code{st_metadata}-class list of \code{table.data} and
\code{classes}, both data frames.
}
\description{
Obtain the metadata of a table or other resource available
in the Japanese official statistics open-data access
system.
}

