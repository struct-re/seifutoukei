\name{nstac_api_call}
\alias{nstac_api_call}
\title{Access the database at \url{statdb.nstac.go.jp} using its HTTP API

Formats its arguments into an HTTP request to NSTAC's open data
server, provided they validate against the subset of the NSTAC API
supported by this package, and sends the request unless a cached
response is available in local storage. The response from the
server is returned as an xmlDocument object (package XML), ie a
tree of nodes.}
\usage{
nstac_api_call(action, params, application.id = NA, force = FALSE)
}
\arguments{
  \item{action}{API action to request. Currently,
  'getStatsList', 'getMetaInfo', and 'getStatsData' are
  supported.}

  \item{params}{API parameters to send to the server, as a
  \code{list} of flat vectors keyed by parameter name.}

  \item{application.id}{API key. Optional if either of
  \code{params$appId} or
  \code{options(seifutoukei.application.id)} is defined.
  See package documentation for details on obtaining an
  application ID.}

  \item{force}{Logical. Send the query even if invalid or
  unsupported.}
}
\value{
An \code{XMLDocument} parsed from the server's response, or
\code{NULL} if the API call was invalid (and \code{force ==
FALSE}).
}
\description{
Access the database at \url{statdb.nstac.go.jp} using its
HTTP API

Formats its arguments into an HTTP request to NSTAC's open
data server, provided they validate against the subset of
the NSTAC API supported by this package, and sends the
request unless a cached response is available in local
storage. The response from the server is returned as an
xmlDocument object (package XML), ie a tree of nodes.
}

