\name{estat_api_call}
\alias{estat_api_call}
\title{Access the e-stat HTTP API at \url{api.e-stat.go.jp}

Formats its arguments into an HTTP request to the open data server
at \code{api.e-stat.go.jp}, provided the former validate against
the subset of the e-stat API supported by this package; then sends
the request, unless a cached response is available in local
storage. The response, whether cached or from the server, is
returned as an xmlDocument object (see package XML), ie a tree of
nodes.}
\usage{
estat_api_call(action, params, application.id = NA, force = FALSE)
}
\arguments{
  \item{action}{Character string. API action to request.
  Currently supported actions are 'getStatsList',
  'getMetaInfo', and 'getStatsData'.}

  \item{params}{API parameters to send to the server, as a
  \code{list} of flat vectors keyed by parameter name.}

  \item{application.id}{Character string. User's API key.
  Optional if either of \code{params$appId} or
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
Access the e-stat HTTP API at \url{api.e-stat.go.jp}

Formats its arguments into an HTTP request to the open data
server at \code{api.e-stat.go.jp}, provided the former
validate against the subset of the e-stat API supported by
this package; then sends the request, unless a cached
response is available in local storage. The response,
whether cached or from the server, is returned as an
xmlDocument object (see package XML), ie a tree of nodes.
}

