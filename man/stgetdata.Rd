\name{stgetdata}
\alias{stgetdata}
\title{Download data from a table or other resource available in
the Japanese official statistics open-data access system.

This function offers convenient filtering by area and
variable. For more esoteric queries that rely on the exact layout
of the table, \code{\link{nstac_api_call}} can be used directly.
Calling this function will result in one or two HTTP requests to
\url{statdb.nstac.go.jp}, unless the same query has been made on
this computer before and is still in the
cache. \code{\link{stclearcache}} clears the cache.}
\usage{
stgetdata(resource.id, filters = list(), lang = NA, raw = FALSE)
}
\arguments{
  \item{resource.id}{Numerical code of the resource
  containing the desired data. Function \link{stfind} in
  this package provides one way of looking up resources.
  Required.}

  \item{filters}{(NOTE: most of this functionality is not
  yet implemented.)  Optional \code{list} of criteria to
  filter the data by. Filters must be flat vectors of
  acceptable values for the class variable named by the
  key. To build your filters correctly, you will probably
  need to refer to the metadata for the resource in
  question, which is obtained using
  \code{\link{stgetmetadata}}. The special name
  \code{areas} applies to all tables and accepts a list of
  Japanese standard local authority codes (JIS prefecture
  and municipality codes), see package 'jlocalitycodes'.}

  \item{lang}{Language for variables and labels, as
  ISO-639-1 two-letter code. The database provides labels
  in Japanese (\code{ja}) and English. Default: Japanese.}

  \item{raw}{Logical. Whether to return the data as they
  are in the database, with numerical codes for row and
  column labels. Default behaviour is to extract meaningful
  labels from the metadata.}
}
\value{
An \code{st_result} object (\code{list(data=data.frame(),
footnotes=data.frame())}, or \code{NULL} if the request
failed for some reason. If the request succeeded but no
matching data were found, the \code{data} element will have
zero rows.
}
\description{
Download data from a table or other resource available in
the Japanese official statistics open-data access system.

This function offers convenient filtering by area and
variable. For more esoteric queries that rely on the exact
layout of the table, \code{\link{nstac_api_call}} can be
used directly. Calling this function will result in one or
two HTTP requests to \url{statdb.nstac.go.jp}, unless the
same query has been made on this computer before and is
still in the cache. \code{\link{stclearcache}} clears the
cache.
}

