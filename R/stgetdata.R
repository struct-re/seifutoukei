##' @name stgetdata
##' @title Download data from a table or other resource available in
##' the Japanese official statistics open-data access system.
##'
##' This function offers convenient filtering by area and
##' variable. For more esoteric queries that rely on the exact layout
##' of the table, \code{\link{nstac_api_call}} can be used directly.
##' Calling this function will result in one or two HTTP requests to
##' \url{statdb.nstac.go.jp}, unless the same query has been made on
##' this computer before and is still in the
##' cache. \code{\link{stclearcache}} clears the cache.
##' 
##' @param resource.id Numerical code of the resource containing the
##' desired data. Function \link{stfind} in this package provides one
##' way of looking up resources. Required.
##' @param filters (NOTE: most of this functionality is not yet
##' implemented.)  Optional \code{list} of criteria to filter the data
##' by. Filters must be flat vectors of acceptable values for the
##' class variable named by the key. To build your filters correctly,
##' you will probably need to refer to the metadata for the resource
##' in question, which is obtained using
##' \code{\link{stgetmetadata}}. The special name \code{areas} applies
##' to all tables and accepts a list of Japanese standard local
##' authority codes (JIS prefecture and municipality codes), see
##' package 'jlocalitycodes'.
##' @param lang Language for variables and labels, as ISO-639-1
##' two-letter code. The database provides labels in Japanese
##' (\code{ja}) and English. Default: Japanese.
##' @param raw Logical. Whether to return the data as they are in the
##' database, with numerical codes for row and column labels. Default
##' behaviour is to extract meaningful labels from the metadata.
##' @return An \code{st_result} object (\code{list(data=data.frame(),
##' footnotes=data.frame())}, or \code{NULL} if the request failed for
##' some reason. If the request succeeded but no matching data were
##' found, the \code{data} element will have zero rows.
##'
##' @importFrom XML xmlElementsByTagName xmlAttrs
##' @export
stgetdata <- function(resource.id, filters=list(), lang=NA, raw=FALSE) {

    extract_data <- function(node) {
        value.nodes   <- xmlElementsByTagName(node[['DATA_INF']], 'VALUE')
        res           <- data.frame(t(sapply(value.nodes, xmlAttrs)))
        res$value     <- sapply(value.nodes, xmlValue)
        rownames(res) <- NULL
        return(res)
    }

    extract_footnotes <- function(node) {
        note.nodes <- xmlElementsByTagName(node[['DATA_INF']], 'NOTE')
        return(
            data.frame(
                marker  = sapply(note.nodes, xmlGetAttr, 'char'),
                content = sapply(note.nodes, xmlValue)
                )
            )
    }

    ## get metadata to implement filtering by variable name and value
    meta <- stgetmetadata(resource.id)

    ## format data query
    params <- list(
        statsDataId = format_resource_id(resource.id),
        metaGetFlg  = "N" # since we are obtaining the metadata separately
        )
    params <- add_lang_option(params, lang)
    if(length(filters) > 1) {
        ## if(!is.null(filters$地域コード)) filters$areas <- filters$地域コード
        if(!is.null(filters$areas)) {
            ## elements of filters$areas at least look like JIS codes
            stopifnot(grep('^\\d{5}$', filters$areas == 1:length(filters$areas)))
            params$cdArea <- paste(filters$areas, collapse = ",")
        }
        filters$areas <- NULL
    }
    for(key in names(filters)) {
        class_code  <- NULL #TODO search 'meta' using filter[[key]]
        value_codes <- NULL #TODO idem
        params[[class_code]] <- paste(value_codes, collapse = ",")
    }

    ## send query & parse XML results
    subroot <- xmlRoot(
        nstac_api_call("getStatsData", params)
        )[['STATISTICAL_DATA']]

    ## format and deliver results
    res <- list(
        metadata  = meta,
        footnotes = extract_footnotes(subroot),
        data      = extract_data(subroot)
        )

    class(res) <- 'st_result'
    res
}
## ST統計表データ <- function(表コード, 絞り込み条件=list(), 言語=NA, 無処理=FALSE)
##     stgetdata(resource.id=表コード, filters=絞り込み条件, lang=言語, raw=無処理)
