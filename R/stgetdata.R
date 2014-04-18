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
##' @param raw.params API parameters to pass through unmodified. Use
##' with care.
##' @return An \code{st_result} object (\code{list(data=data.frame(),
##' footnotes=data.frame())}, or \code{NULL} if the request failed for
##' some reason. If the request succeeded but no matching data were
##' found, the \code{data} element will have zero rows.
##'
##' @importFrom XML xmlElementsByTagName xmlAttrs
##' @export
stgetdata <- function(resource.id, filters=list(), lang=NA, raw.params=list()) {

    extract_data <- function(node) {
        nodes     <- xmlElementsByTagName(node[['DATA_INF']], 'VALUE')
        res       <- data.frame(
            t(sapply(value.nodes, xmlAttrs, USE.NAMES = FALSE)),
            row.names = NULL)
        res$value <- sapply(value.nodes, xmlValue, USE.NAMES = FALSE)
        res$value <- as.numeric(res$value)
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

    transcode_filter <- function(key, values, meta) {
        categories <- unique(meta$classes[, c('class.id', 'class.name')])
        cat.code  <- categories$class.id[categories$class.name == key]
        value.codes <- vapply(values, function(v) {
            tmp <- meta$classes[meta$classes$name == v, 'code']
            if (is.null(tmp) | length(tmp) != 1)
                stop(paste0('No mention of',
                            '\'', v, '\'',
                            'in table metadata.'))
            tmp
        }, '')
        if (!is.null(cat.code) & length(cat.code) == 1) {
            c(paste0('cd',
                     toupper(substr(cat.code, 1, 1)),
                     substr(cat.code, 2, nchar(cat.code))),
              paste0(value.codes, collapse = ",")
              )
        } else {
            NULL
        }
    }

    ## get metadata to implement filtering by variable name and value
    meta <- stgetmetadata(resource.id)

    ## Prepare query parameters
    ## ========================
    params <- list(
        statsDataId = format_resource_id(resource.id),
        metaGetFlg  = "N" # since we obtained the metadata above
        )
    params <- add_lang_option(params, lang)

    if(length(filters) > 0) {
        ## filter by area (JIS prefecture and municipality codes)
        ## if(!is.null(filters$地域コード)) filters$areas <- filters$地域コード
        if(!is.null(filters$areas)) {
            ## ensure elements of filters$areas at least look like JIS codes
            stopifnot(length(grep('^\\d{5}$', filters$areas)) ==
                      length(filters$areas))
            if (length(filters$areas) == 2 &
                ## accept a range of codes if labeled c(from=, to=)
                !(is.na(filters$areas['from']) |
                  is.na(filters$areas['to']))) {
                params$cdAreaFrom <- filters$areas['from']
                params$cdAreaTo   <- filters$areas['to']
            } else {
                ## otherwise interpret it as a simple set of codes
                params$cdArea <- paste(filters$areas, collapse = ",")
            }
        }
        filters$areas <- NULL ## that's taken care of
    }

    if(length(filters) > 0) {
        for(key in names(filters)) {
            filter <- transcode_filter(key, filters[[key]], meta)
            if (!is.null(filter)) params[[filter[1]]] <- filter[[2]]
            rm(filter)
        }
    }

    if(length(raw.params) > 0) {
        warning('Passing through these parameters with no checks: ',
                paste(names(raw.params), raw.params, sep = '=', collapse = ', '))
        params <- c(params, raw.params)
    }

    res <- list(
        metadata  = meta,
        footnotes = NULL,
        data      = NULL
        )

    ## there is a 100,000-row hard limit; if there is data left to
    ## request, adjust, repeat, and bind
    received.until <- 0
    total.results  <- NA

    while (!isTRUE(received.until == total.results)) {
        ## send query & parse XML results
        xml     <- nstac_api_call("getStatsData", params)
        subroot <- xmlRoot(xml)[['STATISTICAL_DATA']]

        table_inf      <- subroot[['TABLE_INF']]
        total.results  <- as.numeric(xmlValue(table_inf[['TOTAL_NUMBER']]))
        received.until <- as.numeric(xmlValue(table_inf[['TO_NUMBER']]))
        ## not needed:
        ## received.from  <- as.numeric(xmlValue(table_inf[['FROM_NUMBER']]))

        ## format and deliver results
        res$footnotes <-
            if (is.null(res$footnotes)) {
                extract_footnotes(subroot)
            } else {
                rbind(res$footnotes, extract_footnotes(subroot))
            }

        res$data <-
            if (is.null(res$data)) {
                extract_data(subroot)
            } else {
                rbind(res$data, extract_data(subroot))
            }

        if (received.until < total.results) {
            message("Partial result: ",
                    prettyNum(format = "d", received.until, big.mark = ","),
                    " out of ",
                    prettyNum(format = "d", total.results, big.mark = ","),
                    " received. Requesting next batch.")
            params$startPosition <- sprintf("%d", received.until + 1)
        }
    }

    class(res) <- 'st_result'
    res
}
## ST統計表データ <- function(表コード, 絞り込み条件=list(), 言語=NA, 無処理=FALSE)
##     stgetdata(resource.id=表コード, filters=絞り込み条件, lang=言語, raw=無処理)
