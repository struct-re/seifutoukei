##' @name nstac_api_call
##' @title Access the database at \url{statdb.nstac.go.jp} using its HTTP API
##'
##' Formats its arguments into an HTTP request to NSTAC's open data
##' server, provided they validate against the subset of the NSTAC API
##' supported by this package, and sends the request unless a cached
##' response is available in local storage. The response from the
##' server is returned as an xmlDocument object (package XML), ie a
##' tree of nodes.
##' 
##' @param action API action to request. Currently, 'getStatsList',
##' 'getMetaInfo', and 'getStatsData' are supported.
##' @param params API parameters to send to the server, as a
##' \code{list} of flat vectors keyed by parameter name.
##' @param application.id API key. Optional if either of
##' \code{params$appId} or \code{options(seifutoukei.application.id)}
##' is defined. See package documentation for details on obtaining an
##' application ID.
##' @param force Logical. Send the query even if invalid or
##' unsupported.
##' @return An \code{XMLDocument} parsed from the server's response,
##' or \code{NULL} if the API call was invalid (and \code{force ==
##' FALSE}).
##'
##' @importFrom XML xmlParse saveXML
##' @importFrom httr parse_url build_url
##' @importFrom digest digest
nstac_api_call <- function(action, params, application.id=NA, force=FALSE) {

    messages <- msgs()

    ## Below two functions reuse code by Scott Chamberlain at
    ## https://github.com/ropensci/mocker
    cache_save <- function(cache, x, key, path) {
        if (cache) {
            if (!isTRUE(file.info(path)$isdir)) {
                dir.create(path, recursive = TRUE)
            }
            filepath <- paste0(path, digest(key), '.rds')
            saveRDS(x, filepath)
        } else {
            NULL
        }
    }

    ## TODO make into a method?
    cache_save_xml <- function(cache, doc, key, path) {
        if (cache) {
            if (!isTRUE(file.info(path)$isdir)) {
                dir.create(path, recursive = TRUE)
            }
            filepath <- paste0(path, digest(key), '.xml')
            saveXML(doc, filepath)
        } else {
            NULL
        }
    }

    cache_get_xml <- function(cache, key, path) {
        if (cache) {
            hash <- digest(key)
            stored_hashes <- dir(path, full.names = TRUE, pattern = ".xml")
            getname <- function(x) strsplit(x, "/")[[1]][length(strsplit(x, "/")[[1]])]
            stored_hashes_match <-
                gsub("\\.xml", "", sapply(stored_hashes, getname, USE.NAMES=FALSE))

            if (length(stored_hashes) == 0) {
                NULL
            } else {
                tt <- stored_hashes[stored_hashes_match %in% hash]
                if (identical(tt, character(0))) {
                    NULL
                } else {
                    xmlParse(tt)
                }
            }
        } else {
            NULL
        }
    }

    ## validate arguments
    stopifnot(class(action) == "character" & class(params) == "list")
    if (!validate_query(action, params, application.id) | force) {
        stop(messages$api.invalid)
    }

    ## get the application ID == API key and add it to the parameters
    global.app.id <- getOption("seifutoukei.application.id")
    
    if (!is.na(application.id)) {
        params$appId <- application.id
    } else if (!is.null(params$appId)) {
        params$appId <- params$appId
    } else if (!is.null(global.app.id)) {
        params$appId <- global.app.id
    } else {
        stop(messages$app.id.missing)
    }

    ## build the query URL
    qry.url       <- parse_url("http://statdb.nstac.go.jp/api/1.0b/app/")
    qry.url$path  <- paste0(qry.url$path, action)
    qry.url$query <- params
    qry.url       <- build_url(qry.url)

    ## set the directory to cache requests in
    cache.dir <- getOption("seifutoukei.http.cache.dir")
    if (is.null(cache.dir)) cache.dir <- "~/.seifutoukei.cache/"

    ## check the cache, if cacheing is activated
    cache <- isTRUE(getOption("seifutoukei.http.cache"))
    res   <- cache_get_xml(cache, qry.url, path = cache.dir)

    if(!is.null(res)) {
        ## cache hit
        message("Cache hit: ", qry.url, "\n")
        res
    } else {
        ## cache miss, res is NULL
        ## default behaviour is to ask before sending the HTTP request
        message("HTTP GET", " ", qry.url, "\n")
        if (isTRUE(getOption("seifutoukei.http.skip.confirmation"))) {
            go_ahead <- TRUE
        } else {
            go_ahead <- yesno2(paste(messages$http.confirm))
            message(messages$skip.advice)
        }

        if (go_ahead) {
            ## get result from server
            res <- xmlParse(qry.url, isURL=TRUE)
            cache_save_xml(cache, doc = res, key = qry.url, path = cache.dir)

            res
        } else {
            NULL
        }
    }
}
