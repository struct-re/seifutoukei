#' @name stclearcache
#' @title Clear the on-disk cache used by this package.
#'
#' @export
stclearcache <- function() {
    ## cache directory
    cache.dir <- getOption("seifutoukei.http.cache.dir")
    if (is.null(cache.dir)) cache.dir <- "~/.seifutoukei.cache"
    go_ahead <- yesno(paste0("This will delete all files in ", cache.dir,
                             ". Continue?"))
    if (go_ahead) do.call(file.remove,
                          list(file.path(cache.dir, list.files(cache.dir))))
    invisible(NULL)
}
