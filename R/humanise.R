#' Generic: replace machine-readable codes by human-readable labels in
#' data tables
#' @title Human-readable data table
#' @param x object
#' @return data frame
#' @export
humanise <- function(x) UseMethod("humanise", x)

## Returns just the data tables, but with column names and factor
## names extracted from the metadata
#' @title Extract data table from st_result object and add descriptive
#' labels
#' @param x currently defined only for \code{st_result} objects
#' @return data frame
#' @export
humanise.st_result <- function(result) {
    res  <- result$data
    ## keep a copy of the JIS area codes
    res[, "area.code"] <- res$area

    cols <- colnames(res)[!colnames(res) %in% c("unit", "value",
                                                "area.code")]
    vars <- result$metadata$classes
    ## replace subclass codes with names (eg 001, 002 --> Men, Women)
    for (c in cols) {
        codes <-        vars[vars$class.id == c, "code"]
        names <- factor(vars[vars$class.id == c, "name"])
        res[, c] <- transcode(res[, c], data.frame(codes, names))
    }

    ## replace class ids with names (eg cat01 --> Gender)
    cat.names <- unique(vars[, c('class.id', 'class.name')])
    colnames(res) <- transcode(colnames(result$data),
                               cat.names)

    res
}
