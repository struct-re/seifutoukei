#' @importFrom XML xmlChildren xmlRoot xmlValue xmlGetAttr xmlSApply xmlName xmlAttrs
#' @importFrom plyr rbind.fill
#' @importFrom lubridate ceiling_date ymd days day

## Package seifutoukei
## shared data store and utility functions

## Package-global shared data (in-memory cache)
## do not confuse with on-disk cache of server responses, see nstac_api_call.R
################################################################################

.st_cache <- new.env()

cached_variable <- function(name, producer) {
    if (!exists(name, envir = .st_cache)) {
        value <- do.call(producer, list())
        assign(name, value, envir = .st_cache)
    }
    res <- get(name, envir = .st_cache)
    return(res)
}

msgs <- function() {
    cached_variable('messages', function() {
        readRDS(system.file('extdata', 'messages.rds', package="seifutoukei"))
    })
}

spec <- function() {
    cached_variable('api_spec', function() {
        readRDS(system.file('extdata', 'api_spec.rds', package="seifutoukei"))
    })
}

## General-purpose utilities
################################################################################

is_digit_sequence <- function(str) {
    length(grep("^\\d+$", str)) == length(str)
}

yesno2 <- function(msg) {
    ans <- tolower(readline(paste0(msg, "\n(yes/no): ")))
    if (ans %in% c("n", "no")) {
        FALSE
    } else if(ans %in% c("y", "yes")) {
        TRUE
    } else {
        yesno2("Please answer ‘yes’ or ‘no’.")
    }
}

yesno <- function(msg) {
    ans <- match.arg(tolower(readline(paste0(msg, "\n(y/n): "))), c("yes", "no"))

    (ans == "yes")
}

lmerge <- function(into, from) {
    res <- into
    for(i in 1:length(from)) {
        res[[ names(from)[i] ]] <- from[[i]]
    }
    res
}

transcode <- function(x, table, y) {
    indices  <- match(as.character(x), as.character(table), nomatch=NA)
    matching <- !is.na(indices)
    res      <- c(x)
    res[matching] <- (as.character(y)[indices])[matching]
    if (class(y) == "factor") {
        res         <- factor(res)
        levels(res) <- levels(y)
    }

    res
}

ympunct <- function(ymstrings) {
    res <- ymstrings
    ixs <- grep("^\\d{6}$", res)
    res[ixs] <- paste0(substr(res[ixs], 1, 4),
                       "-",
                       substr(res[ixs], 5, 6))
    res
}

ym.re <- "(19[5-9]|20[012])\\d(0[1-9]|1[012])"

is_ymrange <- function(strings) {
    re <- paste0('^', ym.re, '-', ym.re, '$')
    regexpr(re, strings) > 0
}

is_ymdate <- function(strings) {
    re <- paste0('^', ym.re, '$')
    regexpr(re, strings) > 0
}

is_sane_year <- function(strings) {
    re <- '^(19[5-9]|20[012])\\d$'
    regexpr(re, strings) > 0
}

ymrange <- function(strings) {
    rangesplit <- function(str) {
        cbind(substr(str, 1, 4),
              substr(str, 5, 6),
              substr(str, 8, 11),
              substr(str, 12, 13))
    }
    ymsplit <- function(str) {
        cbind(substr(str, 1, 4),
              substr(str, 5, 6),
              substr(str, 1, 4),
              substr(str, 5, 6))
    }

    res       <- data.frame(start.year  = rep(NA, length(strings)),
                            start.month = NA,
                            end.year    = NA,
                            end.month   = NA)
    ranges    <- grep("^\\d{6}-\\d{6}$", strings)
    ym_dates  <- grep("^\\d{6}$", strings)
    years     <- grep("^\\d{4}$", strings)

    res[ranges, ]        <- rangesplit(strings[ranges])
    res[ym_dates, ]      <- ymsplit(strings[ym_dates])
    res[years, "end.year"] <- strings[years]
    res
}

guess_survey_year_from_title <- function(titles) {
    year = rep(NA, length(titles))
    ## shōwa
    for (y in 20:64) year[grep(paste0("昭和", y, "年"), titles)] <- (1925 + y)
    ## heisei
    ## this code has an expiry date
    for (y in 1:64) year[grep(paste0("平成", y, "年"), titles)] <- (1988 + y)

    year
}

## Domain-specific utilities
################################################################################

add_lang_option <- function(params, lang) {
    res <- params
    if(!is.na(lang) & (lang == "ja" | lang == "en")) {
        res$lang <- ifelse(lang=="en", "E", "J")
    }

    res
}

find_survey_code <- function(string) {
    list <- all_surveys()
    if(is_digit_sequence(string)) {
        indexes <- grep(string, list$code)
    } else {
        indexes <- grep(string, list$name)
    }
    return(list[indexes, ])
}

all_surveys <- function() {
    cached_variable('surveys', function() {
        xs <- xmlChildren(
            xmlRoot(
                nstac_api_call("getStatsList", list(statsNameList = "Y"))
                )[["DATALIST_INF"]]
            )
        xs <- xs[names(xs) == "LIST_INF"]
        names <- sapply(xs, function(n) xmlValue(n[["STAT_NAME"]]))
        codes <- sapply(xs, function(n) xmlGetAttr(n[["STAT_NAME"]], "code"))
        depts <- sapply(xs, function(n) xmlValue(n[["GOV_ORG"]]))

        data.frame(
            code   = codes,
            name   = names,
            author = depts,
            stringsAsFactors = FALSE)
    })
}

validate_query <- function(action, params, application.id) {
    api_spec      <- spec()
    valid.actions <- api_spec$actions
    valid.params  <- api_spec$params
    problems      <- 0

    if (!is.null(params$appId) & !is.na(application.id)) {
        if(params$appId != application.id) {
            warning("Conflicting application IDs provided, ",
                    "‘params$appId’ ignored.")
            problems <- problems + 1
        }
    }

    if (!(action %in% valid.actions)) {
        warning(paste("Unknown action ‘", action, "’", sep=""))
        problems <- problems + 1
    }

    if ((action %in% c("getMetaInfo", "getStatsData")) &
       is.null(params$statsDataId)) {
        which <- c("data", "metadata")[match(
            action, c("getMetaInfo", "getStatsData"))] 
        warning("No table or resource identifier given for ",
                which, " request.")
        problems <- problems + 1
    }

    for (n in names(params)) {
        if(!(n %in% valid.params[[action]])) {
            warning(paste("Unknown parameter ‘", n, "’", sep=""))
            problems <- problems + 1
        }
    }
    return(problems == 0)
}

format_resource_id <- function(id) {
    if (!is_digit_sequence(id)) {
        stop("Malformed resource ID: ‘", id, "’")
    } else {
        sprintf("%010d", as.numeric(id))
    }
}

#' @name parse_metadata
#' @title Parse XML metadata information from the NSTAC open data
#' server into an R data frame (internal function)
#' @param parent.node An xmlNode with TABLE_INF and CLASS_INF children
#' @return an \code{st_metadata} list of \code{table.data} and \code{classes}, both
#' \code{data.frame}s
parse_metadata <- function(parent.node) {
    handle_class_inf <- function(node) {
        classes  <- t(xmlSApply(node, function(node) {
            a <- xmlAttrs(node)
            cbind(a['code'],
                  a['name'],
                  a['level'],
                  a['unit'])
        }))
        colnames(classes) <- c('code', 'name', 'level', 'unit')
        rownames(classes) <- NULL

        cbind(class.id   = xmlGetAttr(node, "id"),
              class.name = xmlGetAttr(node, "name"),
              classes)
    }

    t.node <- parent.node[['TABLE_INF']]
    c.node <- parent.node[['CLASS_INF']]

    res <- list(
        table   = xmlSApply(t.node, xmlValue),
        classes = do.call(rbind, xmlSApply(c.node, handle_class_inf))
        )

    res <- lapply(res, data.frame, row.names = NULL)

    res
}

## experimental: shorter thanks to plyr::rbind.fill
parse_metadata2 <- function(parent.node) {
    handle_class_inf <- function(node) {
        classes  <- t(xmlSApply(node, xmlAttrs))
        rownames(classes) <- NULL
        cbind(class.id   = xmlGetAttr(node, "id"),
              class.name = xmlGetAttr(node, "name"),
              classes)
    }

    t.node <- parent.node[['TABLE_INF']]
    c.node <- parent.node[['CLASS_INF']]
    ## FIXME: does/should `handle_class_inf` return a data frame for
    ## rbind.fill to work?
    res <- list(
        table   = xmlSApply(t.node, xmlValue),
        classes = do.call(rbind.fill, xmlSApply(c.node, handle_class_inf))
        )
    res <- lapply(res, data.frame, row.names = NULL)

    res
}

## Returns just the data tables, but with column names and factor
## names extracted from the metadata
humanise <- function(x) UseMethod("humanise", x)

humanise.st_result <- function(result) {
    res  <- result$data
    cols <- colnames(res)[!colnames(res) %in% c("unit", "value")]
    vars <- result$metadata$classes
    ## replace subclass codes with names (eg 001, 002 --> Men, Women)
    for (c in cols) {
        codes    <-        vars[vars$class.id == c, "code"]
        names    <- factor(vars[vars$class.id == c, "name"])
        res[[c]] <- transcode(x=res[[c]], table=codes, y=names)
    }
    ## replace class ids with names (eg cat01 --> Gender)
    cat.names <- unique(vars[, c('class.id', 'class.name')])
    colnames(res) <- transcode(colnames(result$data),
                               cat.names[, 'id'],
                               cat.names[, 'name'])

    res
}
