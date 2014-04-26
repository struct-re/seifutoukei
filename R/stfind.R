#' @title Locate tables and other resources in the Japanese official
#' statistics open data access system
#'
#' @param keywords vector of keywords to search for (resources that
#' contain the keywords in metadata will be listed). Place names
#' should work as keywords.
#' @param survey.name set this to limit the search to data from one
#' survey. Use \code{stlistsurveys()} to get a list.
#' @param survey.date year and optionally month \code{"yyyy-mm"}, or
#' interval specified as starting and ending year and month
#' \code{c(from="yyyy-mm", to="yyyy-mm")}.
#' @param raw.params API parameters to pass through unmodified.
#' 
#' @importFrom XML xmlRoot append.xmlNode xmlChildren xmlValue xmlGetAttr xmlSApply removeChildren xmlToDataFrame xmlElementsByTagName
#' @importFrom lubridate ymd
#' @importFrom plyr arrange
#' @importMethodsFrom XML xmlToDataFrame
#' @export
stfind <- function(keywords=NULL, survey.name=NULL, survey.date=NULL,
                   ##, area=NULL, years=NULL, survey.author=NULL
                   raw.params=list()) {
    args <- as.list(match.call()[-1])
    if (all(is.null(args))) stop("Please specify at least one query parameter.")

    ## Prepare query parameters
    params <- list()

    ## keyword search
    if (any(!is.null(keywords))) {
        params$searchWord <- paste(keywords, collapse=" AND ")
    }

    ## specify source survey
    if (!is.null(survey.name)) {
        srv <- find_survey_code(survey.name)
        if (nrow(srv) == 1) {
            params$statsCode <- srv$code
        }
    }

    ## specify (year-month) date or date range
    ## preferred format: yyyy-mm or yyyy-mm-yyyy-mm
    if (!is.null(survey.date)) {
        survey.date <- gsub("–", "-",
                            gsub("-(\\d{2})(-|$)", "\\1\\2",
                                 survey.date))
        if (length(survey.date) == 2 &
            all(is_ymdate(survey.date))) {
            params$surveyYears <- paste(survey.date, collapse = '-')
        } else if (length(survey.date == 1) &
                   (is_sane_year(survey.date) |
                    is_ymdate(survey.date)    |
                    is_ymrange(survey.date))) {
            params$surveyYears <- survey.date
        } else {
            stop("Could not make sense of survey.date parameter '",
                 survey.date, "'.")
        }
    }

    if(length(raw.params) > 0) {
        warning('Passing through these parameters with no checks: ',
                paste(names(raw.params), raw.params, sep = '=', collapse = ', '))
        params <- c(params, raw.params)
    }

    ## Run DB queries and parse results
    results <- list(NULL, NULL, NULL)
    counts  <- rep(0, 3)
    ## run the search for:
    ## (1) standard tables
    results[[1]] <- nstac_api_call("getStatsList", params)
    ## (2) "小地域・地域メッシュ" tables
    results[[2]] <- nstac_api_call("getStatsList",
                            lmerge(params, list(searchKind="2")))
    ## (3) "社会・人口統計体系" tables
    results[[3]] <- nstac_api_call("getStatsList",
                            lmerge(params, list(searchKind="3")))

    ## combine the results
    for (i in c(1, 2, 3)) {
        root      <- xmlRoot(results[[i]])[['DATALIST_INF']]
        counts[i] <- as.numeric(xmlValue(root[['NUMBER']]))
        cat('Received', counts[i], 'results of type', i, '\n')
        if (counts[i] > 0) {
            local({
                root  <- removeChildren(root, 'NUMBER')
                items <- xmlElementsByTagName(root, 'LIST_INF')
                df    <- xmlToDataFrame(results[[i]],
                                        nodes = items,
                                        stringsAsFactors = FALSE)
                ## start of additional processing
                ## - add the resource ids as column "id"
                df$id <- xmlSApply(root, xmlGetAttr, 'id')
                ## - parse dates into month and year
                ## df[, c("survey.start.year",
                ##        "survey.start.month",
                ##        "survey.end.year",
                ##        "survey.end.month")] <- ymrange(df$SURVEY_DATE)
                ## end of additional processing
                results[[i]] <<- df
            })
        }
    }

    results[counts == 0] <- NULL

    if (sum(counts) > 0) {
        if (length(results) > 1) {
            columns <- lapply(results, names)
            if(!do.call(identical, columns)) {
                stop('Incompatible formats: ', paste(columns, collapse="; "))
            }
            res <- do.call(rbind, results)
        } else {
            res <- results[[1]]
        }
        row.names(res) <- NULL
        ## reorganise logically
        res2 <- data.frame(
            id            = res$id,
            name          = res$TITLE,
            survey.name   = res$STAT_NAME,
            survey.date   = ympunct(res$SURVEY_DATE),
            survey.author = res$GOV_ORG,
            publ.title    = res$STATISTICS_NAME,
            publ.cycle    = res$CYCLE,
            publ.date     = res$OPEN_DATE,
            is.shouchiiki = res$SMALL_AREA
            )
        ## guess blank years
        if (any(res2$survey.date == 0)) {
            res2$survey.title.year <-
                guess_survey_year_from_title(res2$publ.title)
        }

        ## sort by date, survey, and resource id
        res2 <- arrange(res2, survey.date, survey.name, id)

        ## kill the is.shouchiiki column if full of zeroes
        if (all(res2$is.shouchiiki == 0)) res2[, 'is.shouchiiki'] <- NULL

        ## kill the publ.cycle column if full of dashes
        if (all(res2$publ.cycle == '-')) res2[, 'publ.cycle'] <- NULL

        res2
    } else {

        NULL
    }
}

## ST統計表検索 <- function(地域=NA, 年=NA, キーワード=NA, 調査名=NA, 作成機関=NA) {
##     st_find(area=地域, years=年, keywords=キーワード,
##             survey.name=調査名, survey.author=作成機関)
## }

######################################################################

#' @title List all surveys from which data can be obtained through the
#' official statistics open data system
#' @export
stlistsurveys <- function() {
    all_surveys()
}
## ST調査一覧 <- st_list_available_surveys

######################################################################

st_survey_info <- function(survey) {
    survey.codes <- find_survey_code(survey)
    if (length(survey.codes) > 1) {
        cat("More than one survey matches your query:\n")
        print(survey.codes)
    }
    if (length(survey.codes) != 1) {
        stop(paste("No available survey has the exact name or code ‘",
                   survey, "’.", sep=""))
    } else {
        ## FIXME
        ## look up number of tables by publication and year

        ## TODO later: gather some links to the official
        ## explanations of survey methodology etc, and distribute with
        ## the package to feed this function
    }
    
}
## ST調査情報 <- function(調査) st_survey_info(survey=調査)
