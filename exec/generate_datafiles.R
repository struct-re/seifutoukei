## UI message collection
messages <- list(
    skip.advice    = "To skip this step, set\n  options(seifutoukei.http.skip.confirmation = TRUE).",
    api.invalid    = "Invalid action or parameter(s).\nCheck your code against the API specification and package documentation.",
    http.confirm   = "Send HTTP request to nstac.go.jp?",
    app.id.missing = "No application ID provided."
    )

saveRDS(object=messages, file="inst/extdata/messages.rds")

## This code generates the list of valid parameter name values for API
##    version 1.0b, excluding the two "dataset" actions.
expand <- function(key)
    paste(c("lv", "cd", "cd", "cd"), key,
          c("", "", "From", "To"), sep="")
all_15 <- function(key)
    c(sapply(paste(key, sprintf("%02d", 1:15), sep=""), expand))

common_param_names <- c("appId", "lang")

api_spec <- list(
    actions = c("getStatsList", "getMetaInfo", "getStatsData"),
    params = lapply(list(
        getStatsList = c("surveyYears", "openYears", "statsField",
            "statsCode", "searchWord", "searchKind", "statsNameList"),
        getMetaInfo  = c("statsDataId"),
        getStatsData = c("statsDataId", expand("Tab"), expand("Time"),
            expand("Area"), all_15("Cat"), "startPosition", "limit",
            "metaGetFlg", "cntGetFlg")
        ), function(x) x <- c(common_param_names, x))
    )

saveRDS(object=api_spec, file="inst/extdata/api_spec.rds")
