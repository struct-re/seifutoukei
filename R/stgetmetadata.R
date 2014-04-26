#' @title Obtain the metadata of a table or other resource available
#' in the Japanese official statistics open-data access system.
#' 
#' @param resource.id Numerical code of the resource, typically a
#' table, to look up metadata for.
#' @param lang Language for labels and values (ISO-639-1 code).
#' @return A list of \code{table}, a list of information about the
#' table, and \code{classes}, a data frame of the class (category)
#' codes and names used in the table.
#' @export
stgetmetadata <- function(resource.id, lang=NA) {
    id     <- format_resource_id(resource.id)
    params <- list(statsDataId = id)
    params <- add_lang_option(params, lang)
    res <- parse_metadata(
        xmlRoot(
            nstac_api_call("getMetaInfo", params)
            )[['METADATA_INF']]
        )
    res$table$RESOURCE_ID <- resource.id

    res
}
## ST統計表メタ情報 <- function(表コード, 言語=NA) stgetmetadata(表コード, 言語)
