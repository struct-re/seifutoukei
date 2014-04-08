#' @name stgetmetadata
#' @title Obtain the metadata of a table or other resource available
#' in the Japanese official statistics open-data access system.
#' 
#' @param resource.id Numerical code of the resource to look up
#' metadata for.
#' @param lang Language of labels and values (ISO-639-1 code).
#' @return An \code{st_metadata}-class list of \code{table.data} and
#' \code{classes}, both data frames.
#' @export
stgetmetadata <- function(resource.id, lang=NA) {
    id     <- format_resource_id(resource.id)
    params <- list(statsDataId = id)
    params <- add_lang_option(params, lang)
    parse_metadata(
        xmlRoot(
            nstac_api_call("getMetaInfo", params)
            )[['METADATA_INF']]
        )
}
## ST統計表メタ情報 <- function(表コード, 言語=NA) stgetmetadata(表コード, 言語)
