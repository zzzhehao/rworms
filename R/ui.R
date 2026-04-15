#' Get taxonomy information (Records) from Aphia IDs
#' 
#' @description
#' Get the taxonomy information of a given aphia ID. 
#' 
#' @param aphiaIDs A numeric vector of Aphia IDs.
#' @return A dataframe with one row of taxonomic information.
#' @importFrom dplyr left_join
#' @seealso [get_taxonomy_hr()]
#' @export
get_taxonomy <- function(aphiaIDs) {
    fullrecord <- AphiaRecordsFullByAphiaIDs(aphiaIDs) %>% mutate(aphiaID = as.numeric(aphiaID))
    taxonomy <- AphiaClassificationsByAphiaIDs(aphiaIDs)@value %>% mutate(aphiaID = as.numeric(aphiaID))
    res <- left_join(fullrecord, taxonomy)
    return(res)
}

#' Get taxonomy (records) of all species from a high taxon 
#' 
#' @description
#' Get the taxonomy information of all members of the given aphia ID. 
#' 
#' 
#' @param aphiaID Numeric. An Aphia ID of higher taxon (> species).
#' @param recursive Logical. Whether to get the taxonomy of direct children only (FALSE). Default is TRUE, which means to get the taxonomy of all members recursively untill it reaches the lowest level given by \code{rank}.
#' @param rank Character. The lowest taxonomic rank to be retrieved. Default is "Species". If recursive is TRUE, the function will retrieve all members until it reaches the specified rank. If recursive is FALSE, this argument will be ignored. Accepted values are taxonomic ranks with the first letter capitalized. 
#' @param ... Further arguments passed to [AphiaChildrenByAphiaID()]
#' @return A dataframe with taxonomic information of all species of \code{aphiaID}.
#' @seealso [get_taxonomy()]
#' @export
get_taxonomy_hr <- function(aphiaID, recursive = TRUE, rank = "Species", ...) {
    aphiaIDs <- AphiaChildrenByAphiaID(aphiaID, recursive = recursive, ...)@value$aphiaID
    get_taxonomy(aphiaIDs)
}

#' Get distribution records of all Aphia IDs (species)
#' @param aphiaIDs A numeric vector of Aphia IDs.
#' @return A dataframe of 
#' @importFrom dplyr left_join
#' @export
get_distribution <- function(aphiaIDs) {
    res.dis <- AphiaDistributionsByAphiaIDs(aphiaIDs)
    if (res.dis@exit > 0) {cli::cli_alert_warning(res.dis@message)}

    res.tax <- get_taxonomy(aphiaIDs)
    left_join(res.dis@value, res.tax, by = "aphiaID") %>% 
        dplyr::select(scientificName, dplyr::everything())
}

