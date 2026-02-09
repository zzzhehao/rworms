#' Get taxonomy (Records) from Aphia IDs
#' @param aphiaIDs A numeric vector of Aphia IDs.
#' @return A dataframe with one row of taxonomic information.
#' @importFrom dplyr left_join
#' @export
get_taxonomy <- function(aphiaIDs) {
    fullrecord <- AphiaRecordsFullByAphiaIDs(aphiaIDs) %>% mutate(aphiaID = as.numeric(aphiaID))
    taxonomy <- AphiaClassificationsByAphiaIDs(aphiaIDs)@value %>% mutate(aphiaID = as.numeric(aphiaID))
    res <- left_join(fullrecord, taxonomy)
    return(res)
}

#' Get taxonomy (records) of all species from a high taxon 
#' @param aphiaID Numeric. An Aphia ID of higher taxon (> species).
#' @return A dataframe with taxonomic information of all species of \code{aphiaID}.
#' @export
get_taxonomy_hr <- function(aphiaID) {
    aphiaIDs <- AphiaChildrenByAphiaID(aphiaID)@value$aphiaID
    get_taxonomy(aphiaIDs)
}

#' Get distribution records of all Aphia IDs (species)
#' @param aphiaIDs A numeric vector of Aphia IDs.
#' @return A dataframe of 
#' @importFrom dplyr left_join
#' @export
get_distributions <- function(aphiaIDs) {
    res.dis <- AphiaDistributionsByAphiaIDs(aphiaIDs)
    if (res.dis@exit > 0) {cli::cli_alert_warning(res.dis@message)}

    res.tax <- get_taxonomy(aphiaIDs)
    res.dis@value
    # left_join(res.dis@value) 
        # dplyr::select(scientificName, dplyr::everything())
}

