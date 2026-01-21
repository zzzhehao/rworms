#' @importFrom dplyr left_join
get_records <- function(aphiaIDs) {
    fullrecord <- AphiaRecordsFullByAphiaIDs(aphiaIDs) %>% mutate(aphiaID = as.numeric(aphiaID))
    taxonomy <- AphiaClassificationsByAphiaIDs(aphiaIDs)@value %>% mutate(aphiaID = as.numeric(aphiaID))
    res <- left_join(fullrecord, taxonomy)
    return(res)
}

get_records_hr <- function(aphiaID) {
    aphiaIDs <- AphiaChildrenByAphiaID(aphiaID)@value$aphiaID
    get_records(aphiaIDs)
}
