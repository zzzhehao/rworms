
.db_url <- function() {
    "https://www.marinespecies.org/rest"
}

.has_error <- function(reqres.ls, on_error = "warning") {
    exits <- map_vec(reqres.ls, ~.x@exit)
    idx <- exits > 0
    error <- any(idx)
    if (error) {
        if (on_error == "warning") {
            cli::cli_alert_warning("Index {which(idx)} exited non 0.")
        } else if (on_error == "abort") {
            cli::cli_abort("Index {which(idx)} exited non 0.")
        }
        return(max(exits))
    } else {
        return(0)
    }
}

.bind_values <- function(reqres.ls) {
    map_dfr(reqres.ls, ~.x@value)
}

AphiaNameByAphiaID <- function(aphiaID) {
    url <- sprintf("%s/AphiaNameByAphiaID/%d", .db_url(), aphiaID)
    reqres <- request(url)
    if (reqres@exit == 1){
        cli::cli_alert_danger("aphiaID {aphiaID} is not valid.")
    }

    result(reqres@value, reqres@exit, "") %>% 
        return()
}

AphiaRecordByAphiaID <- function(aphiaID) {
    # .is_valid_aphiaID(aphiaID)
    url <- sprintf("%s/AphiaRecordByAphiaID/%d", .db_url(), aphiaID)
    reqres <- request(url)

    result(reqres@value, reqres@exit, "") %>% 
        return()
}

AphiaRecordsByAphiaIDs <- function(aphiaIDs) {
    if (length(aphiaIDs) == 1) {
        return(AphiaRecordByAphiaID(aphiaIDs))
    } else if (length(aphiaIDs) > 50) {
        batch <- length(aphiaIDs) %/% 50 + 1
    } else if (length(aphiaIDs) > 1 && length(aphiaIDs) <= 50) {
        batch <- 1
    }

    res <- map(1:batch, ~{
        if (.x == batch) {
            idx <- (50*(.x-1)+1):length(aphiaIDs)
        } else {
            idx <- (50*(.x-1)+1):(50*.x)
        }

        ids <- paste0("aphiaids%5B%5D=", aphiaIDs[idx]) %>% 
            paste(collapse = "&")
        url <- sprintf("%s/AphiaRecordsByAphiaIDs?%s", .db_url(), ids) 
        reqres <- request(url) 
        return(reqres)
    })

    exits <- .has_error(res)

    res <- .bind_values(res)
    
    result(res, exits) %>% return()
}

.is_valid_aphiaID <- function(aphiaIDs, abort = T, idx = F) {
    pb <- cli::cli_progress_bar("Checking AphiaID ...")
    records.request <- AphiaRecordsByAphiaIDs(aphiaIDs)
    cli::cli_progress_done(pb)
    isna.ls <- records.request@value$scientificname %>% is.na()
    is.empty <- length(isna.ls) == 0
    isvalid <- all(c(!isna.ls, !is.empty))

    if (is.empty) {cli::cli_abort("All AphiaID are not valid. Check input.")}
    if (!isvalid && abort) {cli::cli_abort("AphiaID {aphiaIDs[isna.ls]} is not valid.")}
    if (idx) {return(isna.ls)} else {return(isvalid)}
}

#' @importFrom dplyr bind_rows
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#' @importFrom dplyr %>% 
#' @importFrom dplyr rename
#' @importFrom purrr map
#' @importFrom purrr map_dfr
.get_AphiaChildrenByAphiaID <- function(aphiaID, recursive = T, accept = T, marine = "true", extant = "true", pb) {
    # init
    url_root <- sprintf("%s/AphiaChildrenByAphiaID/", .db_url())
    offset <- 1
    result <- data.frame()

    repeat({
        request <- sprintf(
            "%s%d?marine_only=%s&extant_only=%s&offset=%d", 
            url_root, aphiaID, marine, extant, offset
        )
        reqres <- request(request)
        if (reqres@exit == 1) {
            break
        } else {
            offset <- offset + 50
            result <- bind_rows(result, reqres@value)
        }
        cli::cli_progress_update(id = pb)
    })

    # exit if nothing retrieved
    if (all(dim(result) == c(0, 0))) {return(list(value = invisible(NULL), exit = 1))}

    # filter: only accepted names
    if (accept) {
        result <- result %>% 
            filter(status == "accepted")
    }

    # return if not recursive
    if (!recursive) {return(list(value = result, exit = 0))}

    aphiaID_highrank <- result %>% 
        filter(rank != "Species") %>% 
        rename(aphiaID = AphiaID) %>% 
        pull(aphiaID) 

    # return if no higher rank to loop
    if (length(aphiaID_highrank) == 0) {return(list(value = result, exit = 0))}

    # recursive step
    result_rec <- map(aphiaID_highrank, ~{
        .get_AphiaChildrenByAphiaID(.x, T, T, marine, extant, pb)
    })
    result_rec <- result_rec %>% 
        rlist::list.filter(exit == 0) %>% 
        map_dfr(~.x$value) 

    result_merged <- bind_rows(
        result %>% rename(aphiaID = AphiaID) %>% filter(!aphiaID %in% aphiaID_highrank),
        result_rec %>% rename(aphiaID = AphiaID)
    )

    # exit if no children
    if (all(dim(result_merged) == c(0, 0))) {return(list(value = invisible(NA), exit = 1))}

    return(list(value = result_merged, exit = 0))
}

AphiaChildrenByAphiaID <- function(aphiaID, recursive = T, accept = T, marine = T, extant = T) {
    .is_valid_aphiaID(aphiaID)
    if (marine) {marine <- "true"} else {marine <- "false"}
    if (extant) {extant <- "true"} else {extant <- "false"}

    pb <- cli::cli_progress_bar(
        format = "Retrieving aphiaID {cli::pb_spin} {cli::pb_current} request fetched.",
        total = NA 
    )

    result <- .get_AphiaChildrenByAphiaID(aphiaID, recursive, accept, marine, extant, pb)

    cli::cli_progress_done(id = pb)

    if (result$exit != 0) {
        cli::cli_alert_warning("No children found.")
        return(invisible(NULL))
    }

    res <- result$value 
        # rename(aphiaID = AphiaID)

    result(res, 0) %>% return()
}

AphiaRecordFullByAphiaID <- function(aphiaID) {
    .is_valid_aphiaID(aphiaID)
    url <- sprintf("%s/AphiaRecordFullByAphiaID/%d", .db_url(), aphiaID)
    reqres <- request(url, media = "application/ld+json")
    result(reqres@value, reqres@exit, reqres@message) %>% return()
}

#' @importFrom purrr map_vec
AphiaRecordsFullByAphiaIDs <- function(aphiaIDs) {
    .is_valid_aphiaID(aphiaIDs)

    pb <- cli::cli_progress_bar(
        format = "Retrieving Aphia Records {cli::pb_current} {cli::pb_bar} {cli::pb_percent} {cli::pb_eta}",
        total = length(aphiaIDs)
    )

    reqres <- map(aphiaIDs, ~{
        cli::cli_progress_update(id = pb)
        AphiaRecordFullByAphiaID(.x)
    })

    cli::cli_progress_done(id = pb)

    values <- map_dfr(reqres, ~.x@value)
    exits <- map_vec(reqres, ~.x@exit)
    if (any(exits > 0)) {
        idx <- which(exits > 0)
        cli::cli_alert_danger("AphiaID {aphiaIDs[idx]} returned nothing.")
    }
    return(values)
}

#' @importFrom dplyr mutate
AphiaDistributionsByAphiaID <- function(aphiaID) {
    .is_valid_aphiaID(aphiaID)
    AphiaRecord <- AphiaRecordByAphiaID(aphiaID)
    if (AphiaRecord@value$rank != "Species") {
        cli::cli_alert_danger("AphiaID {aphiaID} is not a species. No distribution data is retrieved.")
        result(NULL, reqres@exit) %>% return()
    }

    url <- sprintf("%s/AphiaDistributionsByAphiaID/%d", .db_url(), aphiaID)
    reqres <- request(url)
    if (reqres@exit > 0) {
        cli::cli_alert_danger("AphiaID {aphiaID}: {AphiaNameByAphiaID(aphiaID)@value} does not have any distribution data.")
        result(NULL, reqres@exit) %>% return()
    } else {
        res <- reqres@value %>% 
            mutate(aphiaID = aphiaID)
        result(res, reqres@exit) %>% return()
    }
}

#' Get Distribution from Aphia IDs
AphiaDistributionsByAphiaIDs <- function(aphiaIDs) {
    .is_valid_aphiaID(aphiaIDs)
    AphiaRecords <- AphiaRecordsByAphiaIDs(aphiaIDs)@value
    highrank <- AphiaRecords$rank != "Species"
    if (any(highrank)) {cli::cli_alert_warning("AphiaID {aphiaIDs[which(highrank)]} are not species. No distribution data will be retrieved.")}
    
    pb <- cli::cli_progress_bar(
        format = "Retrieving Distribution Records {cli::pb_current} {cli::pb_bar} {cli::pb_percent}",
        total = length(aphiaIDs)
    )

    reqres <- map(aphiaIDs[!highrank], ~{
        cli::cli_progress_update(id = pb)
        AphiaDistributionsByAphiaID(.x)
    })

    exits <- .has_error(reqres)
    res <- .bind_values(reqres)

    cli::cli_progress_done(id = pb)

    result(res, exits) %>% return()
}

#' @importFrom rrapply rrapply
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr pivot_longer
#' @importFrom tibble as_tibble
AphiaClassificationByAphiaID <- function(aphiaID, df = "wide") {
    url <- sprintf("%s/AphiaClassificationByAphiaID/%d", .db_url(), aphiaID)
    reqres <- request(url)
    if (reqres@exit > 1) {
        cli::cli_alert_danger("AphiaID {aphiaID} failed to retrieve taxonomic classification data.")
        return(result(NULL, 1, "Failed to retrieve taxonomic classification data."))
    }
    
    tf <- reqres@value %>% rrapply(how = "flatten")

    df_long <- map_dfr(1:((length(tf)-1)/3), \(i){
        tf[-length(tf)][((i-1)*3+1):(i*3)] %>% 
            as_tibble()
    }) %>% 
        rename(aphiaID = AphiaID)

    df_wide <- df_long %>% 
        pivot_wider(., id_cols = !aphiaID, names_from = rank, values_from = scientificname) %>% 
        mutate(aphiaID = aphiaID, .before = everything())

    if (df == "wide") {
        result(df_wide, reqres@exit) %>% return()
    } else if (df == "long") {    
        result(df_long, reqres@exit) %>% return()
    }
}

#' Get taxonomy from aphia IDs
#' @export
AphiaClassificationsByAphiaIDs <- function(aphiaIDs) {
    pb <- cli::cli_progress_bar(
        format = "Retrieving Taxnonmy {cli::pb_current} {cli::pb_bar} {cli::pb_percent}",
        total = length(aphiaIDs)
    )

    reqres <- map(aphiaIDs, ~{
        cli::cli_progress_update(id = pb)
        AphiaClassificationByAphiaID(.x)
    })

    cli::cli_progress_done(id = pb)

    exits <- .has_error(reqres)
    values <- .bind_values(reqres)
    result(values, exits)
}
