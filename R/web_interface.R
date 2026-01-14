library(jsonlite)
library(httr)
library(tidyverse)


.request <- function(url) {
    tryCatch({
        return(list(value = fromJSON(url), exit = 0))
    }, error = function(e){
        return(list(value = invisible(NA), exit = 1))
    })
}

AphiaNameByAphiaID <- function(AphiaID) {
    tryCatch({
        fromJSON(sprintf("https://www.marinespecies.org/rest/AphiaNameByAphiaID/%d", AphiaID))
    }, error = function(e) {
        cli::cli_alert_danger("AphiaID {AphiaID} is not valid.")
        return(invisible(NA))
    })
}

.get_AphiaChildrenByAphiaID <- function(AphiaID, recursive = T, accept = T, marine = "true", extant = "true", pb) {
    # # check AphiaID
    # if (is.na(AphiaNameByAphiaID(AphiaID))) {
    #     return(invisible(NA))
    # } else {
    #     AphiaName <- AphiaNameByAphiaID(AphiaID)
    # }

    # init
    url_root <- "https://www.marinespecies.org/rest/AphiaChildrenByAphiaID/"
    offset <- 1
    result <- data.frame()

    repeat({
        url_request <- sprintf(
            "%s%d?marine_only=%s&extant_only=%s&offset=%d", 
            url_root, AphiaID, marine, extant, offset
        )
        reqres <- .request(url_request)
        if (reqres$exit == 1) {
            break
        } else {
            offset <- offset + 50
            result <- bind_rows(result, reqres$value)
        }
        cli::cli_progress_update(id = pb)
    })

    # exit if nothing retrieved
    if (all(dim(result) == c(0, 0))) {return(list(value = invisible(NA), exit = 1))}

    # filter: only accepted names
    if (accept) {
        result <- result %>% 
            filter(status == "accepted")
    }

    # return if not recursive
    if (!recursive) {return(list(value = result, exit = 0))}

    AphiaID_highrank <- result %>% 
        filter(rank != "Species") %>% 
        pull(AphiaID)

    # return if no higher rank to loop
    if (length(AphiaID_highrank) == 0) {return(list(value = result, exit = 0))}

    # recursive step
    result_rec <- map(AphiaID_highrank, ~{
        .get_AphiaChildrenByAphiaID(.x, T, T, marine, extant, pb)
    })
    result <- result_rec %>% 
        rlist::list.filter(exit == 0) %>% 
        map_dfr(~.x$value) 

    # exit if no children
    if (all(dim(result) == c(0, 0))) {return(list(value = invisible(NA), exit = 1))}

    return(list(value = result, exit = 0))
}

AphiaChildrenByAphiaID <- function(AphiaID, recursive = T, accept = T, marine = T, extant = T) {
    # check AphiaID
    if (is.na(AphiaNameByAphiaID(AphiaID))) {
        return(invisible(NA))
    } else {
        AphiaName <- AphiaNameByAphiaID(AphiaID)
    }

    if (marine) {marine <- "true"} else {marine <- "false"}
    if (extant) {extant <- "true"} else {extant <- "false"}

    pb <- cli::cli_progress_bar(
        format = "Retrieving AphiaID {cli::pb_spin} {cli::pb_current} request fetched.",
        total = NA 
    )

    result <- .get_AphiaChildrenByAphiaID(AphiaID, recursive, accept, marine, extant, pb)

    cli::cli_progress_done(id = pb)

    if (result$exit != 0) {
        cli::cli_alert_warning("No children found.")
        return(invisible(NA))
    }

    return(result$value)
}

t <- AphiaChildrenByAphiaID(118264)
