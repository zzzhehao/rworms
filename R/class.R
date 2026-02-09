#' Execute Request
#' 
#' Internal. Interface that execute communication with WoRMS webservice
#' 
#' @importFrom S7 new_class
#' @importFrom S7 class_character
#' @importFrom S7 class_any
#' @importFrom S7 class_numeric
#' @importFrom S7 new_object
#' @importFrom S7 S7_object
#' @importFrom jsonlite fromJSON
request <- new_class(
    "request",
    properties = list(
        url = class_character,
        value = class_any,
        exit = class_numeric,
        message = class_character
    ),
    constructor = function(url, media = "application/json", query = list(), http_error = F, request_error = T, ...) {
        # AphiaRecordFull is the only response in json-ld
        parse_json_ld <- function(graph_list) {
            
            node_map <- graph_list %>% 
                purrr::map(function(x) {
                    if (!is.null(x[["@id"]])) stats::setNames(list(x), x[["@id"]]) else NULL
                }) %>% 
                purrr::list_flatten()
            
            main_node <- graph_list %>% 
                purrr::keep(~ any(grepl("scientificName$", names(.x)))) %>%
                .[[1]]
            
            if (is.null(main_node)) return(list())

            clean_record <- purrr::imap(main_node, function(val, key) {
                
                # skip metadata
                if (grepl("^@", key)) return(NULL)
                
                simple_key <- tail(strsplit(key, "[/#]")[[1]], 1)
                content <- val[[1]]
                
                # citation
                if (simple_key == "source" && !is.null(content[["@id"]])) {
                    linked_node <- node_map[[ content[["@id"]] ]]
                    
                    if (!is.null(linked_node)) {
                        
                        cit_val <- NA
                        cit_key <- grep("bibliographicCitation$", names(linked_node), value = TRUE)
                        if (length(cit_key) > 0) cit_val <- linked_node[[cit_key]][[1]][["@value"]]
                        
                        year_val <- NA
                        year_key <- grep("datePublished$", names(linked_node), value = TRUE)
                        if (length(year_key) > 0) year_val <- linked_node[[year_key]][[1]][["@value"]]
                        
                        doi_val <- NA
                        doi_key <- grep("source$", names(linked_node), value = TRUE)
                        if (length(doi_key) > 0) {
                            raw_doi <- linked_node[[doi_key]][[1]][["@id"]]
                            doi_val <- gsub("https?://(www\\.)?doi\\.org/", "", raw_doi)
                        }

                        return(list(
                            reference_citation = cit_val,
                            reference_doi = doi_val,
                            reference_year = year_val
                        ))
                    }
                }

                final_val <- NA
                if (!is.null(content[["@value"]])) {
                    final_val <- content[["@value"]]
                } else if (!is.null(content[["@id"]])) {
                    final_val <- tail(strsplit(content[["@id"]], "[/#]")[[1]], 1)
                }
                
                stats::setNames(list(final_val), simple_key)
            })
            
            # 4. Final Cleanup
            clean_record %>% 
                purrr::compact() %>% 
                unname() %>% 
                purrr::list_flatten()
        }

        res <- tryCatch({
            cli <- crul::HttpClient$new(
                url = url,
                headers = list(Accept = media),
                opts = list(...)
            )
            temp <- cli$get()

            if (temp$status_code > 201) { # HTTP Error: 1
                if (http_error) {
                    cli::cli_alert_danger("Error: {temp$status_code}: {temp$status_http()$message}, url: {url}")
                }

                list(
                    value = invisible(NULL), 
                    exit = 1, 
                    message = sprintf("HTTP Error %s: %s", temp$status_code, temp$status_http()$message)
                )
            } else {
                raw_text <- temp$parse("UTF-8")
            
                if (media == "application/ld+json") {
                    raw_json <- jsonlite::fromJSON(raw_text, flatten = F, simplifyVector = F)
                    value <- parse_json_ld(raw_json) %>% as.data.frame()
                } else if (media == "application/json") {
                    value <- jsonlite::fromJSON(raw_text, flatten = T)
                }

                if ("AphiaID" %in% names(value)) {
                    value <- value %>% 
                        dplyr::rename("aphiaID" = "AphiaID")
                }

                list(
                    value = value, 
                    exit = 0, 
                    message = "Request responded successfully."
                )
            }
        }, error = function(e){ # Request failed: 2
            if (request_error) {
                cli::cli_alert_danger("Error: {e$message}, url: {url}")
            }
            
            list(
                value = invisible(NULL), 
                exit = 2, 
                message = sprintf("Request failed: %s", e$message)
            )
        })
        obj <- new_object(
            S7_object(),
            url = url,
            value = res$value,
            exit = res$exit,
            message = res$message
        )
        class(obj) <- c("url_request", class(obj))
        return(obj)
    }
)

result <- new_class(
    "result",
    properties = list(
        value = class_any,
        exit = class_numeric,
        message = class_character
    ), 
    constructor = function(value, exit, message = "") {
        obj <- new_object(
            S7_object(),
            value = value,
            exit = exit,
            message = message
        )

        class(obj) <- c("result", class(obj))
        return(obj)
    }
)