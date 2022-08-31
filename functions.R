fetch_ldc <- function(keys,
                      key_type = "ecosite",
                      data_type = "lpi",
                      verbose = FALSE){
  
  if (!(data_type %in% c("lpi", "height", "gap", "soil"))) {
    stop("data_type must be 'lpi', 'height', 'gap', or 'soil'.")
  }
  current_data_source <- switch(data_type,
                                "lpi" = {"datalpi"},
                                "height" = {"dataheight"},
                                "gap" = {"datagap"},
                                "soil" = {"datasoilstability"})
  
  if (!(key_type %in% c("ecosite", "primarykey", "projectkey"))) {
    stop("key_type must be 'ecosite', 'primarykey', 'projectkey'.")
  }
  current_key_type <- switch(key_type,
                             "ecosite" = {"EcologicalSiteId"},
                             "primarykey" = {"PrimaryKey"},
                             "projectkey" = {"ProjectKey"})
  
  query_results_list <- lapply(X = keys,
                               current_data_source = current_data_source,
                               current_key_type = current_key_type,
                               FUN = function(X, current_data_source, current_key_type){
                                 # Build the query
                                 query <- paste0("https://api.landscapedatacommons.org/api/",
                                                 current_data_source, "?",
                                                 current_key_type, "=",
                                                 X)
                                 
                                 # Getting the data via curl
                                 # connection <- curl::curl(query)
                                 # results_raw <- readLines(connection)
                                 # results <- jsonlite::fromJSON(results_raw)
                                 if (verbose) {
                                   message("Attempting to query EDIT with:")
                                   message(query)
                                 }
                                 
                                 # Full query results
                                 full_results <- httr::GET(query,
                                                           config = httr::timeout(60))
                                 # Grab only the data portion
                                 results_raw <- full_results[["content"]]
                                 # Convert from raw to character
                                 results_character <- rawToChar(results_raw)
                                 # Convert from character to data frame
                                 results <- jsonlite::fromJSON(results_character)
                                 if (verbose) {
                                   message("Results converted from json to character")
                                 }
                                 
                                 results
                               })
  
  results <- do.call(rbind,
                     query_results_list)
  
  # So we can tell the user later which actually got queried
  if (is.null(results)) {
    warning("No results returned. There are no records in the LDC for the supplied keys.")
    return(results)
  } else {
    if (verbose) {
      message("Determining if keys are missing.")
    }
    queried_keys <- unique(results[[current_key_type]])
    missing_keys <- keys[!(keys %in% queried_keys)]
  }
  
  if (length(missing_keys) > 0) {
    missing_key_warning <- paste0("The following keys did not return data from the LDC: ",
                   paste(missing_keys,
                         collapse = ", "))
    warning(missing_key_warning)
  }
  
  
  # Only keep going if there are results!!!!
  if (length(results) > 0) {
    # Convert from character to numeric variables where possible
    data_corrected <- lapply(X = names(results),
                             data = results,
                             FUN = function(X, data){
                               # Get the current variable values as a vector
                               vector <- data[[X]]
                               # Try to coerce into numeric
                               numeric_vector <- suppressWarnings(as.numeric(vector))
                               # If that works without introducing NAs, return the numeric vector
                               # Otherwise, return the original character vector
                               if (all(!is.na(numeric_vector))) {
                                 return(numeric_vector)
                               } else {
                                 return(vector)
                               }
                             })
    
    # From some reason co.call(cbind, data_corrected) was returning a list not a data frame
    # so I'm resorting to using dplyr
    data <- suppressMessages(dplyr::bind_cols(data_corrected))
    # Correct the names of the variables
    names(data) <- names(results)
    
    # Put it in the workspace list
    return(data)
  } else {
    return(results)
  }
}
