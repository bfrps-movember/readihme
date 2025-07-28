#' Tidy IHME API Response
#'
#' Converts raw JSON responses from the IHME API into clean data frames.
#' This function is specifically designed to handle the IHME API's data structure.
#'
#' @param response Raw JSON response from IHME API
#'
#' @return Tidied data frame
#' @export
tidy_ihme_response <- function(response) {
  if (is.null(response) || length(response) == 0) {
    return(data.frame())
  }

  # Handle IHME API specific structure
  if (is.list(response) && "results" %in% names(response)) {
    # Extract the actual data from the results field
    response <- response$results
  }

  # If response is still empty after extracting results
  if (is.null(response) || length(response) == 0) {
    return(data.frame())
  }

  # Handle different response structures
  if (is.list(response) && !is.data.frame(response)) {
    # Check if this is a list of records (typical IHME structure)
    if (all(sapply(response, is.list))) {
      # Convert each record to a clean data frame row
      clean_records <- purrr::map(response, ~{
        if (is.list(.x)) {
          # Flatten the record, handling nested structures
          flattened <- flatten_ihme_record(.x)
          # Convert to single row data frame
          as.data.frame(flattened, stringsAsFactors = FALSE, check.names = FALSE)
        } else {
          data.frame(value = .x, stringsAsFactors = FALSE)
        }
      })

      # Remove the first row if it contains API metadata (common in IHME responses)
      if (length(clean_records) > 1) {
        # Check if first record is metadata (contains api_version or similar)
        first_record <- clean_records[[1]]
        if ("api_version" %in% names(first_record) && all(is.na(first_record[1, ]))) {
          clean_records <- clean_records[-1]
        }
      }

      # Bind all records together
      if (length(clean_records) > 0) {
        tryCatch({
          dplyr::bind_rows(clean_records)
        }, error = function(e) {
          # Fallback: standardize columns manually
          all_cols <- unique(unlist(lapply(clean_records, names)))
          standardized <- purrr::map(clean_records, ~{
            missing_cols <- setdiff(all_cols, names(.x))
            for (col in missing_cols) {
              .x[[col]] <- NA
            }
            .x[all_cols]
          })
          do.call(rbind, standardized)
        })
      } else {
        data.frame()
      }
    } else {
      # Simple list - convert to data frame
      flattened <- flatten_ihme_record(response)
      as.data.frame(flattened, stringsAsFactors = FALSE, check.names = FALSE)
    }
  } else if (is.data.frame(response)) {
    response
  } else {
    # Fallback
    data.frame(value = response, stringsAsFactors = FALSE)
  }
}

#' Flatten IHME Record
#'
#' Internal function to flatten nested IHME records properly.
#'
#' @param record A single record from IHME API
#' @return Flattened list
#' @keywords internal
flatten_ihme_record <- function(record) {
  if (!is.list(record)) {
    return(list(value = record))
  }

  flattened <- list()

  for (name in names(record)) {
    value <- record[[name]]

    if (is.null(value)) {
      flattened[[name]] <- NA
    } else if (is.list(value) && length(value) == 1 && !is.null(names(value))) {
      # This is likely a nested object - extract its values with prefixed names
      nested <- flatten_ihme_record(value)
      for (nested_name in names(nested)) {
        new_name <- paste(name, nested_name, sep = "_")
        flattened[[new_name]] <- nested[[nested_name]]
      }
    } else if (is.list(value) && length(value) > 1) {
      # Multiple values - convert to character string
      flattened[[name]] <- paste(unlist(value), collapse = "; ")
    } else {
      # Simple value
      flattened[[name]] <- value
    }
  }

  return(flattened)
}

#' Clean IHME Column Names
#'
#' Internal function to clean up messy column names from IHME API responses.
#'
#' @param df Data frame with potentially messy column names
#' @return Data frame with cleaned column names
#' @keywords internal
clean_ihme_columns <- function(df) {
  if (ncol(df) == 0) return(df)

  # Remove numbered suffixes from duplicate column names
  clean_names <- names(df)
  clean_names <- stringr::str_replace(clean_names, "\\.\\.\\.\\d+$", "")

  # Handle duplicates by adding suffixes
  clean_names <- make.unique(clean_names, sep = "_")

  names(df) <- clean_names
  return(df)
}

#' Find Countries by Name Pattern
#'
#' Search for countries/locations that match a given text pattern.
#'
#' @param pattern Text pattern to search for in country names
#' @param ignore_case Whether to ignore case in search (default: TRUE)
#'
#' @return Data frame of matching locations
#' @export
#'
#' @examplesIf interactive() && curl::has_internet()
#' find_countries("united")
#' find_countries("africa")
find_countries <- function(pattern, ignore_case = TRUE) {
  locations <- get_locations()

  # Clean the data first
  locations <- clean_ihme_columns(locations)

  if ("location_name" %in% names(locations)) {
    locations |>
      dplyr::filter(stringr::str_detect(location_name,
                                        stringr::regex(pattern, ignore_case = ignore_case)))
  } else {
    message("Location data structure may have changed. Returning raw data.")
    locations
  }
}

#' Create Location Lookup Table
#'
#' Creates a simplified lookup table with location IDs and names for easy reference.
#'
#' @return Data frame with location codes and names for easy lookup
#' @export
#'
#' @examplesIf interactive() && curl::has_internet()
#' create_location_lookup()
create_location_lookup <- function() {
  locations <- get_locations()

  # Clean the data first
  locations <- clean_ihme_columns(locations)

  # Create a simplified lookup table
  if ("location_name" %in% names(locations) && "location_id" %in% names(locations)) {
    locations |>
      dplyr::select(location_id, location_name) |>
      dplyr::arrange(location_name) |>
      dplyr::filter(!is.na(location_id), !is.na(location_name))
  } else {
    message("Location data structure may have changed. Returning raw data.")
    locations
  }
}

#' Validate Year Range
#'
#' Internal function to validate that years are within the acceptable range.
#'
#' @param years Vector of years to validate
#' @return Logical indicating if all years are valid
#' @keywords internal
validate_years <- function(years) {
  if (is.null(years)) return(TRUE)

  valid_range <- 1990:2030
  invalid_years <- years[!years %in% valid_range]

  if (length(invalid_years) > 0) {
    warning(glue::glue("Invalid years detected: {paste(invalid_years, collapse = ', ')}. Valid range is 1990-2030."))
    return(FALSE)
  }

  TRUE
}

#' Build API Parameter List
#'
#' Internal helper function to build parameter lists for API calls.
#'
#' @param ... Named parameters to include in the API call
#' @return List of non-null parameters
#' @keywords internal
build_params <- function(...) {
  params <- list(...)
  # Remove NULL parameters
  params[!sapply(params, is.null)]
}
