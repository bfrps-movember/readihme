#' Tidy IHME API Response
#'
#' Converts raw JSON responses from the IHME API into clean data frames.
#'
#' @param response Raw JSON response from IHME API
#' @return Tidied data frame
#' @export
tidy_ihme_response <- function(response) {
  if (is.null(response) || length(response) == 0) {
    return(data.frame())
  }

  if (is.list(response) && "results" %in% names(response)) {
    response <- response$results
  }

  if (is.null(response) || length(response) == 0) {
    return(data.frame())
  }

  if (is.list(response) && !is.data.frame(response)) {
    if (all(sapply(response, is.list))) {
      clean_records <- purrr::map(response, ~{
        if (is.list(.x)) {
          flattened <- flatten_ihme_record(.x)
          as.data.frame(flattened, stringsAsFactors = FALSE, check.names = FALSE)
        } else {
          data.frame(value = .x, stringsAsFactors = FALSE)
        }
      })

      if (length(clean_records) > 1) {
        first_record <- clean_records[[1]]
        if ("api_version" %in% names(first_record) && all(is.na(first_record[1, ]))) {
          clean_records <- clean_records[-1]
        }
      }

      if (length(clean_records) > 0) {
        tryCatch({
          dplyr::bind_rows(clean_records)
        }, error = function(e) {
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
      flattened <- flatten_ihme_record(response)
      as.data.frame(flattened, stringsAsFactors = FALSE, check.names = FALSE)
    }
  } else if (is.data.frame(response)) {
    response
  } else {
    data.frame(value = response, stringsAsFactors = FALSE)
  }
}

#' Flatten IHME Record
#'
#' Internal function to flatten nested IHME records.
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
      nested <- flatten_ihme_record(value)
      for (nested_name in names(nested)) {
        new_name <- paste(name, nested_name, sep = "_")
        flattened[[new_name]] <- nested[[nested_name]]
      }
    } else if (is.list(value) && length(value) > 1) {
      flattened[[name]] <- paste(unlist(value), collapse = "; ")
    } else {
      flattened[[name]] <- value
    }
  }

  return(flattened)
}

#' Clean IHME Column Names
#'
#' @param df Data frame with potentially messy column names
#' @return Data frame with cleaned column names
#' @keywords internal
clean_ihme_columns <- function(df) {
  if (ncol(df) == 0) return(df)

  clean_names <- names(df)
  clean_names <- stringr::str_replace(clean_names, "\\.\\.\\.\\d+$", "")
  clean_names <- make.unique(clean_names, sep = "_")

  names(df) <- clean_names
  return(df)
}

#' Find Countries by Name Pattern
#'
#' @param pattern Text pattern to search for in country names
#' @param ignore_case Whether to ignore case in search (default: TRUE)
#' @return Data frame of matching locations
#' @export
#'
#' @examplesIf interactive() && curl::has_internet()
#' find_countries("united")
#' find_countries("africa")
find_countries <- function(pattern, ignore_case = TRUE) {
  locations <- get_locations()
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
#' @return Data frame with location codes and names for easy lookup
#' @export
#'
#' @examplesIf interactive() && curl::has_internet()
#' create_location_lookup()
create_location_lookup <- function() {
  locations <- get_locations()
  locations <- clean_ihme_columns(locations)

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

  if (length(params) == 0) {
    return(list())
  }

  filtered_params <- list()

  for (name in names(params)) {
    value <- params[[name]]
    if (!is.null(value)) {
      filtered_params[[name]] <- value
    }
  }

  return(filtered_params)
}
