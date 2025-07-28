#' Call the IHME SDG API
#'
#' @param api_url url endpoint
#' @param ... additional query parameters
#'
#' @return JSON response from the API, usually parsed to a list
#' @export
#'
#' @examplesIf interactive() && curl::has_internet()
#' call_ihme_api("GetLocation")
call_ihme_api <- function(api_url, ...) {
  api_key <- get_ihme_key(error_if_missing = TRUE)
  base_url <- paste0(get_base_url(), api_url)

  params <- list(...)
  if (length(params) > 0) {
    params <- params[!sapply(params, is.null)]

    if (length(params) > 0) {
      query_parts <- character(0)

      for (param_name in names(params)) {
        param_value <- params[[param_name]]

        if (length(param_value) > 1) {
          # Handle array parameters - IHME API expects repeated parameter names
          for (value in param_value) {
            query_parts <- c(query_parts, paste0(param_name, "=", utils::URLencode(as.character(value))))
          }
        } else if (length(param_value) == 1) {
          query_parts <- c(query_parts, paste0(param_name, "=", utils::URLencode(as.character(param_value))))
        }
      }

      if (length(query_parts) > 0) {
        query_string <- paste(query_parts, collapse = "&")
        full_url <- paste0(base_url, "?", query_string)
      } else {
        full_url <- base_url
      }
    } else {
      full_url <- base_url
    }
  } else {
    full_url <- base_url
  }

  req <- httr2::request(full_url)
  req <- httr2::req_headers(req, Authorization = api_key)

  tryCatch({
    resp <- httr2::req_perform(req)
    httr2::resp_check_status(resp)
    httr2::resp_body_json(resp)
  }, error = function(e) {
    if (grepl("401|403", e$message)) {
      stop("Authentication failed. Please check your API key with validate_ihme_key()", call. = FALSE)
    } else if (grepl("429", e$message)) {
      stop("Rate limit exceeded. Please wait before making more requests.", call. = FALSE)
    } else if (grepl("500|502|503", e$message)) {
      stop("IHME server error. Please try again later.", call. = FALSE)
    } else {
      stop(e$message, call. = FALSE)
    }
  })
}

get_base_url <- function() {
  "https://api.healthdata.org/sdg/v1/"
}

#' Get API Version
#'
#' @return API version information
#' @export
#'
#' @examplesIf interactive() && curl::has_internet()
#' get_api_version()
get_api_version <- function() {
  call_ihme_api("api_version")
}
