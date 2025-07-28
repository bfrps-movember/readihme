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
  # Check for API key
  api_key <- get_ihme_key(error_if_missing = TRUE)

  # Create the base request
  req <- httr2::request(paste0(get_base_url(), api_url))

  # Add authorization header
  req <- httr2::req_headers(req, Authorization = api_key)

  # Add query parameters if provided
  params <- list(...)
  if (length(params) > 0) {
    # Remove NULL parameters
    params <- params[!sapply(params, is.null)]

    # Only add query parameters if we have any non-NULL ones
    if (length(params) > 0) {
      req <- httr2::req_url_query(req, !!!params)
    }
  }

  # Perform the request with enhanced error handling
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
      stop(paste("API request failed:", e$message), call. = FALSE)
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
