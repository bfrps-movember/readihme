#' Set IHME API Key
#'
#' Set your IHME API authentication key for the current R session.
#' You can get your API key from: https://api.healthdata.org/sdg/v1/
#'
#' @param key Your IHME API key (character string)
#' @param install Whether to install the key for use across sessions (default: FALSE)
#'
#' @return Invisible TRUE if successful
#' @export
#'
#' @examples
#' \dontrun{
#' # Set key for current session only
#' set_ihme_key("your_api_key_here")
#'
#' # Set key and install for future sessions
#' set_ihme_key("your_api_key_here", install = TRUE)
#' }
set_ihme_key <- function(key, install = FALSE) {
  if (missing(key) || is.null(key) || nchar(key) == 0) {
    stop("Please provide a valid IHME API key. Get yours at: https://api.healthdata.org/sdg/v1/")
  }

  # Validate key format (basic check)
  if (!is.character(key) || nchar(key) < 10) {
    stop("Invalid API key format. Please check your key and try again.")
  }

  # Set for current session
  Sys.setenv(IHME_API_KEY = key)

  if (install) {
    # Install key for future sessions
    home <- Sys.getenv("HOME")
    renv <- file.path(home, ".Renviron")

    if (file.exists(renv)) {
      # Read existing .Renviron
      lines <- readLines(renv)
      # Remove any existing IHME_API_KEY lines
      lines <- lines[!grepl("^IHME_API_KEY=", lines)]
    } else {
      lines <- character(0)
    }

    # Add the new key
    lines <- c(lines, paste0("IHME_API_KEY=", key))
    writeLines(lines, renv)

    message("IHME API key installed for future sessions. Restart R to use.")
  }

  message("IHME API key set successfully for this session.")
  invisible(TRUE)
}

#' Get IHME API Key
#'
#' Retrieve the currently set IHME API key from environment variables.
#'
#' @param error_if_missing Whether to throw an error if no key is found (default: TRUE)
#'
#' @return Character string with the API key, or NULL if not found and error_if_missing is FALSE
#' @export
#'
#' @examples
#' \dontrun{
#' # Get key (will error if not set)
#' key <- get_ihme_key()
#'
#' # Get key without error
#' key <- get_ihme_key(error_if_missing = FALSE)
#' }
get_ihme_key <- function(error_if_missing = TRUE) {
  key <- Sys.getenv("IHME_API_KEY")

  if (nchar(key) == 0) {
    if (error_if_missing) {
      stop(
        "IHME API key not found. Please set it using set_ihme_key().\n",
        "Get your API key at: https://api.healthdata.org/sdg/v1/",
        call. = FALSE
      )
    } else {
      return(NULL)
    }
  }

  return(key)
}

#' Check if IHME API Key is Set
#'
#' Check whether an IHME API key is currently available.
#'
#' @return Logical indicating whether a key is set
#' @export
#'
#' @examples
#' \dontrun{
#' if (has_ihme_key()) {
#'   # Proceed with API calls
#'   locations <- get_locations()
#' } else {
#'   message("Please set your IHME API key first")
#' }
#' }
has_ihme_key <- function() {
  key <- Sys.getenv("IHME_API_KEY")
  return(nchar(key) > 0)
}

#' Validate IHME API Key
#'
#' Test whether the current API key is valid by making a simple API call.
#'
#' @return Logical indicating whether the key is valid
#' @export
#'
#' @examples
#' \dontrun{
#' # Test if your key works
#' if (validate_ihme_key()) {
#'   message("API key is valid!")
#' } else {
#'   message("API key is invalid or expired")
#' }
#' }
validate_ihme_key <- function() {
  if (!has_ihme_key()) {
    message("No API key found. Use set_ihme_key() to set one.")
    return(FALSE)
  }

  tryCatch({
    # Try a simple API call
    call_ihme_api("api_version")
    return(TRUE)
  }, error = function(e) {
    if (grepl("401|403|Unauthorized|Forbidden", e$message)) {
      message("API key is invalid or expired. Please check your key.")
      return(FALSE)
    } else {
      # Some other error (network, etc.)
      warning("Could not validate API key due to: ", e$message)
      return(FALSE)
    }
  })
}

#' Remove IHME API Key
#'
#' Remove the IHME API key from the current session and optionally from .Renviron.
#'
#' @param uninstall Whether to also remove from .Renviron file (default: FALSE)
#'
#' @return Invisible TRUE
#' @export
#'
#' @examples
#' \dontrun{
#' # Remove from current session only
#' remove_ihme_key()
#'
#' # Remove from current session and .Renviron
#' remove_ihme_key(uninstall = TRUE)
#' }
remove_ihme_key <- function(uninstall = FALSE) {
  # Remove from current session
  Sys.unsetenv("IHME_API_KEY")

  if (uninstall) {
    # Remove from .Renviron
    home <- Sys.getenv("HOME")
    renv <- file.path(home, ".Renviron")

    if (file.exists(renv)) {
      lines <- readLines(renv)
      lines <- lines[!grepl("^IHME_API_KEY=", lines)]
      writeLines(lines, renv)
      message("IHME API key removed from .Renviron. Restart R for changes to take effect.")
    }
  }

  message("IHME API key removed from current session.")
  invisible(TRUE)
}
