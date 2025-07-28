#' Check if cached data is current
#'
#' @param file Path to cached data file
#' @return List with cached data if current, NULL if needs refresh
check_cached_data <- function(file) {
  if (!file.exists(file)) {
    return(NULL)
  }

  cached_data <- readRDS(file)
  current_hash <- get_version_hash()

  if (!is.null(cached_data$version_hash) && cached_data$version_hash == current_hash) {
    download_date <- if (!is.null(cached_data$downloaded)) {
      format(cached_data$downloaded, "%Y-%m-%d %H:%M")
    } else {
      "unknown"
    }
    message(glue::glue("{file} already exists and is the latest release (downloaded: {download_date}), use download_all_ihme_metadata(force=TRUE) to force download even if file exists and is current"))
    return(cached_data)
  }

  message("Data version changed - downloading fresh data")
  return(NULL)
}

#' Version checking for caching
#' @export
get_version_hash <- function() {
  tryCatch({
    location_count <- nrow(get_locations())
    indicator_count <- nrow(get_indicators())
    target_count <- nrow(get_targets())

    metadata <- list(
      location_count = location_count,
      indicator_count = indicator_count,
      target_count = target_count,
      date = Sys.Date()
    )
    digest::digest(metadata, algo = "md5")
  }, error = function(e) {
    warning("Could not generate version hash, using date: ", e$message)
    digest::digest(Sys.Date(), algo = "md5")
  })
}
