#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom dplyr bind_rows filter mutate select arrange left_join
#' @importFrom digest digest
#' @importFrom glue glue
#' @importFrom httr2 request req_url_query req_perform resp_check_status resp_body_json req_headers
#' @importFrom janitor clean_names
#' @importFrom purrr map map_dfr
#' @importFrom rlang .data !!!
#' @importFrom stringr str_detect regex
## usethis namespace: end
NULL

utils::globalVariables(
  c(
    "location_name",
    "indicator_id",
    "location_id",
    "year",
    "indicator_name",
    "age_group_id",
    "sex_id",
    "target_id",
    "goal_id",
    "scenario"
  )
)
