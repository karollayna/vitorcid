#' get JSON data
#'
#' get data from json file, either subset of full data
#'
#' @param s string with the name of the first-level JSON entry to be fetched
#' if set to NULL (default) data from full JSON entry is returned
#' @param names_only logical, if TRUE only names
#'        of the first-level JSON entries are returned
#' @param json_path string with the path to the JSON file
#'
#' @return R object returned from `jsonlite::fromJSON`
#' @keywords JSON
#' @export

gjd <-
  function(s = NULL,
           names_only = FALSE,
           json_path = NULL) {
    checkmate::assert_character(s, null.ok = TRUE)
    checkmate::assert_character(json_path, null.ok = TRUE)
    if (!is.null(json_path)) {
      checkmate::assert_file_exists(json_path)
    }

    cache_l <- jsonlite::fromJSON(json_path)

    res <- if (names_only) {
      names(cache_l)
    } else {
      out <- if (!is.null(s)) {
        checkmate::assert_subset(s, names(cache_l))
        cache_l[[s]]
      } else {
        cache_l
      }
      if (inherits(out, "data.frame")) {
        data.table::as.data.table(out)
      } else {
        out
      }
    }
    res
  }

#' @keywords .internal
get_supported_entries <-
  function() {
    gjd("supported_entries",
        json_path = system.file(package = "vitorcid", "config.json"))
  }

#' @keywords .internal
get_json_names <-
  function(json_path = system.file(package = "vitorcid", "config.json")) {
    checkmate::assert_file_exists(json_path)
    gjd(names_only = TRUE,
        json_path = json_path)
  }
