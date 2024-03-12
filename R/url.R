# validate URL
validate_url <- function(url, null.ok = TRUE, local.ok = TRUE) {
  if (is.null(url) && null.ok) {
    return(TRUE)
  } else if (file.exists(url)) {
    return(TRUE)
  } else {
    RCurl::url.exists(url)
  }
}

# normalize URL
normalize_url <- function(url,
                          null.ok = TRUE,
                          local.ok = TRUE) {
  f_url <- if (is.null(url) && null.ok) {
    url
  } else {
    gsub("https*://", "", url)
  }
  f_url
}

