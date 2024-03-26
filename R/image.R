# validate image
validate_cv_image <- function(img, null.ok = TRUE) {
  if (is.null(url) && null.ok) {
    return(TRUE)
  } else {
    img_s <- tryCatch({
      mimg <- magick::image_read(img)
    },
    error = function(e) {
      NULL
    })
    if (is.null(img_s)) {
      return(FALSE)
    }
    iimg <- magick::image_info(mimg)
    if (iimg$format %in% c("JPEG", "PNG", "GIF")) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}

get_image_format <- function(img) {
  mimg <- magick::image_read(img)
  iimg <- magick::image_info(mimg)
  tolower(iimg[["format"]])
}

normalize_cv_image <- function(img) {
  if (!is.null(img)) {
    if (file.exists(img)) {
      img
    } else if (RCurl::url.exists(img)) {
      img_format <- get_image_format(img)
      out_file <-
        file.path(tempdir(), paste0("profilepic", ".", img_format))
      utils::download.file(img, destfile = out_file, quiet = TRUE)
      out_file
    } else {
      stop("Invalid profilepic URL or file path. Please check the URL or file path.")
    }
  } else {
    NULL
  }
}
