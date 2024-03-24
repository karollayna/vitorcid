#' get CV
#'
#' get CV as Rmd and/or PDF file with data from ORCID
#'
#' @param out_file string output file
#' @param orcid string with the ORCID ID
#'  by default fetched fomr "ORCID_ID" system variable
#' @param output_type string with output format ('Rmd' or 'pdf')
#' @param json_path string to JSON file with additional/custom data
#' @param entries character vector with entries to be included in the CV
#'
#' @return NULL
#' @export
#' @keywords vitae
#'
get_cv <-
  function(out_file = file.path(getwd(), "CV"),
           orcid = Sys.getenv("ORCID_ID"),
           output_type = c("Rmd", "pdf"),
           json_path = NULL,
           entries = c("education", "employment", "r_package", "citation")) {
    cvl <- get_cv_data(orcid = orcid, json_path = json_path)
    header_yaml <-
      get_cv_header(orcid = orcid,
                    output_type = "yaml",
                    json_path = json_path)
    rmd_tmpfile <- tempfile(fileext = ".Rmd")
    write("---", rmd_tmpfile)
    write(header_yaml, rmd_tmpfile, append = TRUE)
    write("---", rmd_tmpfile, append = TRUE)

    json_path_str <- if (is.null(json_path)) {
      "NULL"
    } else {
      sprintf("'%s'", json_path)
    }
    rmd_str <-
      sprintf("```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)
library(vitorcid)
cvl <- get_cv_data(orcid = '%s', json_path = %s)
```", orcid, json_path_str)

    if (!is.null(cvl$pd$summary)) {
      rmd_str <- c(
        rmd_str,
        "# Professional Summary
```{r echo=FALSE, comment=''}
ps_str <- strwrap(cvl$pd$summary, 80)
cat(ps_str, sep = '\n')
```"
      )
    }

    for (entry in setdiff(names(cvl), "pd")) {
      if (entry %in% entries) {
        rmd_str <- c(
          rmd_str,
          sprintf(
            "# %s
```{r}
cvl[[\"%s\"]]
```
",
            stringr::str_to_title(entry),
            entry
          )
        )
      }
    }
    write(rmd_str, rmd_tmpfile, append = TRUE)

    if (any(output_type %in% "Rmd")) {
      file.copy(rmd_tmpfile, paste0(out_file, ".Rmd"), overwrite = TRUE)
    }

    if (any(output_type %in% "pdf")) {
      rmarkdown::render(input = rmd_tmpfile,
                        output_file = paste0(out_file, ".pdf"))
    }
  }

#' get CV header
#'
#' get CV header as YAML or list with data from ORCID
#'
#' @param orcid string with the ORCID ID
#'  by default fetched fomr "ORCID_ID" system variable
#' @param json_path string to JSON file with additional/custom data
#' @param output_type string with output format ('list' or 'yaml'
#'
#' return list or YAML with header data
#' @export
#' @keywords vitae
#'
get_cv_header <- function(orcid = Sys.getenv("ORCID_ID"),
                          json_path = NULL,
                          output_type = c("list", "yaml")) {
  
  output_type <- match.arg(output_type)
  shf <- gjd("supported_header_fields",
             json_path = system.file(package = "vitorcid", "config.json"))

  huv <- gjd("header_urls_to_validate",
             json_path = system.file(package = "vitorcid", "config.json"))

  pd <- get_personal_data(orcid)
  links_l <-
    as.list(structure(pd$links$url.value, names = pd$links$`url-name`))
 
  # get header data from JSON file (if present) 
  hd_l <-
    if (!is.null(json_path) &&
        "header_data" %in% get_json_names(json_path)) {
      out_l <- gjd("header_data", json_path = json_path)
      if (!all(names(out_l) %in% shf)) {
        stop(
          sprintf(
            "Some entries in 'header_data' (from JSON file) are not supported: '%s'. Select on from: '%s'",
            toString(setdiff(names(out_l), shf)),
            toString(shf)
          )
        )
      }
      out_l
    } else {
      list()
    }
  
  # mergee header data from JSON file with data from ORCID 
  # (with higher priority given to values from JSON file)
  links_l <- utils::modifyList(links_l, hd_l)
  
  for (entry in huv) {
    if (entry %in% names(links_l)) {
      if (!validate_url(links_l[[entry]])) {
        stop(sprintf("Invalid URL/path for %s: %s", entry, links_l[[entry]]))
      }
    }
  }

  hfs <- gjd("header_fields_to_shorten",
             json_path = system.file(package = "vitorcid", "config.json"))
  for (entry in hfs) {
    if (entry %in% names(links_l)) {
      links_l[[entry]] <- basename(links_l[[entry]])
    }
  }
  links_l[["profilepic"]] <-
    normalize_cv_image(links_l[["profilepic"]])
  links_l[["www"]] <- normalize_url(links_l[["www"]])

  out_l <- pd
  out_l[c("links", "summary", "given_names", "family_name")] <- NULL

  out_l[["orcid"]] <- orcid
  out_l[["email"]] <- pd$email
  out_l[["name"]] <- get_full_name(pd)
  out_l[["date"]] <- format(Sys.time(), "%B %Y")
  out_l[["headcolor"]] <- "009ACD"
  out_l[["output"]] <- "vitae::awesomecv"

  out_l <- c(out_l, links_l)

  stopifnot(names(out_l) %in% shf)
  if (output_type == "list") {
    out_l
  } else if (output_type == "yaml") {
    yaml::as.yaml(out_l)
  } else {
    stop("invalid output type")
  }
}

#' get CV data from ORCID
#'
#' get CV data from ORCID
#'
#' @param orcid string with the ORCID ID
#'  by default fetched fomr "ORCID_ID" system variable
#' @param json_path string to JSON file with additional/custom data
#' @param entries character vector with entries to be included in the CV
#'
#' @return list with CV data
#' @export
#' @keywords vitae
#'
get_cv_data <-
  function(orcid = Sys.getenv("ORCID_ID"),
           json_path = NULL,
           entries = c("education", "employment", "r_package", "citation")) {
    ml <- lapply(entries, function(entry) {
      get_vitae_entry(entry, orcid, json_path = json_path)
    })
    names(ml) <- entries
    ml$pd <- get_personal_data(orcid)

    # TODO: add validators

    ml
  }


#' extract data
#'
#' extract date from ORCID section (e.g. employment or education summary)
#'
#' @param tbl summary table fetched from ORCID
#' @param tag string with the summary tag
#'
#' @return character vector with start date - end date entris for each employment or education record
#'
extract_date <- function(tbl, tag = "employment-summary") {
  sy <- paste0(tag, ".start-date.year.value")
  sm <- paste0(tag, ".start-date.month.value")
  ey <- paste0(tag, ".end-date.year.value")
  em <- paste0(tag, ".end-date.month.value")

  v_out <- vapply(seq(nrow(tbl)), function(idx) {
    out <- if (is.na(tbl[[idx, ey]])) {
      sprintf("%s/%s - present", tbl[[idx, sy]], tbl[[idx, sm]])
    } else {
      sprintf("%s/%s - %s/%s", tbl[[idx, sy]], tbl[[idx, sm]], tbl[[idx, ey]], tbl[[idx, em]])
    }
    out
  }, character(1))
  v_out
}

#' get personal data from ORCID
#'
#' get personal data from ORCID
#'
#' @param orcid string with the ORCID ID
#'  by default fetched fomr "ORCID_ID" system variable
#' @export
#' @return list with personal data
#'
get_personal_data <- function(orcid = Sys.getenv("ORCID_ID")) {
  pd <- rorcid::orcid_person(orcid)[[orcid]]

  list(
    given_names = pd[["name"]][["given-names"]][["value"]],
    family_name = pd[["name"]][["family-name"]][["value"]],
    summary = pd[["biography"]][["content"]],
    links = pd[["researcher-urls"]][["researcher-url"]],
    email = pd[["emails"]][["email"]][1, "email"]
  )
}

#' @keywords .internal
get_full_name <- function(pd) {
  # sanitize
  pd$family_name <- gsub("\\s+", " ", pd$family_name)
  pd$family_name <- gsub("^\\s|\\s$", "", pd$family_name)
  pd$given_names <- gsub("\\s+", " ", pd$given_names)
  pd$given_names <- gsub("^\\s|\\s$", "", pd$given_names)
  sprintf("%s %s", pd$given_names, pd$family_name)
}
