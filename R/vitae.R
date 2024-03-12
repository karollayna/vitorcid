#' get vitae entry with data from ORCID or CRAN/BioC
#'
#' get vitae entry with data from ORCID or CRAN/BioC
#'
#' @param entry_type string with the type of entry to be fetched
#' @param orcid string with the ORCID ID
#' by default fetched fomr "ORCID_ID" system variable
#' @param why list with the data for the 'why' vitae column
#' @param json_path string with the path to the JSON file
#'
#' @export
#' @return vitae entry (standardized data.table)
#'
get_vitae_entry <- function(entry_type,
                            orcid = Sys.getenv("ORCID_ID"),
                            why = NULL,
                            json_path = NULL) {
  checkmate::assert_string(orcid)
  checkmate::assert_choice(entry_type, get_supported_entries())
  if (entry_type == "citation") {
    get_vitae_bib_entry(orcid)
  } else if (entry_type == "r_package") {
    get_vitae_r_package_entry(orcid)
  } else {
    get_vitae_detailed_entry(entry_type, orcid, why, json_path = json_path)
  }
}

#' @inheritParams get_vitae_entry
#' @keywords .internal
get_vitae_detailed_entry <- function(entry_type,
                                     orcid = Sys.getenv("ORCID_ID"),
                                     why = NULL,
                                     json_path = NULL) {

  # define fields
  f_a_city <-
    paste0(entry_type, "-summary.organization.address.city")
  f_a_country <-
    paste0(entry_type, "-summary.organization.address.country")
  f_o_name <- paste0(entry_type, "-summary.organization.name")
  f_r_title <- paste0(entry_type, "-summary.role-title")
  f_p_code <- paste0(entry_type, "-summary.put-code")
  f_s <- paste0(entry_type, "-summary")

  # define function name from rorcid package
  my_f <-
    get(paste0("orcid_", entry_type, "s"), asNamespace("rorcid"))

  # get data from orcid
  e_out <- my_f(orcid = orcid)

  # empty dt
  e_dt <- if (is.null(e_out[[1]]$`affiliation-group`$summaries)) {
    data.table::data.table(
      what = "No data available",
      with = "No data available",
      where = "No data available",
      when = "No data available",
      why = list()
    )
  } else {
    # get data.table with 'what', 'with', 'where', 'when', 'why' columns
    e_dt <-
      data.table::rbindlist(e_out[[1]]$`affiliation-group`$summaries, fill = TRUE)
    e_dt$what <- e_dt[[f_o_name]]
    e_dt[["with"]] <- e_dt[[f_r_title]]
    e_dt$where <- sprintf("%s, %s", e_dt[[f_a_city]], e_dt[[f_a_country]])
    e_dt$when <-
      extract_date(e_dt, tag = paste0(entry_type, "-summary"))
    e_dt <- add_why(e_dt, entry_type, why, json_path = json_path)
  }

  # get vitae_detailed entry from data.table
  vitae::detailed_entries(
    e_dt,
    what = e_dt$what,
    with = e_dt$with,
    where = e_dt$where,
    when = e_dt$when,
    why = e_dt$why
  )
}

#' @inheritParams get_vitae_entry
#' @keywords .internal
get_vitae_bib_entry <- function(orcid = Sys.getenv("ORCID_ID")) {
  ref_tbl <- rorcid::orcid_citations(orcid, cr_format = "bibtex")
  tmp_bib_file <- file.path(tempdir(), "bibliography.bib")
  write(ref_tbl$citation, tmp_bib_file)
  vitae::bibliography_entries(tmp_bib_file)
}

#' @inheritParams get_vitae_entry
#' @keywords .internal
get_vitae_r_package_entry <-
  function(orcid = Sys.getenv("ORCID_ID")) {
    pd <- get_personal_data(orcid)

    # sanitize
    pd$family_name <- gsub("\\s+", " ", pd$family_name)
    pd$family_name <- gsub("^\\s|\\s$", "", pd$family_name)
    pd$given_names <- gsub("\\s+", " ", pd$given_names)
    pd$given_names <- gsub("^\\s|\\s$", "", pd$given_names)

    # empty dt
    e_dt <-
      data.table::data.table(
        what = character(),
        with = character(),
        where = character(),
        when = character(),
        why = list()
      )

    # search Biocounductor
    fname <- paste0(pd$given_names, " ", pd$family_name)
    bl <- suppressMessages(BiocPkgTools::biocPkgList())
    m_idx <-
      union(grep(fname, bl$Author), grep(fname, bl$Maintainer))
    mp <- bl[m_idx, ]
    mp$where <-
      ifelse(
        grepl(fname, mp$Maintainer),
        "Author & maintainer (Bioconductor)",
        "Author (Bioconductor)"
      )
    bcp <- if (nrow(mp)) {
      data.table::data.table(
        what = mp$Title,
        with = mp$Package,
        where = mp$where,
        when = mp$Version,
        why = mp$Description
      )
    } else {
      e_dt
    }

    cp <-
      pkgsearch::advanced_search(
        Author = pd$given_names,
        Author = pd$family_name,
        size = 10 ^ 3
      )
    ccre <-
      pkgsearch::advanced_search(
        Maintainer = pd$given_names,
        Maintainer = pd$family_name,
        size = 10 ^ 3
      )
    cp$cre <- cp$package %in% ccre$package
    cp$where <-
      ifelse(cp$cre, "Author & maintainer (CRAN)", "Author (CRAN)")

    ccp <- if (nrow(cp)) {
      data.table::data.table(
        what = cp$title,
        with = cp$package,
        where = cp$where,
        when = as.character(cp$version),
        why = cp$description
      )
    } else {
      e_dt
    }
    pp <- rbind(bcp, ccp)

    vitae::detailed_entries(
      pp,
      what = pp$what,
      with = pp$with,
      where = pp$where,
      when = pp$when,
      why = pp$why
    )
  }

#' @inheritParams get_vitae_entry
#' @keywords .internal
add_why <- function(e_dt, entry_type, why, json_path = NULL) {
  if (is.null(why) &&
      !(is.null(json_path)) &&
      entry_type %in% get_json_names(json_path = json_path)) {
    e_why <- gjd(entry_type)
    checkmate::assert_data_table(e_why)
    checkmate::assert_true("why" %in% names(e_why))
    checkmate::assert_true(any(c("put-code", "idx") %in% names(e_why)))
    checkmate::assert_true(nrow(e_why) == nrow(e_dt))

    e_dt$idx <-  seq(nrow(e_dt))
    if ("put-code" %in% names(e_why) &&
        !("idx" %in% names(e_why))) {
      e_why$idx <-
        match(e_why[["put-code"]], e_dt[[paste0(entry_type, "-summary.put-code")]])
      checkmate::assert_true(!all(is.na(e_why$idx)))
    }
    e_dt$why <- e_why$why[e_why$idx]
  } else {
    e_dt$why <- why
  }

  if (!is.null(e_dt$why)) {
    e_dt$why <- strsplit(e_dt$why, "__")
  }
  e_dt
}
