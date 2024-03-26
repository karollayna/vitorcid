test_that("get_personal_data works", {
  vcr::use_cassette("get_personal_data", {
    pd <- get_personal_data(orcid = "0000-0002-7059-6378")
  })
  checkmate::expect_list(pd, any.missing = FALSE, null.ok = FALSE)
  checkmate::expect_data_frame(pd$links, null.ok = FALSE)
  expect_true(all(
    c("given_names", "family_name", "summary", "links", "email") %in% names(pd)
  ))
  
})

test_that("get_cv_header works", {
  vcr::use_cassette("get_cv_header", {
    ch <- get_cv_header(orcid = "0000-0002-7059-6378")
  })
  conf_json_path <- system.file(package = "vitorcid", "config.json")
  shf <- gjd(json_path = conf_json_path, s = "supported_header_fields") 
  expect_true(all(names(ch) %in% shf))
  expect_true(file.exists(ch$profilepic))
  checkmate::expect_list(ch, any.missing = FALSE, null.ok = FALSE)
  
})

test_that("get_cv_data works", {
  vcr::use_cassette("get_cv_data", {
    tmp_bib_file <-
      system.file(package = "vitorcid",
                  "testdata",
                  "0000-0002-7059-6378_bibliography.bib")
    bib_dt <- vitae::bibliography_entries(tmp_bib_file)
    mockery::stub(
      where = get_cv_data,
      what = "get_vitae_bib_entry",
      how = bib_dt,
      depth = 3
    )
    
    cd <- get_cv_data(orcid = "0000-0002-7059-6378")
    cd2 <-
      get_cv_data(orcid = "0000-0002-7059-6378",
                  entries = c("education", "employment"))
    cd3 <-
      get_cv_data(orcid = "0000-0002-7059-6378",
                  entries = c("education", "employment", "work"))
  })
  conf_json_path <- system.file(package = "vitorcid", "config.json")
  se <- gjd(json_path = conf_json_path, s = "supported_entries") 
  expect_true(all(names(cd) %in% c("pd", se)))
    
  expect_true(all(names(cd2) %in% c("pd", se)))
  
  expect_true(all(names(cd3) %in% c("pd", se)))
  
})


test_that("get_cv_works", {
  vcr::use_cassette("get_cv", {
    tmp_bib_file <-
      system.file(package = "vitorcid",
                  "testdata",
                  "0000-0002-7059-6378_bibliography.bib")
    bib_dt <- vitae::bibliography_entries(tmp_bib_file)
    mockery::stub(
      where = get_cv_data,
      what = "get_vitae_bib_entry",
      how = bib_dt,
      depth = 3
    )
    
    out_path <- file.path(tempdir(), "0000-0002-7059-6378_cv")
    cd <-
      get_cv(orcid = "0000-0002-7059-6378",
             output_type = "Rmd",
             out_file = out_path)
  })
  rmd_fpath <- paste0(out_path, ".Rmd")
  expect_true(file.exists(rmd_fpath))
  
})