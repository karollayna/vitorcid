test_that("get_vitae_entry works", {
  vcr::use_cassette("get_vitae_entry", {
    tmp_bib_file <-
      system.file(package = "vitorcid",
                  "testdata",
                  "0000-0002-7059-6378_bibliography.bib")
    bib_dt <- vitae::bibliography_entries(tmp_bib_file)
    mockery::stub(
      where = get_vitae_entry,
      what = "get_vitae_bib_entry",
      how = bib_dt,
      depth = 3
    )
    vee <- get_vitae_entry(entry_type = "education", orcid = "0000-0002-7059-6378")
    # disabled until https://github.com/r-hub/pkgsearch/issues/111 is fixed
    # ver <- get_vitae_entry(entry_type = "r_package", orcid = "0000-0002-7059-6378") # nolint
    vec <- get_vitae_entry(entry_type = "citation", orcid = "0000-0002-7059-6378")
  })
  expect_true(checkmate::check_class(vee, "vitae_detailed"))
  # disabled until https://github.com/r-hub/pkgsearch/issues/111 is fixed
  # expect_true(checkmate::check_class(ver, "vitae_detailed")) # nolint
  expect_true(checkmate::check_class(vec, "vitae_bibliography"))
  
})
