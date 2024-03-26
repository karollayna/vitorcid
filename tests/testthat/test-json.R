test_that("gjd works", {
  
  json_path <- system.file(package = "vitorcid", "config.json")
  out1 <- gjd(json_path = json_path, s = "supported_entries")
  out2 <- gjd(json_path = json_path)
  out3 <- gjd(json_path = json_path, names_only = TRUE)
  expect_true(is.character(out1))
  expect_true(is.list(out2))
  expect_true(is.character(out3))
})
