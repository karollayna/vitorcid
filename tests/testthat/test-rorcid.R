test_that("get_template_settings works", {
  out_l <-
    get_template_settings("vitae::awesomecv", template_args = list(show_footer = TRUE))
  checkmate::expect_list(out_l)
  out_s <- get_template_settings("vitae::awesomecv")
  checkmate::expect_string(out_s)
  expect_error(
    get_template_settings("vitae::awesomecv", template_args = list(a = 2)),
    "The parameters a are not valid"
  )
  expect_error(get_template_settings("unknown::template"),
               "Assertion on 'template' failed")
})
