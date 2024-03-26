library("vcr") # *Required* as vcr is set up on loading
invisible(vcr::vcr_configure(
  dir = vcr::vcr_test_path("fixtures"),
  filter_sensitive_data =
    list("<<rorcid_bearer_token>>" = Sys.getenv('ORCID_TOKEN'))
))
vcr::check_cassette_names()
