bev <- b_env_v()
on.exit({
  r_env_v(bev)
})
Sys.setenv(ORCID_TOKEN = "_dummy_value__")

# setup vcr
library("vcr") # *Required* as vcr is set up on loading
invisible(vcr::vcr_configure(
  dir = vcr::vcr_test_path("fixtures"),
  filter_sensitive_data =
    list("<<rorcid_bearer_token>>" = Sys.getenv("ORCID_TOKEN"))
))
vcr::check_cassette_names()
