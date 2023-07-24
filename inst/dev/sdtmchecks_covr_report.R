# Run covr package report 
# Save html file and metadata about report run
# Run Shiny app based on html 

options(covr.record_tests = TRUE) 

# create HTML report of unit test coverage from {covr}
html_rpt_name <- "sdtmchecks_covr_report.html"

html_loc_rpt <- here::here("inst", "dev", html_rpt_name)

# save metadata - create as a markdown file 
html_rpt_run_meta <- data.frame(
  pkg = "sdtmchecks",
  vers = packageVersion("sdtmchecks"),
  when = Sys.time(),
  vers_covr = packageVersion("covr")
)



x <- covr::package_coverage()
covr::report(
  x = x
  ,file = html_loc_rpt
  ,browse = interactive()
)

# save metadata from report run as html_rpt_meta.RData in sdtmchecks/inst/dev/lib
save(html_rpt_run_meta, file = here::here("inst", "dev", "html_rpt_run_meta.RData"), version=2)