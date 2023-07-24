# Run covr package report 
# Save html file and metadata about report run
# Run Shiny app based on html 


options(covr.record_tests = TRUE) 

# create HTML report of unit test coverage from {covr}
html_rpt_name <- "sdtmchecks_covr_report.html"
x <- covr::package_coverage()
html_loc_rpt <- here::here("inst", "dev", html_rpt_name)

# save metadata - create as a markdown file 
html_rpt_run_meta <- data.frame(
  pkg = "sdtmchecks",
  vers = packageVersion("sdtmchecks"),
  when = Sys.time(),
  vers_covr = packageVersion("covr")
)


covr::report(
  x = x
  ,file = html_loc_rpt
  ,browse = interactive()
)

# save metadata from report run as html_rpt_meta.RData in sdtmchecks/inst/dev/lib
save(html_rpt_run_meta, file = here::here("inst", "dev", "lib", "html_rpt_run_meta.RData"), version=2)
     

# report as Shiny App based on report
library(shiny) 

meta <- html_rpt_run_meta

ui <- fluidPage(
  htmlOutput("covr_rpt"), 
  div(print(paste0("Report from {covr} ", meta$covr, 
                   " created ", meta$when,  
                   " for {sdtmchecks} ", meta$vers))
      )
)

addResourcePath("library", here::here("inst", "dev", "lib"))
addResourcePath("rpt", here::here("inst", "dev"))

server <- function(input,output){
  output$covr_rpt <- renderUI({
    tags$iframe(seamless = "seamless",
                src= paste0("rpt/", html_rpt_name), 
                width=1200, height=800)
  })
}

shinyApp(ui, server)
