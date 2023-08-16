#' Search data check functions available in sdtmchecksmeta
#'
#' Interactively search the check functions of the sdtmchecks package
#'
#' This addin can be used to interactively subset the dataframe
#'
#'
#' @importFrom DT renderDT renderDataTable
#' @importFrom miniUI miniPage gadgetTitleBar miniTabstripPanel miniContentPanel
#' @importFrom rstudioapi getActiveDocumentContext
#' @importFrom shiny reactive paneViewer runGadget icon observeEvent stopapp
#' @importFrom dplyr %>% rename 
#' 
#' @export
#'
#'


usethis::use_package("miniUI")
usethis::use_package("rstudioapi")
usethis::use_package("shiny")


sdtmchecksSearch <- function() {
  
  #library(dplyr)
  
  # Get the document context.
  #context <- rstudioapi::getActiveDocumentContext()
  
  # Read in sdtmchecksmeta data file from package 
  load("data/sdtmchecksmeta.RData")
  
  showvars <- c("check", "category", "pdf_title", "domains", "pdf_subtitle")
  
  sdtmchecksmetasm <- sdtmchecksmeta[showvars]
  
  #match UI term, cosmetic adjustments
  sdtmchecksmetasm$category <- gsub("ALL", "General", sdtmchecksmetasm$category)
  sdtmchecksmetasm$category <- gsub("ONC", "Oncology", sdtmchecksmetasm$category)
  sdtmchecksmetasm$category <- gsub("COVID", "Covid-19", sdtmchecksmetasm$category)
  sdtmchecksmetasm$category <- gsub("OPHTH", "Ophthalmology", sdtmchecksmetasm$category)
  
  sdtmchecksmetasm$domains <- toupper(sdtmchecksmetasm$domains)
  
  #update table column names
  sdtmchecksmetasm <- sdtmchecksmetasm %>%
    rename('Check function name' = check,
           'Check category' = category, 
           'Description' = pdf_title,
           'Domains' = domains,
           'Details' = pdf_subtitle
    )
  
  #============= UI ==============
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("sdtmchecks - data check function viewer",
                           left = NULL),
    miniUI::miniContentPanel(padding = 0,
                     miniUI::miniContentPanel(DT::DTOutput("table", width = "100%", height = "40em"))
    )
  )
 #end of ui function
  
  #============= SERVER ==============
  
  server <- function(input, output, session) { 
    
    # when "Done" button is clicked
    shiny::observeEvent(input$done, {
      invisible(shiny::stopApp())
    })
    
    checkdata <- sdtmchecksmetasm 
    
    sdtmchecksdisplay <-  shiny::reactive({
      
      return(sdtmchecksmetasm)

    })#end of reactive function for sdtmchecksdisplay
    
    output$table <- DT::renderDataTable(
      sdtmchecksdisplay(), options = list(lengthChange = TRUE, 
                                          autoWidth = TRUE, 
                                          pageLength = 10)
      )
    
  } # end of server function 
   
  # opens in 'Viewer' of RStudio
  shiny::runGadget(ui, server, 
                   viewer = shiny::paneViewer(minHeight = NULL), 
                   stopOnCancel = TRUE)
  
} # end of sdtmchecksSearch() 

# run the add-in
#sdtmchecksSearch()



