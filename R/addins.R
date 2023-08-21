#' @title Search data check functions available in sdtmchecksmeta
#'
#' @description Interactively search the check functions of the sdtmchecks package
#'
#' @details This RStudio Addin (for RStudio v0.99.878+) can be used to search 
#' and subset the sdtmchecksmeta dataframe, when using R Studio interactively. 
#' The data check functions searched all start with "check_..." 
#'
#' @importFrom DT renderDT renderDataTable DTOutput
#' @importFrom miniUI miniPage gadgetTitleBar miniTabstripPanel miniContentPanel
#' @importFrom shiny reactive paneViewer runGadget observeEvent stopApp
#' @importFrom dplyr %>% rename 
#' 
#' @export
#' 
#' @keywords internal
#'
#'


sdtmchecksSearch <- function() {
  
  # Read in sdtmchecksmeta data file from package 
  load("data/sdtmchecksmeta.RData")
  
  showvars <- c("check", "category", "domains", "pdf_title", "pdf_subtitle")
  
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
           'Domain(s)' = domains,
           'Description' = pdf_title,
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
    
    sdtmchecksdisplay <-  shiny::reactive({
      
      return(sdtmchecksmetasm)

    })#end of reactive function for sdtmchecksdisplay
    
    output$table <- DT::renderDataTable(
      sdtmchecksdisplay()
      ,filter = list(position = 'top', clear = TRUE) 
      ,escape = FALSE 
      ,options = list(searchHighlight = TRUE
                      ,lengthChange = TRUE
                      ,autoWidth = TRUE
                      ,pageLength = 10)
      )
    
  } # end of server function 
   
  # opens in 'Viewer' of RStudio
  shiny::runGadget(ui, server, 
                   viewer = shiny::paneViewer(minHeight = NULL), 
                   stopOnCancel = TRUE)
  
} # end of sdtmchecksSearch() 

# run the add-in
#sdtmchecksSearch()
