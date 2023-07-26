#' Search data check functions available in sdtmchecksmeta
#'
#' Interactively search the check functions of the sdtmchecks package
#'
#' This addin can be used to interactively subset the dataframe
#'
#'
#' @importFrom DT renderDT renderDataTable
#' @importFrom miniUI miniPage gadgetTitleBar miniTabstripPanel miniTabPanel miniContentPanel
#' @importFrom rstudioapi getActiveDocumentContext
#' @importFrom shiny reactive dialogViewer runGadget icon
#' @importFrom dplyr %>% rename 
#' 
#' @export
#'
#'


library(dplyr)

usethis::use_package("miniUI")
usethis::use_package("rstudioapi")
usethis::use_package("shiny")


sdtmchecksSearch <- function() {
  
  # Get the document context.
  #context <- rstudioapi::getActiveDocumentContext()
  
  # Read in sdtmchecksmeta data file from package 
  
  load("data/sdtmchecksmeta.RData")
  
  showvars <- c("check", "category", "priority", "pdf_title", "xls_title", "domains", "pdf_subtitle")
  
  sdtmchecksmetasm <- sdtmchecksmeta[showvars]
  
  #match UI term, cosmetic adjustments
  sdtmchecksmetasm$category <- gsub("ALL", "Cross TA", sdtmchecksmetasm$category)
  sdtmchecksmetasm$category <- gsub("ONC", "Onc", sdtmchecksmetasm$category)
  sdtmchecksmetasm$category <- gsub("COVID", "Covid-19", sdtmchecksmetasm$category)
  sdtmchecksmetasm$category <- gsub("OPHTH", "Ophthalmology", sdtmchecksmetasm$category)
  
  sdtmchecksmetasm$domains <- toupper(sdtmchecksmetasm$domains)
  
  #update table column names
  sdtmchecksmetasm <- sdtmchecksmetasm %>%
    rename('Check name' = check,
           'Check category' = category,
           'Check priority' = priority,
           'Description (PDF title)' = pdf_title,
           'Excel report tab' = xls_title,
           'SDTMv domains' = domains,
           'Details' = pdf_subtitle
    )
  
  
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Search sdtmchecks"),
    miniUI::miniTabstripPanel(
      miniUI::miniTabPanel("Data", icon = shiny::icon("table"), 
                           miniUI::miniContentPanel(
                             DT::DTOutput("table", height = "auto")
                           )
      )
    )
  ) # end of ui function  
  
  
  
  server <- function(input, output, session) {
    
    checkdata <- sdtmchecksmetasm 
    
    sdtmchecksdisplay <-  shiny::reactive({
      
      return(sdtmchecksmetasm)
      
      req(input$priority)
      
      #subset on priority
      prioritygrp <- input$priority
      
      sdtmchecksmetasm <- sdtmchecksmetasm[(sdtmchecksmetasm$priority) %in% (prioritygrp),]
      
      #subset on category
      categorygrp <- input$category
      
      sdtmchecksmetasm <- sdtmchecksmetasm[(sdtmchecksmetasm$category) %in% (categorygrp),]
      
    })#end of reactive function for sdtmchecksdisplay
    
    output$table <- DT::renderDataTable(
      sdtmchecksdisplay(), options = list(lengthChange = TRUE)
    )
  } # end of server function 
  
  shiny::runGadget(ui, server, viewer = shiny::dialogViewer("Data"))  
  #shiny::runGadget(ui, server, viewer = shiny::dialogViewer("Data"), stopOnCancel = TRUE)
  
} # end of sdtmchecksSearch() 



# run the add-in
#sdtmchecksSearch()
