# Module UI
  
#' @title   mod_release_notes_ui and mod_release_notes_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_release_notes
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
#' @importFrom shinyBS bsCollapse bsCollapsePanel
mod_release_notes_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    shinyBS::bsCollapse(id = "collapseFormerReleases",
                        open = "Current release",
                        multiple = TRUE,
                        shinyBS::bsCollapsePanel("Current release", 
                                                 insert_md_ui(ns("versionNotes_MD")), style = "info"),
                        shinyBS::bsCollapsePanel("Former releases", 
                                                 insert_md_ui(ns("formerReleases_MD")), style = "info")
    )
  )
}
    
# Module Server
    
#' @rdname mod_release_notes
#' @export
#' @keywords internal
    
mod_release_notes_server <- function(id){
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    insert_md_server("versionNotes_MD", URL_versionNotes)
    insert_md_server("formerReleases_MD", URL_formerReleases)
    
  })
  
  
}
    


library(shiny)
library(shinyjs)

ui <- mod_release_notes_ui("notes")

server <- function(input, output, session) {
  mod_release_notes_server("notes")
}

shinyApp(ui = ui, server = server)
