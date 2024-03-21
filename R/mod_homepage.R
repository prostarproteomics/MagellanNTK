#' @title   mod_homepage_ui and mod_homepage_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @name mod_homepage
#' 
#' @examplesIf interactive()
#' shiny::runApp(mod_homepage())
#' 
#' 
NULL


#' @rdname mod_homepage
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_homepage_ui <- function(id){
  ns <- NS(id)
  tagList(
    insert_md_ui(ns("Presentation"))
  )
}
    

    
#' @rdname mod_homepage
#' @export
#' @keywords internal
mod_homepage_server <- function(id,
  dirpath = system.file('app/md', package = 'MagellanNTK'),
  filename = 'Presentation.Rmd'){
  
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    insert_md_server("Presentation",
      file.path(dirpath, filename))
  })

}


#' @export
#' @rdname mod_homepage
#' @importFrom shiny fluidPage shinyApp
#' 
mod_homepage <- function(){
    ui <- fluidPage(
        mod_homepage_ui("mod_pkg")
    )

    server <- function(input, output, session) {
      mod_homepage_server("mod_pkg")
    }

    app <- shinyApp(ui, server)
}
