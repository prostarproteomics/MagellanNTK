#' @title   mod_homepage_ui and mod_homepage_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param mdfile xxx
#' 
#' @name mod_homepage
#' 
#' @examplesIf interactive()
#' shiny::runApp(mod_homepage())
#' 
#' 
NULL


#' @rdname mod_homepage
#' @export 
#' @importFrom shiny NS tagList 
mod_homepage_ui <- function(id){
  ns <- NS(id)
  tagList(
    insert_md_ui(ns("md_file"))
  )
}
    

    
#' @rdname mod_homepage
#' @export
mod_homepage_server <- function(id,
  mdfile = file.path(system.file('app/md', 
    package = 'MagellanNTK'),'Presentation.Rmd')){
  
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    if(file.exists(mdfile))
      .mdfile <- mdfile
    else
      .mdfile <-file.path(system.file('app/md', 
        package = 'MagellanNTK'),'404.Rmd')
    
    insert_md_server("md_file", normalizePath(.mdfile))
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
