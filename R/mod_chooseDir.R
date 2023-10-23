#' @title Module choose directory
#'
#' @description  A shiny Module which xxx
#' 
#' @name choose_dir
#' 
#' @param id xxx
#'
#' @examples
#' NULL
#' 
#' @author Samuel Wieczorek
NULL


#' @import shinyFiles
#' @export
#' @rdname choose_dir
chooseDir_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyDirButton(ns("dir"), "Input directory", "Upload"),
    textOutput(ns("dir")) 
  )
}
    

#' @import shinyFiles
#' @export
#' @rdname choose_dir
chooseDir_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
shinyDirChoose(input, 'dir', roots = c(home = '~'))

path <- reactiveVal(getwd())

dir <- reactive(input$dir)

output$dir <- renderText({
  path()
})

observeEvent(ignoreNULL = TRUE,
             eventExpr = {
               input$dir
             },
             handlerExpr = {
               if (!"path" %in% names(dir())) return()
               home <- normalizePath("~")
               path(
                 file.path(home, paste(unlist(dir()$path[-1]), collapse = .Platform$file.sep))
               )
             })

reactive(path())

})
}


#' @export
#' @rdname choose_dir
chooseDir<- function(){
  ui <- fluidPage(
    chooseDir_ui('test'),
    uiOutput('info')
    )
  
  server <- function(input, output, session) {
    path <- reactiveVal(chooseDir_server('test'))
    
    output$info <- renderUI({
      p(path()())
    })
  }
  shinyApp(ui, server)
  }

