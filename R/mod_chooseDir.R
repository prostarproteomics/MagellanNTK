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
  fluidPage(
    fluidRow(
      column(1),
      column(
        width = 10,
        
        # Application title
        #titlePanel("Directory Input Demo"),
        directoryInput(ns('directory'), label = 'selected directory', value = '~')
        #tags$h5('Files'),
        #dataTableOutput(ns('files'))
      ),
      column(1)
    )
  )
}
    

#' @import shinyFiles
#' @export
#' @rdname choose_dir
chooseDir_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    

path <- reactiveVal(getwd())

session$onSessionEnded(function(){
  stopApp()
})

observeEvent(
  ignoreNULL = TRUE,
  eventExpr = {
    input$directory
  },
  handlerExpr = {
    if (input$directory > 0) {
      # condition prevents handler execution on initial app launch
      path = choose.dir(default = readDirectoryInput(session, 'directory'),
                        caption="Choose a directory...")
      updateDirectoryInput(session, 'directory', value = path)
    }
  }
)

output$directory = renderText({
  readDirectoryInput(session, 'directory')
})

# output$files = renderDataTable({
#   files = list.files(readDirectoryInput(session, 'directory'), full.names = T)
#   data.frame(name = basename(files), file.info(files))
# })

reactive(readDirectoryInput(session, 'directory'))

})
}


#' @export
#' @rdname choose_dir
chooseDir <- function(){
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

