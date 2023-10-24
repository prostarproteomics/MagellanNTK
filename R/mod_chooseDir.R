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
    shinyjs::useShinyjs(),
    uiOutput(ns('directory_ui')),
    uiOutput(ns('details_check_ui')),
    
    shinyjs::hidden(
      div(id = ns('div_details'),
          tags$h5('Files'),
          dataTableOutput(ns('files'))
      )
      )
  )
}
    

#' @import shinyFiles
#' @export
#' @rdname choose_dir
chooseDir_server <- function(id,
                             reset = reactive({NULL}),
                             is.enabled = reactive({TRUE})
                             ) {
  
  # Define default selected values for widgets
  # This is only for simple workflows
  widgets.default.values <- list(
    directory = '~',
    widget2 = NULL,
    widget3 = NULL
  )
  
  
  rv.custom.default.values <- list(
    path = getwd()
  )
  
  
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # DO NOT MODIFY THIS FUNCTION CALL
    eval(
      str2expression(
        Get_AdditionalModule_Core_Code(
          w.names = names(widgets.default.values),
          rv.custom.names = names(rv.custom.default.values)
        )
      )
    )
    
 
session$onSessionEnded(function(){
  stopApp()
})


output$directory_ui <- renderUI({
  widget <- div(id='div_directory',
                directoryInput(ns('directory'), label = 'selected directory', value = '~')
  )
  MagellanNTK::toggleWidget(widget, is.enabled())
})

output$details_check_ui <- renderUI({
  widget <- checkboxInput(ns('details_ckb'), 'Show details', value = FALSE)
  MagellanNTK::toggleWidget(widget, is.enabled())
})

observeEvent(
  ignoreNULL = TRUE,
  eventExpr = {
    input$directory
  },
  handlerExpr = {
    if (input$directory > 0) {
      # condition prevents handler execution on initial app launch
      rv.custom$path = choose.dir(default = readDirectoryInput(session, 'directory'),
                        caption="Choose a directory...")
      updateDirectoryInput(session, 'directory', value = rv.custom$path)
    }
  }
)


observeEvent(input$details_ckb, {
  shinyjs::toggle('div_details', condition = isTRUE(input$details_ckb))
})
  
  
output$directory = renderText({
  readDirectoryInput(session, 'directory')
})

output$files = renderDataTable({
  files = list.files(readDirectoryInput(session, 'directory'), full.names = T)
  data.frame(name = basename(files), file.info(files))
})

reactive(readDirectoryInput(session, 'directory'))

})
}


#' @export
#' @rdname choose_dir
chooseDir <- function(){
  ui <- fluidPage(
    chooseDir_ui('test')
    #uiOutput('info')
    )
  
  server <- function(input, output, session) {
    path <- reactiveVal(chooseDir_server('test', is.enabled = reactive({FALSE})))
    
    output$info <- renderUI({
      p(path()())
    })
  }
  shinyApp(ui, server)
  }

