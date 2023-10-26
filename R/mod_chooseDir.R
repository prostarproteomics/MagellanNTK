#' @title Module choose directory
#'
#' @description  A shiny Module which xxx
#' 
#' @name choose_dir
#' 
#' @param id xxx
#' @param path xxx
#' @param is.enabled xxx
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
    uiOutput(ns('details_ckb_ui')),
    
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
                             path = reactive({'~'}),
                             is.enabled = reactive({TRUE})
                             ) {
  
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    rv <- reactiveValues(path = NULL)
    
    observeEvent(path(), { rv$path <- path()})
    
output$directory_ui <- renderUI({
  widget <- div(id = ns('div_directory'),
      directoryInput(ns('directory'), 
                 label = 'Selected directory', 
                 value = rv$path)
  )
  toggleWidget(widget, condition = is.enabled())
})


output$details_ckb_ui <- renderUI({
  widget <- checkboxInput(ns('details_ckb'), 'Show details', value = FALSE)
  toggleWidget(widget, condition = is.enabled())
})




observeEvent(req(input$directory > 0), ignoreNULL = TRUE, {
 
    # condition prevents handler execution on initial app launch
    rv$path <- choose.dir(default = readDirectoryInput(session, 'directory'),
                      caption="Choose a directory...")
    
    updateDirectoryInput(session, 'directory', value = rv$path)
    })



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

reactive({readDirectoryInput(session, 'directory')})

})
}


#' @export
#' @rdname choose_dir
chooseDir <- function(){
  ui <- fluidPage(
    div(id = 'div_test',
        chooseDir_ui('test')
    ),
    uiOutput('info')
    )
  
  server <- function(input, output, session) {
    
    observe({
      shinyjs::toggleState('div_test', condition=FALSE)
    })
     
    rv <- reactiveValues(
       path = '~'
     )
    
    rv$path <- chooseDir_server('test',
                                path = reactive({'~'}),
                                is.enabled = reactive({TRUE}))

    
    output$info <- renderUI({
      rv$path
      #browser()
      p(rv$path())
    })
  }
  shinyApp(ui, server)
  }

