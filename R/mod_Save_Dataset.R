#' @title mod_Load_Dataset
#' 
#' @description  A shiny Module.
#' @param id xxx
#'
#' @rdname mod_Save_Dataset
#' 
#' @export
#' 
mod_Save_Dataset_ui <- function(id){
  ns <- NS(id)
  uiOutput(ns('show_download_ui'))
}

#' @param dataIn xxx
#' 
#' @return NA 
#' 
#' @rdname mod_Save_Dataset
#' 
#' @export
#' 
mod_Save_Dataset_server <- function(id, 
                                    dataIn = reactive({NULL})
                                    ){
  
  
  req(dataIn())
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # modal <- function(){
    #   modalDialog(
    #     #textInput(ns("nameExport"), 
    #     #          label = "Save as rds file. Choose filename"),
    #     footer = tagList(
    #       modalButton('Cancel'),
    #       downloadLink(ns('download'), 'Download file')
    #     )
    #   )
    # }
    # 
    # observe({showModal(modal())})
    # 
    # observeEvent(input$Cancel, {removeModal()})

    
    output$show_download_ui <- renderUI({
      
    downloadLink(ns('download'), 'Quick link')
    })
    
    
    
    output$download <- downloadHandler(
      filename = function() {
        #paste0(input$nameExport, '.rds')
        paste0('foo.rds')
        },
      content = function(file) {
        fname <- tempfile()
        saveRDS(dataIn(), file = fname)
        file.copy(fname, file)
        file.remove(fname)  
        removeModal()
        }
      )
  })
}
