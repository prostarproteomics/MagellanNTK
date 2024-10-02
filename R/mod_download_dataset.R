#' @title dl
#'
#' @description  A shiny Module.
#' 
#' 
#' @param id internal
#' @param dataIn internal
#'@param widget.type Available values are `Button` and `Link` (default).
#' @param name internal
#' @param filename xxx
#'
#' @return NA
#'
#' @name download_dataset
#' @examples
#' \dontrun{
#' data(sub_R25)
#' shiny::runApp(download_dataset(sub_R25))
#' 
#' shiny::runApp(download_dataset(sub_R25, filename = 'myDataset'))
#' }
#'
NULL


#' @import shiny
#'
#' @rdname download_dataset
#'
#' @export
#'
download_dataset_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3('Donwload dataset (default)'),
    uiOutput(ns('dl_xl')),
    uiOutput(ns('dl_csv')),
    uiOutput(ns('dl_raw'))
  )
}

#' @rdname download_dataset
#'
#' @export
#'
download_dataset_server <- function(id,
  dataIn = reactive({NULL}),
  widget.type = 'Link',
  filename = 'myDataset') {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    rv <- reactiveValues(
      UI_type = NULL,
      export_file_RData = NULL
    )
    
    
    BuildTempFile <- function(filename){
      path <- tempfile(pattern = filename, tmpdir = tempdir(), fileext = ".RData")
      prefix <- unlist(strsplit(path, split = filename))[1]
      paste0(prefix, filename, '.RData')
    }
    
    observeEvent(dataIn(), ignoreNULL = TRUE,{
      
      
      rv$export_file_RData <- tryCatch({
        out.RData <- BuildTempFile(filename)

        saveRDS(dataIn(), file = out.RData)
        out.RData
      },
        warning = function(w) w,
        error = function(e) e
      )
      
      print(rv$export_file_RData)

    })
    
    
    
    output$dl_raw <- renderUI({
      req(rv$export_file_RData)
      
      do.call(paste0('download', widget.type),
        list(
          ns("downloadDataRData"), 
          "RData",
          class = if (widget.type=='Button') actionBtnClass else ''
        )
      )
    })
    

    output$downloadDataRData <- downloadHandler(
      filename = function() {
        #paste ("data-", Sys.Date(), ".RData", sep = "")
        paste(filename, '.RData', sep = "")
      },
      content = function(file) {
        file.copy(
          from = rv$export_file_RData,
          to = file
        )
      }
    )
    
  }
  )
}




#' @rdname download_dataset
#'
#' @export
#'
download_dataset <- function(data, filename = 'myDataset'){
  ui <- download_dataset_ui("dl")
  
  server <- function(input, output, session) {
    
    download_dataset_server("dl",
      dataIn = reactive({data}),
      filename = filename
    )
  }
  
  app <- shiny::shinyApp(ui = ui, server = server)
}
