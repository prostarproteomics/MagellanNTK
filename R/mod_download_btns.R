#' @title   mod_format_DT_ui and mod_format_DT_server
#' 
#' @description  A shiny Module.
#' 
#' 
#' @name mod_download_btns
#'
#' @keywords internal
#' 
#' @example examples/test_mod_download_btns.R
#' 
NULL



#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_download_btns
#' @export
#'  
#' @importFrom shiny NS tagList 
#' @importFrom DT dataTableOutput
#' 
mod_download_btns_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    shinyjs::useShinyjs(),
    uiOutput(ns('dl_xl')),
    uiOutput(ns('dl_csv')),
    uiOutput(ns('dl_raw'))
  )
}




#' @rdname mod_download_btns
#' 
#' @param id internal
#' @param df.data internal
#' @param extension Available values are `csv` (default), `RData` and `Excel`.
#' @param widget.type Available values are `Button` and `Link` (default).
#' @param name internal
#' @param style xxx
#' 
#' @export
#' 
#' @keywords internal
#' 
#' @import DT
#' @importFrom htmlwidgets JS    
#' 
mod_download_btns_server <- function(id, 
  df.data,
  extension = reactive({'csv'}),
  widget.type = reactive({'Link'}),
  name, 
  excel.style = reactive({NULL})) {
  moduleServer(
    id, function(input, output, session) {
      ns <- session$ns
      
      rv <- reactiveValues(
        UI_type = NULL
      )
      
      GetType <- reactive({
        if(length(extension()) != length(widget.type())){
          warning("Widget.type is not correctly configured. As one cannot decide, 
            all values are set to default ('Link')")
          rv$UI_type <- rep('Link', length(extension()))
        } else {
          rv$UI_type <- widget.type()
        }
        
        rv$UI_type
      })
      
      output$dl_csv <- renderUI({
        req('csv' %in% extension())
        type <- GetType()[which(extension() == 'csv')]
        do.call(paste0('download', type),
          list(
            ns("downloadDatacsv"), 
            "csv",
            class = if (type=='Button') actionBtnClass else ''
          )
          )
      })
      
      
      output$dl_xl <- renderUI({
        req('Excel' %in% extension())
        type <- GetType()[which(extension() == 'Excel')]
        do.call(paste0('download', type),
          list(
            ns("downloadDataExcel"), 
            "Excel",
            class = if (type=='Button') actionBtnClass else ''
          )
        )
      })
      
      output$dl_raw <- renderUI({
        req('RData' %in% extension())
        type <- GetType()[which(extension() == 'RData')]
        do.call(paste0('download', type),
          list(
            ns("downloadDataRData"), 
            "RData",
            class = if (type=='Button') actionBtnClass else ''
          )
        )
      })
      
      output$downloadDatacsv <- downloadHandler(
        filename = function() {
          paste(name(), "-", Sys.Date(), ".csv", sep = "")
        },
        content = function(fname) {
          utils::write.table(df.data(), fname, sep = ";", row.names = FALSE)
        }
      )
      
      output$downloadDataRData <- downloadHandler(
        filename = function() {
          paste ("data-", Sys.Date(), ".RData", sep = "")
        },
        content = function(fname) {
          saveRDS(df.data(), file=fname)
        }
      )
      
      output$downloadDataExcel <- downloadHandler(
        filename = function() {
          paste(name(), "-", Sys.Date(), ".xlsx", sep = "")
        },
        content = function(fname) {
          write.excel(df = df.data(), style = excel.style(), filename = fname)
        }
      )
  }
  )
}
