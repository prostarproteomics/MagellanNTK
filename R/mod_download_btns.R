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
    downloadButton(ns("download_as_Excel_btn"), "Excel",
      class = actionBtnClass
    )
    ,downloadButton(ns("download_as_csv_btn"), "csv",
      class = actionBtnClass
    )
    ,downloadButton(ns("download_raw_data_btn"), "raw",
      class = actionBtnClass
    )
  )
}




#' @rdname mod_download_btns
#' 
#' @param id internal
#' @param df.data internal
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
  name, 
  style = reactive({NULL})) {
  moduleServer(
    id,
    function(input, output, session) {
      
      output$download_as_csv_btn <- downloadHandler(
        filename = function() {
          paste(name(), "-", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
          utils::write.table(df.data(), file, sep = ";", row.names = FALSE)
        }
      )

      output$download_raw_data_btn <- downloadHandler(
        filename = function() {
          paste ("data-", Sys.Date(), ".RData", sep = "")
        },
        content = function(file) {
          saveRDS(df.data(), file=file)
        }
      )
      
      output$download_as_Excel_btn <- downloadHandler(
        filename = function() {
          paste(name(), "-", Sys.Date(), ".xlsx", sep = "")
        },
        content = function(file) {
          #fname <- paste("temp", Sys.Date(), ".xlsx", sep = "")
          write.excel(df = df.data(), style = style(), filename = file)
         # file.rename(fname, file)
        }
      )
    }
  )
}
