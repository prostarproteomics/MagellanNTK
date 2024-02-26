#' @title Download_btns shiny module.
#' 
#' @description A shiny module that shows download buttons in different formats.
#' 
#' @param id internal
#' @param data xxxx
#' @param name xxxx.
#' @param colors xxx
#' @param df.tags xxx
#'
#'
#' @name download_btns
#' @example examples/test_download_btns.R
#'
NULL


#' @rdname download_btns
#'
#' @export
#'
download_btns_ui <- function(id, settings = list()) {
  ns <- NS(id)
  
  tagList(
    downloadButton(ns("download_as_Excel_btn"), "Excel", class = settings$actionBtnClass),
    downloadButton(ns("download_as_csv_btn"), "csv", class = settings$actionBtnClass),
    downloadButton(ns("download_as_RData_btn"), "RData", class = settings$actionBtnClass)
  )
}




#' @rdname download_btns
#' @return NA
#'
#' @export
#'
download_btns_server <- function(id,
                                 data = reactive({NULL}), 
                                 name, 
                                 colors = reactive({NULL}), 
                                 tags = reactive({NULL})) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
      output$download_as_csv_btn <- downloadHandler(
        filename = function() {
          paste(name(), "-", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
          write.table(data(), file, sep = ";", row.names = FALSE)
        }
      )
      
      output$download_as_RData_btn <- downloadHandler(
        filename = function() {
          paste ("data-", Sys.Date(), ".RData", sep = "")
        },
        content = function(fname) {
          saveRDS(data(), file=fname)
        }
      )
      
      output$download_as_Excel_btn <- downloadHandler(
        filename = function() {
          paste(name(), "-", Sys.Date(), ".xlsx", sep = "")
        },
        content = function(file) {
          fname <- paste("temp", Sys.Date(), ".xlsx", sep = "")
          write.excel(
            df = data(),
            colors = colors(),
            tags = tags(),
            filename = fname
          )
          
          file.rename(fname, file)
        }
      )
    }
  )
}


###################################################################
#                             Example                             #
###################################################################

library(shiny)

ui <- fluidPage(
  download_btns_ui(id = 'ex', settings = list(actionBtnClass = "btn-primary"))
)

server <- function(input, output) {
  
  data(iris)
  
  download_btns_server(id = "ex",
                       data = reactive({iris}),
                       name = reactive({"iris"}),
                       colors = reactive({NULL}),
                       tags = reactive({NULL
                      })
  )
}

shinyApp(ui = ui, server = server)

