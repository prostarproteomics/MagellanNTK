#' @title   format_DT_ui and format_DT_server
#'
#' @description
#'
#' A shiny Module.
#' 
#' 
#' @param id shiny id
#' @param data xxx
#' @param withDLBtns xxx
#' @param showRownames xxx
#' @param dom xxx
#' @param hc_style xxx
#' @param full_style A list of four items:
#' * cols: a vector of colnames of columns to show,
#' * vals: a vector of colnames of columns that contain values,
#' * unique: unique(conds),
#' * pal: RColorBrewer::brewer.pal(3, "Dark2")[seq_len(2)]
#' @param filename xxx
#' @param hideCols xxx
#'
#' @name format_DT
#' 
#' @return NA
#' 
NULL



#' @importFrom shiny NS tagList
#'
#' @export
#' @rdname format_DT
#'
format_DT_ui <- function(id) {
  if (!requireNamespace("DT", quietly = TRUE)) {
    stop("Please install DT: BiocManager::install('DT')")
  }
  
  ns <- NS(id)
  tagList(
    useShinyjs(),
    h3(style="color: blue;", '-- Default format DT module --'),
    fluidRow(
      column( width = 12,
        DT::dataTableOutput(ns("StaticDataTable"))
      )
    )
  )
}

#'
#' @export
#'
#' @importFrom htmlwidgets JS
#' @rdname format_DT
format_DT_server <- function(id,
                             data){
  
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    proxy = DT::dataTableProxy(session$ns('StaticDataTable'), session)
    
    observe({
      req(data())
      DT::replaceData(proxy, data(), resetPaging = FALSE)
    })
    
    output$StaticDataTable <- DT::renderDataTable(server=TRUE,{
      
      req(length(data()) > 0)
      dt <- DT::datatable(
        data(), 
        escape = TRUE,
        options = list(
          #initComplete = initComplete(),
          dom = 'Bt',
          autoWidth = TRUE
          )
      )
      dt
      
    })
    
  })
  
}





library(shiny)
library(shinyjs)

ui <- format_DT_ui("dt")

server <- function(input, output, session) {
  format_DT_server("dt", data = reactive({iris}) )
}

shinyApp(ui = ui, server = server)
