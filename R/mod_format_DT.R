#' @title   format_DT_ui and format_DT_server
#'
#' @description
#'
#' A shiny Module.
#' 
#' 
#' @param id shiny id
#' @param obj xxx
#'
#' @name format_DT
#' 
#' @examplesIf interactive()
#' data(lldata)
#' shiny::runApp(format_DT(assay(lldata[[1]])))
#' 
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
    h3(style="color: blue;", '-- Default format DT --'),
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
  obj){
  
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    proxy = DT::dataTableProxy(session$ns('StaticDataTable'), session)
    
    observe({
      req(obj())
      DT::replaceData(proxy, obj(), resetPaging = FALSE)
    })
    
    output$StaticDataTable <- DT::renderDataTable(server=TRUE,{
      
      req(length(obj()) > 0)
      dt <- DT::datatable(
        obj(), 
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




#' @export
#' @rdname format_DT
#' 
format_DT <- function(obj){
  
stopifnot(inherits(obj, 'data.frame'))
ui <- format_DT_ui("dt")

server <- function(input, output, session) {
  format_DT_server("dt", obj = reactive({obj}) )
}

app <- shinyApp(ui = ui, server = server)
}
