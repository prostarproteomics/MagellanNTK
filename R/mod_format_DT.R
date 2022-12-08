#' @title   mod_format_DT_ui and mod_format_DT_server
#' 
#' @description  A shiny Module.
#' 
#' 
#' @name mod_format_DT
#'
#' @keywords internal
#' 
#' 
#' 
#' @example inst/examples/test_mod_format_DT.R
#' 
NULL



#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_format_DT
#' @export
#'  
#' @importFrom shiny NS tagList 
#' @importFrom DT dataTableOutput
#' 
mod_format_DT_ui <- function(id){
  ns <- NS(id)
  tagList(
    useShinyjs(),
    shinyjs::hidden(
      div(id = ns("dl_div"),
        mod_download_btns_ui(ns("DL_btns"))
      )
    ),
    fluidRow(
     column(
    # align = "center",
       width = 12,
        DT::dataTableOutput(ns("StaticDataTable"))
      )
    )
  )
}

# Module Server

#' @rdname mod_format_DT
#' 
#' @param input internal
#' @param output internal
#' @param session internal
#' @param withBtns xxx
#' @param showRownames xxxx
#' @param dom xxx
#' @param style A list of four items:
#' * cols: a vector of colnames of columns to show,
#' * vals: a vector of colnames of columns that contain values,
#' * unique: unique(conds),
#' * pal: RColorBrewer::brewer.pal(3, "Dark2")[seq_len(2)]
#' @param filename xxx
#' 
#' @export
#' 
#' @keywords internal
#' 
#' @import DT
#' @importFrom htmlwidgets JS    
#' 
mod_format_DT_server <- function(id,
  data,
  withDLBtns = FALSE,
  showRownames = FALSE,
  dom = 'Bt',
  style = reactive({NULL}),
  filename = "Prostar_export",
  hideCols = reactive({NULL})
  ){
  
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    proxy = DT::dataTableProxy(session$ns('StaticDataTable'), session)
    
    observe({
      req(data())
      DT::replaceData(proxy, data(), resetPaging = FALSE)
      print(hideCols())
    })
    
    observe({
      shinyjs::toggle("dl_div", condition = isTRUE(withDLBtns))
    })
    
    
    mod_download_btns_server(
      id = "DL_btns",
      df.data = reactive({data()}),
      name = reactive({filename}),
      colors = reactive({NULL}),
      df.tags = reactive({NULL})
    )
    
    
    
    output$StaticDataTable <- DT::renderDataTable(server=TRUE,{
      
      req(length(data()) > 0)
      .jscode <- JS("$.fn.dataTable.render.ellipsis( 30 )")
      #isolate({
        dt <- DT::datatable(
          data(), 
          escape = FALSE,
          rownames= showRownames,
          plugins = "ellipsis",
          options = list(
            #initComplete = initComplete(),
            dom = dom,
            autoWidth = TRUE,
            columnDefs = list(
              list(targets = '_all', className = "dt-center", render = .jscode)
              ,list(targets = hideCols()-1, visible = FALSE)
            )
            #ordering = FALSE
          )
        )

        if (!is.null(style())){
          dt <- dt %>%
            DT::formatStyle(
              columns = style()$cols,
              valueColumns = style()$vals,
              backgroundColor = DT::styleEqual(style()$unique, style()$pal)
            )
        }
      #})
      
      dt
      
    })
    
    initComplete <- function(){
      return (htmlwidgets::JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': 'darkgrey', 'color': 'black'});",
        "}"))
    }
    
    
  })
  
}

## To be copied in the UI
# mod_format_DT_ui("format_DT_ui_1")

## To be copied in the server
# callModule(mod_format_DT_server, "format_DT_ui_1")

