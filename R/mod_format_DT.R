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
#' obj <- assay(lldata[[1]])
#' shiny::runApp(format_DT(obj))
#' 
#' 
#' #
#' # Compute style from within main tab
#' #
#' data(Exp1_R25_prot, package = 'DaparToolshedData')
#' obj <- data.frame(colData(Exp1_R25_prot))
#' 
#' style <- list(
#'   cols = colnames(obj),
#'   vals = colnames(obj)[2],
#'   unique = unique(obj$Condition),
#'   pal = RColorBrewer::brewer.pal(3,'Dark2')[1:2])
#' shiny::runApp(format_DT(obj, hc_style = style))
#' 
#' 
#' 
#' #
#' # Compute style from within third party tab
#' #
#' obj <- as.data.frame(matrix(1:30, byrow=TRUE, nrow=6))
#' colnames(obj) <- paste0('col', 1:5)
#' 
#' mask <- as.data.frame(matrix(rep(LETTERS[1:5], 6), byrow=TRUE, nrow=6))
#' 
#' 
#' style <- list(
#'   cols = colnames(obj),
#'   vals = colnames(mask),
#'   unique = unique(mask),
#'   pal = RColorBrewer::brewer.pal(5,'Dark2')[1:5]
#' )
#' 
#' shiny::runApp(format_DT(obj, 
#'   hidden = mask,
#'   hc_style = style))
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
    h3(style="color: blue;", 'Format DT (default)'),
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
  obj = reactive({NULL}),
  hidden = reactive({NULL}),
  withDLBtns = FALSE,
  showRownames = FALSE,
  dom = 'Bt',
  hc_style = reactive({NULL}),
  remoteReset = reactive({NULL}),
  is.enabled = reactive({TRUE})
){
  
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    rv.infos <- reactiveValues(
      obj = NULL
    )
    
    
    proxy = DT::dataTableProxy(session$ns('StaticDataTable'), session)
    
    observe({
      req(obj())
      rv.infos$obj <- obj()
      if(!is.null(hidden()))
        rv.infos$obj <- cbind(obj(), hidden())
      
      
      DT::replaceData(proxy, rv.infos$obj, resetPaging = FALSE)
    })
    
    initComplete <- function(){
      return (htmlwidgets::JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': 'darkgrey', 'color': 'black'});",
        "}"))
    }
    
    
    
    GetColumnDefs <- reactive({
      .jscode <- DT::JS("$.fn.dataTable.render.ellipsis( 30 )")
      
      if (!is.null(hidden())) {
         tgt.seq <- seq.int(from = ncol(obj()), to = ncol(obj()) + ncol(hidden()) -1)
        list(
          list(targets = '_all', className = "dt-center", render = .jscode)
          ,list(targets = tgt.seq, visible = FALSE)
        ) 
      } else { 
        list(list(targets = '_all', className = "dt-center", render = .jscode))
      }
    })
    
    
    output$StaticDataTable <- DT::renderDataTable(server=TRUE,{
      
      req(length(rv.infos$obj) > 0)
      .jscode <- DT::JS("$.fn.dataTable.render.ellipsis( 30 )")
      
      dt <- DT::datatable(
        rv.infos$obj, 
        escape = FALSE,
        rownames= showRownames,
        plugins = "ellipsis",
        options = list(
          #initComplete = initComplete(),
          dom = dom,
          autoWidth = TRUE,
          columnDefs = GetColumnDefs()
          #ordering = FALSE
        )
      )
      

      if (!is.null(hc_style())){
        dt <- dt %>%
          DT::formatStyle(
            columns = hc_style()$cols,
            valueColumns = hc_style()$vals,
            target = 'cell',
            backgroundColor = DT::styleEqual(hc_style()$unique, hc_style()$pal)
          )
      }
      
      
      dt
      
    })
    
  })
  
}




#' @export
#' @rdname format_DT
#' 
format_DT <- function(obj,
  hidden = NULL,
  withDLBtns = FALSE,
  showRownames = FALSE,
  dom = 'Bt',
  hc_style = NULL,
  remoteReset = NULL,
  is.enabled = TRUE
  ){
  
stopifnot(inherits(obj, 'data.frame'))
ui <- format_DT_ui("dt")

server <- function(input, output, session) {
  format_DT_server("dt", 
    obj = reactive({obj}),
    hidden = reactive({hidden}),
    withDLBtns = withDLBtns,
    showRownames = showRownames,
    dom = dom,
    hc_style = reactive({hc_style}),
    remoteReset = reactive({remoteReset}),
    is.enabled = reactive({is.enabled}))
}

app <- shinyApp(ui = ui, server = server)
}
