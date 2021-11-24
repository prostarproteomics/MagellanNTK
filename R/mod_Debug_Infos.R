#' @title mod_Debug_Infos
#' 
#' @description  A shiny Module.
#' @param id xxx
#'
#' @rdname mod_Debug_Infos
#' 
#' @export
#' 
mod_Debug_Infos_ui <- function(id){
  ns <- NS(id)
  uiOutput(ns('show_Debug_Infos'))
}

# Module Server
#' @title xxx
#' 
#' @description xxx
#' 
#' 
#' @param id xxx
#' 
#' @param title xxx
#' @param config xxx
#' @param rv.dataIn xxx
#' @param dataIn xxx
#' @param dataOut xxxx
#' @param steps.status xxx
#' @param current.pos xxx
#' @param steps.enabled xxxx
#' @param is.enabled xxx
#' 
#' @export
#' @return xxx 
#' 
#' @examples 
#' \dontrun{
#' 
#' ui <- fluidPage(
#'   mod_format_DT_ui('tbl')
#' )
#' server <- function(input, output){
#'   mod_format_DT_server(id = 'tbl',table2show = reactive({head(iris)}))
#' }
#' shinyApp(ui, server)
#' }
#' 
#' @rdname mod_Debug_Infos
#' 
#' @export
#' 
mod_Debug_Infos_server <- function(id,
                                   title = NULL,
                                   config = reactive({NULL}),
                                   rv.dataIn = reactive({NULL}),
                                   dataIn = reactive({NULL}),
                                   dataOut = reactive({NULL}),
                                   steps.status = reactive({NULL}),
                                   current.pos = reactive({NULL}),
                                   steps.enabled = reactive({NULL}),
                                   is.enabled = reactive({NULL})
                                   ){
  
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # The following functions are only there for dev and debugging reasons
    # They will not be part of the final code
    
    output$show_Debug_Infos <- renderUI({
      wellPanel(
        h3(title),
        uiOutput(ns('show_tag_enabled')),
        fluidRow(
          column(width=2,
                 tags$b(h4(style = 'color: blue;', paste0("dataIn() ", config()$type))),
                 uiOutput(ns('show_dataIn'))),
          column(width=2,
                 tags$b(h4(style = 'color: blue;', paste0("rv$dataIn ", config()$type))),
                 uiOutput(ns('show_rv_dataIn'))),
          column(width=2,
                 tags$b(h4(style = 'color: blue;', paste0("dataOut()$value ", config()$type))),
                 uiOutput(ns('show_rv_dataOut'))),
          column(width=4,
                 tags$b(h4(style = 'color: blue;', "status")),
                 uiOutput(ns('show_status')))
        )
      )
    })
    
    ###########---------------------------#################
    output$show_dataIn <- renderUI({
      req(dataIn())
      tagList(
        lapply(names(dataIn()), function(x){tags$p(x)})
      )
    })
    
    
    output$show_rv_dataOut <- renderUI({
      tagList(
        lapply(names(dataOut()$value), function(x){tags$p(x)})
      )
    })
    
    GetStringStatus <- function(name){
      if (name == global$VALIDATED) 'Validated'
    else if (name == global$UNDONE) 'Undone'
    else if (name == global$SKIPPED) 'Skipped'
  }
    output$show_status <- renderUI({
      tagList(lapply(seq_len(length(config()$steps)), 
                     function(x){
                       color <- if(steps.enabled()[x]) 'black' else 'lightgrey'
                       if (x == current.pos())
                         tags$p(style = paste0('color: ', color, ';'),
                                tags$b(paste0('---> ', config()$steps[x], ' - ', GetStringStatus(steps.status()[[x]])), ' <---'))
                       else 
                         tags$p(style = paste0('color: ', color, ';'),
                                paste0(config()$steps[x], ' - ', GetStringStatus(steps.status()[[x]])))
                     }))
    })
    
    
    output$show_tag_enabled <- renderUI({
      tagList(
        p(paste0('steps.enabled = ', paste0(as.numeric(steps.enabled()), collapse=' '))),
        p(paste0('enabled() = ', as.numeric(is.enabled())))
      )
    })
    
    
    
    output$show_rv_dataIn <- renderUI({
      req(rv.dataIn())
      tagList(
        lapply(names(rv.dataIn()), function(x){tags$p(x)})
      )
    })
    
  })
  
}

## To be copied in the UI
# mod_format_DT_ui("format_DT_ui_1")

## To be copied in the server
# callModule(mod_format_DT_server, "format_DT_ui_1")

