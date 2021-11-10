#' @title   mod_format_DT_ui and mod_format_DT_server
#' 
#' @description  A shiny Module.
#' @param id xxx
#'
#' @rdname mod_format_DT
#' 
mod_Debug_Infos_ui <- function(id){
  ns <- NS(id)
  wellPanel(title = 'foo',
            tagList(
              uiOutput(ns('show_Debug_Infos'))
            )
  )
}

# Module Server
#' @title xxx
#' 
#' @description xxx
#' 
#' 
#' @param id xxx
#' @param table2show xxx
#' @param style xxx
#' @param withBtns xxx
#' @param showRownames xxxx
#' @param dom xxx
#' 
#' @export
#' 
#' @importFrom DT replaceData dataTableProxy datatable %>% 
#' @importFrom htmlwidgets JS
#' 
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
#' @rdname mod_format_DT
#' 
mod_Debug_Infos_server <- function(id,
                                   config,
                                   rv.dataIn,
                                   dataIn,
                                   dataOut,
                                   steps.status,
                                   current.pos,
                                   steps.enabled,
                                   is.enabled){
  
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    eval(str2expression(GetCode_GetStringStatus()))
    
    # The following functions are only there for dev and debugging reasons
    # They will not be part of the final code
    
    output$show_Debug_Infos <- renderUI({
      tagList(
        h3(paste0('module process "', id, '"')),
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
                 uiOutput(ns('show_rv_dataOut()'))),
          column(width=4,
                 tags$b(h4(style = 'color: blue;', "status")),
                 uiOutput(ns('show_status')))
        )
      )
    })
    
    ###########---------------------------#################
    output$show_dataIn <- renderUI({
      # if (verbose) cat(grey(paste0(id, '::output$show_dataIn\n\n')))
      req(dataIn())
      tagList(
        # h4('show dataIn()'),
        lapply(names(dataIn()), function(x){tags$p(x)})
      )
    })
    
    
    output$show_rv_dataOut <- renderUI({
      req(dataOut()$value)
      #if (verbose) cat(grey(paste0(id, '::output$show_rv_dataOut()\n\n')))
      tagList(
        #h4('show dataOut()$value'),
        lapply(names(dataOut()$value), function(x){tags$p(x)})
      )
    })
    
    
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
      # if (verbose) cat(paste0('::output$show_rv_dataIn from - ', id, "\n\n"))
      req(rv.dataIn())
      tagList(
        # h4('show dataIn()'),
        lapply(names(rv.dataIn()), function(x){tags$p(x)})
      )
    })
    
  })
  
}

## To be copied in the UI
# mod_format_DT_ui("format_DT_ui_1")

## To be copied in the server
# callModule(mod_format_DT_server, "format_DT_ui_1")

