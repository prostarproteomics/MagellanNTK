#' @title Predefined modal
#'
#' @description Displays of formatted modal-dialog with 'Cancel' and 
#' 'Ok' buttons.
#'
#' @param id A `character(1)` which is the id of the instance of the module
#' @param label xxx
#' @param title A `character(1)`
#' @param width A `character(1)` indicating the size of the modal window. Can 
#' be "s" for small (the default), "m" for medium, or "l" for large.
#' @param uiContent The content of the modal dialog.
#' @param shiny.module xxx
#' @param ui.func xxx
#' @param ui.params xxx
#' @param server.func xxx
#' @param server.params xxx
#'
#' @export
#'
#' @return A Shiny modal-dialog
#'
#' @examplesIf interactive()
#' shiny::runApp(mod_bsmodal())
#'
#' @name mod_bsmodal
#'
NULL




#' @importFrom shiny uiOutput tagList NS
#' @rdname mod_bsmodal
#'
#' @export
#'
mod_bsmodal_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("bsmodalUI"))
  )
}



#' @rdname mod_bsmodal
#' @export
#' @importFrom shiny moduleServer observe reactiveVal reactive observeEvent
#' req renderUI tagList actionButton 
#' @importFrom shinyBS bsModal
#' @importFrom shinyjqui jqui_resizable jqui_draggable
#'
mod_bsmodal_server <- function(id,
                           label = "Edit md",
                           title = NULL,
                           width = NULL,
                           uiContent = NULL,
                           shiny.module = NULL) { # height auto
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    observe({
      if(!is.null(uiContent) && !is.null(shiny.module)){
        warning('There can be only one non-NULL among uiContent and shiny.module')
        return(NULL)
      }
    })
    
    dataOut <- reactiveVal(reactive({NULL}))
    
     shinyjqui::jqui_resizable(paste0("#",ns("window")," .modal-content")
                    ,options = list(minHeight = 500, minWidth=500  ))
    
    shinyjqui::jqui_draggable(paste0("#", ns("window"), " .modal-content"),
                              options = list(revert = TRUE)
    )

    # observeEvent(req(dataOut()()), {
    #   print(paste0('dataOut = ', dataOut()()))
    #   
    # })
    
    
    GetUI <- reactive({
      if(!is.null(uiContent))
        ui <- uiContent
      else if(!is.null(shiny.module$ui.func))
        ui <- do.call(shiny.module$ui.func, 
                      append(list(id = ns('id')),
                             shiny.module$ui.params))
      
      ui
    })
    
    output$bsmodalUI <- renderUI({
      if (is.null(width)) {
        width <- "small"
      }
      
      if(!is.null(shiny.module$server.func))
        dataOut(do.call(shiny.module$server.func, 
                        append(list(id = 'id'),
                               shiny.module$server.params)))
      
      tagList(
        tags$head(tags$style(paste0(".modal-dialog { 
                    width:", width, " }"))),
        tags$head(tags$style(".modal-dialog {z-index: 1000;}")),
        tags$head(
          tags$style("#test .modal-dialog {width: fit-content !important;}")),
        actionButton(ns("openModalBtn"), label,
                     icon("chart-bar", lib = "font-awesome"),
                     class = "btn-success"
        ),
        
        shinyBS::bsModal(ns("window"),
                         title = title,
                         trigger = ns("openModalBtn"),
                         GetUI()
        )
      )
    })
    
    reactive({dataOut()()})
  })
}



#' @export
#' @rdname mod_bsmodal
#' @importFrom shiny fluidPage tagList p uiOutput reactiveValues renderUI
#' req shinyApp
#' 
mod_bsmodal <- function(title = 'test',
  ui.func = NULL,
  ui.params = list(),
  server.func = NULL,
  server.params = list()){
  
  ui <- fluidPage(
    tagList(
      uiOutput('res'),
      bsmodal_ui("tbl")
    )
  )
  server <- function(input, output, session) {
    rv <- reactiveValues(res=NULL)
    rv$res <- bsmodal_server(id = "tbl",
                             title = title,
                             shiny.module = list(ui.func = ui.func,
                                              ui.params = ui.params,
                                              server.func = server.func,
                                              server.params = server.params)
    )
   
    
    output$res <- renderUI({
      req(rv$res())
      p(rv$res())
    })
  }
  app <- shinyApp(ui, server)
}
