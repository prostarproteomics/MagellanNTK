# ouvre une fenetre, parametre renseigne quoi afficher dans la fenetre

#' @title Predefined modal
#'
#' @description Displays of formatted modal-dialog with 'Cancel' and 
#' 'Ok' buttons.
#' 
#' @param id A `character(1)` which is the id of the instance of the module
#' @param title A `character(1)`
#' @param width A `character(1)` indicating the size of the modal window. Can 
#' be "s" for small (the default), "m" for medium, or "l" for large.
#' @param uiContent The content of the modal dialog.
#'
#' @importFrom shinyjqui jqui_draggable
#' 
#' @name modalDialog
#' 
#' @return A Shiny modal-dialog
#'
NULL

#'
#' @rdname modalDialog
#'
#' @export
#'
modalDialog_ui <- function(id){
  # create the namespace from the id
  ns <- NS(id)
  
  fluidPage(
    uiOutput(ns('dialog_UI'))
  )
}


#' @export
#'
#' @rdname modalDialog
#' 
#' @import shiny
#' @import shinyBS
#' @import shinyjqui
#'
modalDialog_server <- function(id,
                           title = NULL,
                           width = NULL,
                           uiContent = NULL,
                           external_mod = NULL,
                           external_mod_args = list()){ #height auto
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # reactiveValues object for storing current data set.
    dataOut <- reactiveVal(NULL)
    tmp <- reactiveVal(NULL)
    
    output$dialog_UI <- renderUI({
      if (!is.null(uiContent) && !is.null(external_mod)){
        warning('uiContent and external_mod cannot be both instantiated at the same time.')
      } else
       actionButton(ns("show"), "Show modal dialog",
                   icon("chart-bar", lib = "font-awesome"),
                   class = "btn-success")
    })
    

    # Show modal when button is clicked.
    observeEvent(input$show, {
     #req(external_mod)
     
      if (is.null(width)) {
        width <- "small"
      }

      tagList(
        tags$head(tags$style(paste0(".modal-dialog { width:", width, " }"))),
        tags$head(tags$style(".modal-dialog {z-index: 1000;}")),
        tags$head(tags$style("#test .modal-dialog {width: fit-content !important;}")),
        showModal(
          modalDialog(
            if (!is.null(uiContent))
              uiContent
            else
              do.call(paste0(external_mod, '_ui'), list(id = ns('test'))),
            footer = tagList(
              modalButton("Cancel"),
              actionButton(ns("ok"), "OK2") # wrapped in ns()
              )
            )
          )
      )

    })
    
    observe({
      req(external_mod)
      args <- list(id = 'test')
      if (length(external_mod_args))
        args <- list(args, external_mod_args)
        
      tmp(do.call(paste0(external_mod, '_server'), args))
    })
    
    # When OK button is pressed, attempt to load the data set. If successful,
    # remove the modal. If not show another modal, but this time with a failure
    # message.
    observeEvent(input$ok, {
      if (!is.null(external_mod))
        dataOut(tmp()())
      
      removeModal()
    })
    
    
    return(reactive({dataOut()}))
  })
}



######################################################################
# Example
########################################################################

library(shiny)


simple_mod_ui <- function(id){
  # create the namespace from the id
  ns <- NS(id)
  
  fluidPage(
    actionButton(ns("test"), "Test")
  )
}


simple_mod_server <- function(id){ #height auto
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # reactiveValues object for storing current data set.
    dataOut <- reactiveVal(NULL)
    
    observeEvent(input$test, {
      dataOut(paste0('Clicked ', input$test, ' times.'))
    })
    
    
    return(reactive({dataOut()}))
  })
}





ui <- fluidPage(
  modalDialog_ui(id = "tbl1"),
  modalDialog_ui(id = "tbl2"),
  modalDialog_ui(id = "tbl3")
)

server <- function(input, output) {
  
   res1 <- modalDialog_server(id = "tbl1",
                  title = "test modalDialog",
                  uiContent = p("test"))
  
  res2 <- modalDialog_server(id = "tbl2",
                            title = "test modalDialog",
                            external_mod = 'simple_mod',
                            external_mod_args = list()
                            )
  
  
  funcs <- c('convert', 
             'open_dataset', 
             'open_demoDataset',
             'view_dataset',
             'infos_dataset')
  res3 <- modalDialog_server(id = "tbl3",
                             title = "test modalDialog",
                             external_mod = 'loadapp',
                             external_mod_args = list(funcs = funcs)
                             )
  
  
  observeEvent(req(res2()), {
    print(res2())
  })
  
  observeEvent(req(res3()), {
    print(res3())
  })
}

shinyApp(ui = ui, server = server)


