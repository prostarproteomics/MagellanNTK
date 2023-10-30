# ouvre une fenetre, parametre renseigne quoi afficher dans la fenetre


#' @title Predefined modal
#'
#' @description Displays of formatted modal-dialog with 'Cancel' and 
#' 'Ok' buttons.
#'
#' @rdname bsmodal
#'
#' @export
#'
bsmodal_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("bsmodal_ui"))
  )
}


#' @param id A `character(1)` which is the id of the instance of the module
#' @param title A `character(1)`
#' @param width A `character(1)` indicating the size of the modal window. Can 
#' be "s" for small (the default), "m" for medium, or "l" for large.
#' @param uiContent The content of the modal dialog.
#'
#' @importFrom shinyjqui jqui_draggable
#'
#' @export
#'
#' @return A Shiny modal-dialog
#'
#' @examples
#' if (interactive()) {
#'     library(shiny)
#'     library(shinyBS)
#'
#'     ui <- fluidPage(
#'         bsmodal_ui("tbl")
#'     )
#'     server <- function(input, output) {
#'         bsmodal_server(
#'             id = "tbl",
#'             title = "test",
#'             uiContent = p("test")
#'         )}
#'     shinyApp(ui, server)
#' }
#'
#' @rdname bsmodal
#'
bsmodal_server <- function(id,
                           title = NULL,
                           width = NULL,
                           uiContent = NULL,
                           mod.funcs = NULL) { # height auto
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    rv <- reactiveValues(temp = NULL)
    dataOut <- reactiveVal(NULL)
    
    # shinyjqui::jqui_resizable(paste0("#",ns("fenetre")," .modal-content")
    #                ,options = list(minHeight = 500, minWidth=500  ))
    #
    shinyjqui::jqui_draggable(paste0("#", ns("window"), " .modal-content"),
                              options = list(revert = TRUE)
    )

    rv$temp <- do.call(mod.funcs$server.func, 
                       append(list(id = mod.funcs$id),
                              mod.funcs$server.params))
    
    observeEvent(rv$temp(), {
      print(rv$temp())
      
    })
    
    
    output$bsmodal_ui <- renderUI({
      if (is.null(width)) {
        width <- "small"
      }
      tagList(
        tags$head(tags$style(paste0(".modal-dialog { 
                    width:", width, " }"))),
        tags$head(tags$style(".modal-dialog {z-index: 1000;}")),
        tags$head(
          tags$style("#test .modal-dialog {
                        width: fit-content !important;}")),
        actionButton(ns("openModalBtn"), "",
                     icon("chart-bar", lib = "font-awesome"),
                     class = "btn-success"
        ),
        # div(id = 'test',
        #ui <- uiContent,
        
        
        shinyBS::bsModal(ns("window"),
                         title = title,
                         trigger = ns("openModalBtn"),
                         do.call(mod.funcs$ui.func, 
                                 append(list(id = ns(mod.funcs$id)),
                                        mod.funcs$ui.params))
        )
        #  )
      )
    })
  })
}



if (interactive()) {
  library(shiny)
  library(shinyBS)
  
  ui <- fluidPage(
    bsmodal_ui("tbl")
  )
  server <- function(input, output) {
    bsmodal_server(id = "tbl",
                   title = "test",
                   mod.funcs = list(id = 'toto',
                                    ui.func = mdEditor_ui,
                                    ui.params = list(),
                                    server.func = mdEditor_server,
                                    server.params = list())
    )
  }
  shinyApp(ui, server)
}
