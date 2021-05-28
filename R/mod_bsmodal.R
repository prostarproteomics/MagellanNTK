# ouvre une fenetre, parametre renseigne quoi afficher dans la fenetre


#' @title xxx
#' 
#' @description xxx
#' 
#' @noRd
#' 
mod_bsmodal_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("bsmodal_ui"))
  )
}

#' @title xxxx
#' 
#' @description
#' xxxx
#' 
#' @param id xxxx
#' @param title xxx
#' @param width xxx
#' @param uiContent xxx
#' 
#' @export
#' 
#' @return xxx
#' 
#' @examples 
#' library(shiny)
#' library(shinyBS)
#' 
#' ui <- fluidPage(
#'   mod_bsmodal_ui('tbl')
#' )
#' server <- function(input, output){
#'   mod_bsmodal_server(id = 'tbl',
#'                      title = 'test',
#'                      uiContent = p('test'))
#' }
#' shinyApp(ui, server)
#' 
mod_bsmodal_server <- function(id,
                               title = NULL,
                               width = NULL,
                               uiContent = NULL){ #height auto
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # shinyjqui::jqui_resizable(paste0("#",ns("fenetre")," .modal-content")
    #                ,options = list(minHeight = 500, minWidth=500  ))
    # 
    shinyjqui::jqui_draggable(paste0("#", ns("window")," .modal-content")
                   , options = list(revert=TRUE) 
    )
    
    
    
    output$bsmodal_ui <- renderUI({
      
      if(is.null(width))
        width <- 'small'
      tagList(
        tags$head(tags$style(paste0(".modal-dialog { width:",width," }"))),
        tags$head(tags$style("#test .modal-dialog {width: fit-content !important;}")),
        actionButton(ns("openModalBtn"), "",
                     icon('chart-bar', lib = "font-awesome"),
                     class="btn-success"),
        #div(id = 'test',
            shinyBS::bsModal(ns("window"),
                             title = title,
                             trigger = ns("openModalBtn"),
                             uiContent
                             )
          #  )
      )
      
    })
    
    
  })
  
  
}