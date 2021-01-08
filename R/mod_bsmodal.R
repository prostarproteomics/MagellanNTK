# ouvre une fenetre, parametre renseigne quoi afficher dans la fenetre


#### module fenetre modal ####
mod_bsmodal_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    uiOutput(ns("bsmodal_ui"))
  )
}


#' @description
#' xxxx
#' 
#' @importFrom shinyjqui jqui_resizable jqui_draggable
#' @importFrom shinyBS bsModal
#' 
#' 
mod_bsmodal_server <- function(id,
                               dataIn = NULL,
                               title=NULL,
                               width=NULL){ #height auto
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    shinyjqui::jqui_resizable(paste0("#",ns("fenetre")," .modal-content")
                   ,options = list(minHeight = 500, minWidth=500  ))
    
    shinyjqui::jqui_draggable(paste0("#",ns("fenetre")," .modal-content")
                   , options = list(revert=TRUE) 
    )
    
    
    
    #datasets <- utils::data(package="DAPARdata2")$results[,"Item"]
    
    
    mod_all_plots_server('exemple_plot',
                         dataIn = reactive({dataIn()})
                         ) 
    title <- "Plots"
    
    
    output$bsmodal_ui <- renderUI({
      
      tagList(
        tags$head(tags$style(paste0(".modal-dialog { width:",width," }"))),
        actionButton(ns("button"), "Open Modal"),
        
        shinyBS::bsModal(ns("fenetre"),
                         title,
                         trigger = ns("button"),
                         uiOutput(ns("mod_content"))
                         )
      )
      
    })
    
    mod_UI <- mod_all_plots_ui(ns('exemple_plot'))
    
    output$mod_content <- renderUI({
      tagList(
       mod_UI  
      )
    })
    
    
    
    
  })
  
  
}