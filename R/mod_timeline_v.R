#' @title Module timeline functions
#'
#' @description A shiny Module.
#' 
#' @param id The `id` of the server
#'
#' @importFrom shiny NS tagList 
#' 
#' @rdname mod_timeline_v
#' 
#' @author Samuel Wieczorek
#' 
#' @export
#' 
mod_timeline_v_ui <- function(id){
  ns <- NS(id)
  
  #fpath <- system.file("app/www/sass", 
  #                     "v_timeline.sass", 
  #                     package="Magellan")
  div(
    
    shinyjs::useShinyjs(),
    #shinyjs::inlineCSS(sass::sass(sass::sass_file(fpath))),
    uiOutput(ns('show_v_TL'))

  )
}
    

#' @param id The `id` of the server
#' @param config A static `list` (not `reactiveList`) containing the same elements 
#' as the process module.
#' @param status A `reactive vector` which contain the status (validated, 
#' skipped or undone) of each step of the process module. Its length is equal 
#' to the number of steps.
#' @param position A `reactive integer` that reflects the position of the 
#' current (active) step.
#' @param enabled A `reactive vector` of length the number of steps and which 
#' indicate whether the step is enabled or disabled.
#' 
#' @return xxx
#' 
#' @examples
#' \dontrun{
#' ui <- fluidPage(
#' actionButton('changePos', 'Change position'),
#' mod_timeline_v_ui('TLh')
#' )
#' 
#' server <- function(input, output){
#' rv <- reactiveValues(
#' status = c(0, 1, 0, 0),
#' current.pos = 1,
#' tl.tags.enabled = c(1, 1, 1, 1),
#' position = NULL
#' )
#' config <- list(name = 'Protein_Filtering',
#' steps = c('Description', 'Step1', 'Step2', 'Step3'),
#' mandatory = c(TRUE, FALSE, TRUE, TRUE)
#' )
#' mod_timeline_v_server(id = 'TLh',
#' config = config,
#' status = reactive({rv$status}),
#' position = reactive({rv$current.pos}),
#' enabled = reactive({rv$tl.tags.enabled})
#' )
#' }
#' shinyApp(ui, server)
#' }
#'    
#' @rdname mod_timeline_v
#' 
#' @author Samuel Wieczorek
#' 
#' @export
#' 
mod_timeline_v_server <- function(id, 
                                  config, 
                                  status, 
                                  position, 
                                  enabled) {
  
  # Define colors for the different status of steps  
  colCompletedDisabled <- "#ABDBAC"
  colCompleted <- "#07910A"
  colSkippedDisabled <- "#B3AFAB80"
  colSkipped <- "#B3AFAB"
  colMandatory <- "#D30505"
  colMandatoryDisabled <- "#D3050580"
  colUndone <- "#B3AFAB"
  colUndoneDisabled <- "#B3AFAB80"
  colLine <- "#C6C8C6"
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    UpdateTags <- reactive({
      tl_status <- rep('undone', length(config@steps))
      tl_status[which(config@mandatory)] <- 'mandatory'
      tl_status[which(unlist(status()) == global$VALIDATED)] <- 'completed'
      tl_status[which(unlist(status()) == global$SKIPPED)] <- 'skipped'
        
      for (i in seq_len(length(enabled())))
        if (!enabled()[i])
          tl_status[i] <- paste0(tl_status[i], 'Disabled')
      
      tl_status[position()] <- paste0(tl_status[position()], ' active')
      tl_status
      })
    
    
    # Builds css-styles tags for the different possible status
    active <- reactive({
      "box-shadow: 0 0 0 3px black;"
      })
    
    undone <- reactive({
      paste0("background-color: white;color: black;border: 3px solid ", 
             colUndone, 
             ";")
      })
    
    undoneDisabled <- reactive({
      paste0("background-color: white;color: black;border: 3px solid ", 
             colUndoneDisabled, 
             ";")
      })
    
    mandatory <- reactive({
      paste0("background-color: white; color: black;border: 3px solid ", 
             colMandatory, 
             ";")
      })
    
    mandatoryDisabled <- reactive({
      paste0("background-color: white;color: black;border: 3px solid ", 
             colMandatoryDisabled, 
             ";")
      })
    
    completed <- reactive({
      paste0("background-color: ", 
             colCompleted, 
             ";border: 3px solid ", 
             colCompleted, 
             ";")
      })
    
    completedDisabled <- reactive({
      paste0("background-color: ", 
             colCompletedDisabled, 
             ";border: 3px solid ", 
             colCompletedDisabled, 
             ";")
      })
    
    skipped <- reactive({
      paste0("background-color: white; border: 3px dashed ", 
             colSkipped, 
             ";")
      })
    
    skippedDisabled <- reactive({
      paste0("background-color: white; border: 3px dashed ",
             colSkippedDisabled,
             ";")
    })
    
    
    
    GetStyle <- reactive({
      tl_status <- rep(undone(), length(config@steps))
      tl_status[which(enabled()==1)] <- undoneDisabled()
        
      tl_status[intersect(which(config@mandatory), which(enabled()==1))] <- mandatory()
      tl_status[intersect(which(config@mandatory), which(enabled()==0))] <- mandatoryDisabled()
        
      tl_status[intersect(which(unlist(status()) == global$VALIDATED), which(enabled()==1))] <- completed()
      tl_status[intersect(which(unlist(status()) == global$VALIDATED), which(enabled()==0))] <- completedDisabled()
        
      tl_status[intersect(which(unlist(status()) == global$SKIPPED), which(enabled()==1))] <- skipped()
      tl_status[intersect(which(unlist(status()) == global$SKIPPED), which(enabled()==0))] <- skippedDisabled()
      
       tl_status[position()] <- paste0(tl_status[position()],  active())
        tl_status
      })
    
    output$show_v_TL <- renderUI  ({
        tags$div(style='width: 150px;',
               lapply(seq_len(length(config@steps)), function(x){
             # tags$li(tags$p( class=UpdateTags()[x], config@steps[x]))
               tags$p(style=paste0("font-weight: 100;
                                   border: 3px solid lightgrey;
                                   border-radius: 10px;
                                   display: block;color: #000;
                                   padding: 8px 10px;margin: 10px;
                                   text-align: center;", 
                                   GetStyle()[x]
                                   ),
                      config@steps[x])
              # actionButton(inputId = ns(paste0('toto',x)),
              #              label = config@steps[x],
              #              style=paste0("font-weight: 100;border: 3px solid lightgrey;border-radius: 10px;display: block;color: #000;padding: 8px 10px;margin: 10px;text-align: center;", GetStyle()[x])
              #        )
              
              # actionLink(inputId = ns(paste0('toto',x)),
              #              label = config@steps[x],
              #              style=paste0("font-weight: 100;border: 3px solid lightgrey;border-radius: 10px;display: block;color: #000;padding: 8px 10px;margin: 10px;text-align: center;", GetStyle()[x])
              # )
              
              })
        )
          })
})
}

    
## To be copied in the UI
# mod_timeline_ui("timeline_ui_1")
    
## To be copied in the server
# callModule(mod_timeline_server, "timeline_ui_1")
 
