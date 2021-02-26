#' timeline UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
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
    
#' timeline Server Function
#'
#' @noRd 
mod_timeline_v_server = function(id, 
                               config, 
                               status, 
                               position, 
                               enabled) {
    
  rv.tl <- reactiveValues(
    length = NULL
  )

  colCompletedDisabled <- "#ABDBAC"
  colCompleted <- "#07910A"
  colSkippedDisabled <- "#B3AFAB80"
  colSkipped <- "#B3AFAB"
  colMandatory <- "#D30505"
  colMandatoryDisabled <- "#D3050580"
  colUndone <- "#B3AFAB"
  colUndoneDisabled <- "#B3AFAB80"
  colLine <- "#C6C8C6"
    
  global <- list(
    VALIDATED = 1,
    UNDONE = 0,
    SKIPPED = -1
  )
    moduleServer(id, function(input, output, session) {
      ns <- session$ns
      
      observe({
        req(config)
        rv.tl$length <- length(config$steps)
      })
      
      UpdateTags <- reactive({
        tl_status <- rep('undone', rv.tl$length)
        tl_status[which(config$mandatory)] <- 'mandatory'
        tl_status[which(unlist(status()) == global$VALIDATED)] <- 'completed'
        tl_status[which(unlist(status()) == global$SKIPPED)] <- 'skipped'
        
        for (i in 1:length(enabled()))
          if (!enabled()[i])
            tl_status[i] <- paste0(tl_status[i], 'Disabled')
        
        tl_status[position()] <- paste0(tl_status[position()], ' active')
        tl_status
      })
      
      active <- reactive({
        "background-color: white;box-shadow: 0 0 0 3px black;"
      })
      
      undone <- reactive({
        paste0("background-color: white;color: black;border: 3px solid ", colUndone, ";")
      })
      
      undoneDisabled <- reactive({
        paste0("background-color: white;color: black;border: 3px solid ", colUndoneDisabled, ";")
      })
      
      mandatory <- reactive({
        paste0("background-color: white; color: black;border: 3px solid ", colMandatory, ";")
      })
      
      mandatoryDisabled <- reactive({
        paste0("background-color: white;color: black;border: 3px solid ", colMandatoryDisabled, ";")
      })
      
      completed <- reactive({
        paste0("background-color: ", colCompleted, ";border: 3px solid ", colCompleted, ";")
      })
      
      completedDisabled <- reactive({
        paste0("background-color: ", colCompletedDisabled, ";border: 3px solid ", colCompletedDisabled, ";")
      })
      
      skipped <- reactive({
      paste0("background-color: white; border: 3px dashed ", colSkipped, ";")
      })
      
      skippedDisabled <- reactive({
        paste0("background-color: white; border: 3px dashed ", colSkippedDisabled, ";")
      })
      
      
      
      
      
      
      GetStyle <- reactive({
        #browser()
        tl_status <- rep(undone(), rv.tl$length)
        tl_status[which(enabled()==1)] <- undoneDisabled()
        
        tl_status[intersect(which(config$mandatory), which(enabled()==1))] <- mandatory()
        tl_status[intersect(which(config$mandatory), which(enabled()==0))] <- mandatoryDisabled()
        
        tl_status[intersect(which(unlist(status()) == global$VALIDATED), which(enabled()==1))] <- completed()
        tl_status[intersect(which(unlist(status()) == global$VALIDATED), which(enabled()==0))] <- completedDisabled()
        
        tl_status[intersect(which(unlist(status()) == global$SKIPPED), which(enabled()==1))] <- skipped()
        tl_status[intersect(which(unlist(status()) == global$SKIPPED), which(enabled()==0))] <- skippedDisabled()
        
        # for (i in 1:length(enabled()))
        #   if (!enabled()[i])
        #     tl_status[i] <- paste0(tl_status[i], Disabled')
        # 
        tl_status[position()] <- paste0(tl_status[position()],  active())
        tl_status
      })
      
      output$show_v_TL <- renderUI  ({
        # tl_status <- rep('undone', rv.tl$length)
        # tl_status[which(config$mandatory)] <- 'mandatory'
        # tl_status[which(unlist(status()) == global$VALIDATED)] <- 'completed'
        # tl_status[which(unlist(status()) == global$SKIPPED)] <- 'skipped'
        # 
        # for (i in 1:length(enabled()))
        #   if (!enabled()[i])
        #     tl_status[i] <- paste0(tl_status[i], 'Disabled')
        # 
        # active  <- rep('', rv.tl$length)
        # active[position()] <- 'active'
#browser()
        tags$div(style='width: 150px;',
          #tags$ul(
            lapply(1:rv.tl$length, function(x){
             # tags$li(tags$p( class=UpdateTags()[x], config$steps[x]))
               tags$p(style=paste0("font-weight: 100;border: 3px solid lightgrey;border-radius: 10px;display: block;color: #000;padding: 8px 10px;margin: 10px;text-align: center;", GetStyle()[x]),
                      config$steps[x])
              # actionButton(inputId = ns(paste0('toto',x)),
              #              label = config$steps[x],
              #              style=paste0("font-weight: 100;border: 3px solid lightgrey;border-radius: 10px;display: block;color: #000;padding: 8px 10px;margin: 10px;text-align: center;", GetStyle()[x])
              #        )
              
              # actionLink(inputId = ns(paste0('toto',x)),
              #              label = config$steps[x],
              #              style=paste0("font-weight: 100;border: 3px solid lightgrey;border-radius: 10px;display: block;color: #000;padding: 8px 10px;margin: 10px;text-align: center;", GetStyle()[x])
              # )
              
              })
          #)
        )
          })
})
}

    
## To be copied in the UI
# mod_timeline_ui("timeline_ui_1")
    
## To be copied in the server
# callModule(mod_timeline_server, "timeline_ui_1")
 
