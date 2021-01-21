TimelineDraw <- R6::R6Class(
  "TimelineDraw",
  private=list(
    ns = NULL,
    length = NULL,
    style = NULL,
    mandatory = NULL,
    
    
    BuildTimeline2 = function(status, pos, enabled){
      
      tl_status <- rep('undone', private$length)
      tl_status[which(private$mandatory)] <- 'mandatory'
      tl_status[which(unlist(status) == global$VALIDATED)] <- 'completed'
      tl_status[which(unlist(status) == global$SKIPPED)] <- 'skipped'
      
      for (i in 1:length(enabled))
        if (!enabled[i])
          tl_status[i] <- paste0(tl_status[i], 'Disabled')
      
      active  <- rep('', private$length)
      active[pos] <- 'active'
      
      txt <- "<ul class='timeline' id='timeline'>"
      for (i in 1:private$length){
        txt <- paste0(txt,
                      "<li class='li ",
                      tl_status[i],
                      " ",
                      active[i],
                      "'><div class='timestamp'></div><div class='status'><h4>", 
                      names(private$mandatory)[i],
                      "</h4></div></li>")
      }
      
      txt <- paste0(txt,"</ul>")
      print(txt)
      txt
    }
  ),
  public = list(
    id = NULL,
    
    initialize = function(id, mandatory, style=2) {
      self$id <- id
      private$style <- style
      private$ns <- NS(self$id)
      private$mandatory <- mandatory
      private$length <- length(private$mandatory)
    },
    
    
    ui = function() {
      # browser()
      fpath <- system.file("www", "style6.sass")
      tagList(
        shinyjs::inlineCSS(sass::sass(sass::sass_file(fpath))),
        uiOutput(private$ns('show_TL'))
      )
    },
    
    server = function(status, position, enabled) {
      
      moduleServer(self$id, function(input, output, session) {
        
        output$show_TL <- renderUI({
          HTML(private[[paste0('BuildTimeline', private$style)]](status(), position(), enabled()))
        })
      }
      )
    }
  )
)