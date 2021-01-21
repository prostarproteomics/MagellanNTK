#' @title xxx
#' 
#' @description
#' xxxxx
#' 
#' @author Enora Fremy, Samuel Wieczorek
#' 
#' @export
#' 
TimelineDraw <- R6::R6Class(
  "TimelineDraw",
  private=list(
    ns = NULL,
    length = NULL,
    style = NULL,
    mandatory = NULL,
    orientation = NULL
  ),

  public = list(
    id = NULL,

    initialize = function(id, mandatory, orientation = 'h') {
      self$id <- id
      private$ns <- NS(self$id)
      private$mandatory <- mandatory
      private$length <- length(private$mandatory)
      private$orientation = orientation
      },

    
    ui = function() {
      # fpath <- system.file("app/www/sass", 
      #                      paste0(private$orientation, "_timeline.sass"), 
      #                      package="Magellan")
      fpath <- paste0("../../inst/app/www/sass/",private$orientation, "_timeline.sass")
      tagList(
        shinyjs::inlineCSS(sass::sass(sass::sass_file(fpath))),
        htmlOutput(private$ns(paste0('show_', private$orientation, '_TL')))
        )
      },
    
    server = function(status, position, enabled) {
      
      moduleServer(self$id, function(input, output, session) {

        
        UpdateClassesForTL <- reactive({
          tl_status <- rep('undone', private$length)
          tl_status[which(private$mandatory)] <- 'mandatory'
          tl_status[which(unlist(status()) == global$VALIDATED)] <- 'completed'
          tl_status[which(unlist(status()) == global$SKIPPED)] <- 'skipped'
          
          for (i in 1:length(enabled()))
            if (!enabled()[i])
              tl_status[i] <- paste0(tl_status[i], 'Disabled')
          
          tl_status[position()] <- paste0(tl_status[position()], ' active')
          
          tl_status
        })
        
        output$show_h_TL <- renderText  ({
          
          txt <- "<ul><div class='timeline' id='timeline'>"
          for (i in 1:private$length){
            txt <- paste0(txt,
                          "<li class='li ",
                          UpdateClassesForTL()[i],
                          "'><div class='timestamp'></div><div class='status'><h4>", 
                          names(private$mandatory)[i],
                          "</h4></div></li>")
          }
          
          txt <- paste0(txt,"</div></ul>")
          txt
        })
        
        
        output$show_v_TL <- renderUI  ({
          tl_status <- rep('undone', private$length)
          tl_status[which(private$mandatory)] <- 'mandatory'
          tl_status[which(unlist(status()) == global$VALIDATED)] <- 'completed'
          tl_status[which(unlist(status()) == global$SKIPPED)] <- 'skipped'
          
          for (i in 1:length(enabled()))
            if (!enabled()[i])
              tl_status[i] <- paste0(tl_status[i], 'Disabled')
          
          print(tl_status)
          active  <- rep('', private$length)
          active[position()] <- 'active'
          
          # tags$ul(
          #   tags$li(tags$a( class="undone active", "undone active")),
          #   tags$li(tags$a( class="undone", "undone")),
          #   tags$li(tags$a( class="undoneDisabled", "undone Disabled")),
          #   tags$li(tags$a( class="mandatory", "mandatory")),
          #   tags$li(tags$a( class="mandatoryDisabled", "mandatory Disabled")),
          #   tags$li(tags$a( class="completed", "completed")),
          #   tags$li(tags$a( class="completedDisabled", "completed Disabled")),
          #   tags$li(tags$a( class="skipped", "skipped")),
          #   tags$li(tags$a( class="skippedDisabled", "skipped Disabled"))
          # )
          
          tags$ul(
            tags$li(tags$a( class=UpdateClassesForTL()[1], "Step 1")),
            tags$li(tags$a( class=UpdateClassesForTL()[2], "Step 2")),
            tags$li(tags$a( class=UpdateClassesForTL()[3], "Step 3")),
            tags$li(tags$a( class=UpdateClassesForTL()[4], "Step 4"))
          )
          

        })
        
        
        
        # output$show_v_TL <- renderUI({
        #   
        #  # tl_status <- rep('sub_box', private$length)
        #   
        #   tl_status <- rep('undone', private$length)
        #   tl_status[which(private$mandatory)] <- 'mandatory'
        #   tl_status[which(unlist(status()) == global$VALIDATED)] <- 'completed'
        #   tl_status[which(unlist(status()) == global$SKIPPED)] <- 'skipped'
        #   
        #   for (i in 1:length(enabled()))
        #     if (!enabled()[i])
        #       tl_status[i] <- paste0(tl_status[i], 'Disabled')
        #   
        #   active  <- rep('', private$length)
        #   active[position()] <- 'active'
        #   
        #  txt <-  tags$div(class="box",
        #            tags$div(class=paste0('sub_box',' ', tl_status[1],' ', active[1]),
        #                     p('Filtration')
        #            ),
        #            tags$div(class=paste0('sub_box',' ', tl_status[2],' ', active[2]),
        #                     p('Normalization')
        #            ),
        #            tags$div(class=paste0('sub_box',' ', tl_status[3],' ', active[3]),
        #                     p('Imputation')
        #            ),
        #            tags$div(class=paste0('sub_box',' ', tl_status[4],' ', active[4]),
        #                     p('Aggregation')
        #            )
        #   )
        #  
        #  print(txt)
        #  txt
        # })
        }
      )
      }
  )
)