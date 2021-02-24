btn_style <- "display:inline-block; vertical-align: middle; padding: 7px"

#' @export
#' 
mod_Protein_ui <- function(id){
  ns <- NS(id)
  uiOutput(ns('ui'))
}


#' @export
#' 
#' @import shiny
#' @import shinyjs
#' 
mod_Protein_server <- function(id,
                               dataIn = NULL,
                               tag.enabled = reactive({TRUE})){
  
  config <- reactiveValues(
    name = 'Protein',
    steps = c('Description', 'Normalization'),
    mandatory = c(T, F)
    
  )
  ###-------------------------------------------------------------###
  ###                                                             ###
  ### ------------------- MODULE SERVER --------------------------###
  ###                                                             ###
  ###-------------------------------------------------------------###
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    #source(file.path('.', 'code_for_pipeline.R'), local=TRUE)$value
    
    
    
    
    
    output$ui <- renderUI({
      tagList(
        shinyjs::useShinyjs(),
        fluidRow(
          column(width=2, 
                 wellPanel(
                   div(style = "padding: 10px",
                       div(style = btn_style,
                           shinyjs::disabled(
                             actionButton(ns("prevBtn"), "<<",
                                          class = PrevNextBtnClass,
                                          style='padding:4px; font-size:80%')
                           ),
                           actionButton(ns("rstBtn"), "Reset",
                                        class = redBtnClass,
                                        style='padding:4px; font-size:80%')
                       ),
                       div(style = btn_style,
                           actionButton(ns("nextBtn"),">>",
                                        class = PrevNextBtnClass,
                                        style='padding:4px; font-size:80%')
                       ),
                       mod_timeline_v_ui(ns('timeline'))
                   )
                 )),
          column(width=10,
                 style=" padding-left: 20px;",
                 wellPanel(
                   div(id = ns('Screens'),
                       uiOutput(ns('SkippedInfoPanel')),
                       uiOutput(ns('EncapsulateScreens'))
                       
                   ),
                   wellPanel(title = 'foo',
                             tagList(
                               h3('module process'),
                               uiOutput(ns('show_Debug_Infos'))
                             )
                   )
                 ))
          
        )
      )
    })
    
    
    output$SkippedInfoPanel <- renderUI({
      #if (verbose) cat(paste0(class(self)[1], '::output$SkippedInfoPanel from - ', self$id, '\n\n'))
      
      current_step_skipped <- rv.process$status[rv.process$current.pos] == global$SKIPPED
      entire_process_skipped <- isTRUE(sum(rv.process$status) == global$SKIPPED * rv.process$length)
      req(current_step_skipped)
      
      
      if (entire_process_skipped){
        # This case appears when the process has been skipped from the
        # pipleine. Thus, it is not necessary to show the info box because
        # it is shown below the timeline of the pipeline
      } else {
        txt <- paste0("This ", rv.process$config$type, " is skipped so it has been disabled.")
        wellPanel(
          style = "background-color: #7CC9F0; opacity: 0.72; padding: 0px; align: center; vertical-align: center;",
          height = 100,
          width=300,
          align="center",
          p(style = "color: black;", paste0('Info: ',txt))
        )
      }
    })
    
    
    output$EncapsulateScreens <- renderUI({
      tagList(
        lapply(1:length(rv.process$config$ll.UI), function(i) {
          if (i==1)
            div(id = ns(rv.process$config$steps[i]),
                class = paste0("page_", id),
                rv.process$config$ll.UI[[i]]
            )
          else
            shinyjs::hidden(
              div(id =  ns(rv.process$config$steps[i]),
                  class = paste0("page_", id),
                  rv.process$config$ll.UI[[i]]
              )
            )
        }
        )
      )
      
      
    })
    
    
    
    output$show_Debug_Infos <- renderUI({
      tagList(
        uiOutput(ns('show_tag_enabled')),
        fluidRow(
          column(width=2,
                 tags$b(h4(style = 'color: blue;', paste0("Global input of ", rv.process$config$type))),
                 uiOutput(ns('show_dataIn'))),
          column(width=2,
                 tags$b(h4(style = 'color: blue;', paste0("Temp input of ", rv.process$config$type))),
                 uiOutput(ns('show_rv_dataIn'))),
          column(width=2,
                 tags$b(h4(style = 'color: blue;', paste0("Output of ", rv.process$config$type))),
                 uiOutput(ns('show_rv_dataOut'))),
          column(width=4,
                 tags$b(h4(style = 'color: blue;', "status")),
                 uiOutput(ns('show_status')))
        )
      )
    })
    
    ###########---------------------------#################
    output$show_dataIn <- renderUI({
      if (verbose) cat(paste0('::output$show_dataIn from - ', id, '\n\n'))
      req(dataIn())
      tagList(
        # h4('show dataIn()'),
        lapply(names(dataIn()), function(x){tags$p(x)})
      )
    })
    
    output$show_rv_dataIn <- renderUI({
      if (verbose) cat(paste0('::output$show_rv_dataIn from - ', id, '\n\n'))
      req(rv.process$dataIn)
      tagList(
        # h4('show dataIn()'),
        lapply(names(rv.process$dataIn), function(x){tags$p(x)})
      )
    })
    
    output$show_rv_dataOut <- renderUI({
      if (verbose) cat(paste0('::output$show_rv_dataOut from - ', id, '\n\n'))
      tagList(
        #h4('show dataOut$value'),
        lapply(names(dataOut$value), function(x){tags$p(x)})
      )
    })
    
    
    output$show_status <- renderUI({
      tagList(lapply(1:rv.process$length, 
                     function(x){
                       color <- if(rv.process$tl.tags.enabled[x]) 'black' else 'lightgrey'
                       if (x == rv.process$current.pos)
                         tags$p(style = paste0('color: ', color, ';'),
                                tags$b(paste0('---> ', rv.process$config$steps[x], ' - ', GetStringStatus(rv.process$status[[x]])), ' <---'))
                       else 
                         tags$p(style = paste0('color: ', color, ';'),
                                paste0(rv.process$config$steps[x], ' - ', GetStringStatus(rv.process$status[[x]])))
                     }))
    })
    
    output$show_tag_enabled <- renderUI({
      tagList(
        p(paste0('tl.tags.enabled = ', paste0(as.numeric(rv.process$tl.tags.enabled), collapse=' '))),
        p(paste0('enabled() = ', as.numeric(tag.enabled())))
      )
    })
    
    
   
    reactive({dataOut})
    
    
  }
  )
}