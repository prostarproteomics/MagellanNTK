btn_style <- "display:inline-block; vertical-align: middle; padding: 7px"
#source(file.path('.', 'mod_timeline_h.R'), local=TRUE)$value
#source(file.path('.', 'mod_timeline_v.R'), local=TRUE)$value

#' @export
#'
mod_Protein_Normalization_ui <- function(id){
  ns <- NS(id)
  uiOutput(ns('ui'))
}


#' @export
#' 
#' @import shiny
#' @import shinyjs
#' 
mod_Protein_Normalization_server <- function(id,
                               dataIn = NULL,
                               tag.enabled = reactive({TRUE}),
                               reset = reactive({FALSE}),
                               position = reactive({NULL}),
                               skipped = reactive({NULL})
                               ){
  
  config <- reactiveValues(
    name = 'Protein_Normalization',
    steps = c('Description', 'Step1', 'Step2', 'Step3'),
    mandatory = c(T, F, T, T)
  )
  
  # Define default selected values for widgets
  widgets.default.values <- list(
    select1 =1,
    select2 = NULL,
    select3 = 1,
    select2_1 = 1,
    select2_2 = 1
  )
  
  ###-------------------------------------------------------------###
  ###                                                             ###
  ### ------------------- MODULE SERVER --------------------------###
  ###                                                             ###
  ###-------------------------------------------------------------###
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    source(file.path('.', 'code_for_process.R'), local=TRUE)$value
   
    rv.widgets <- reactiveValues(
      select1 = widgets.default.values$select1,
      select2 = widgets.default.values$select2,
      select3 = widgets.default.values$select3,
      select2_1 = widgets.default.values$select2_1,
      select2_2 = widgets.default.values$select2_2
    )

###-----------------------------------------------------------------------------------------------------

    
    output$ui <- renderUI({
      tagList(
        shinyjs::useShinyjs(),
        # div(style = "vertical-align: middle; padding: 10px; display: flex;",
        #     div(style = "vertical-align: middle; ",
        #         shinyjs::disabled(
        #           actionButton(ns("prevBtn"), "<<",
        #                        class = PrevNextBtnClass,
        #                        style='padding:4px; font-size:80%')
        #         )),
        #     div(style = "vertical-align: middle; ",
        #         actionButton(ns("rstBtn"), "Reset",
        #                      class = redBtnClass,
        #                      style='padding:4px; font-size:80%')
        #     ),
        #     div(style = "vertical-align: middle; ",
        #         mod_timeline_h_ui(ns('timeline'))
        #     ),
        #     div(style = "vertical-align: middle; ",
        #         actionButton(ns("nextBtn"),">>",
        #                      class = PrevNextBtnClass,
        #                      style='padding:4px; font-size:80%')
        # 
        # )),
        fluidRow(style="display: flex;
 align-items: center;
 justify-content: center;",
                 column(width=1, shinyjs::disabled(
                   actionButton(ns("prevBtn"), "<<",
                                class = PrevNextBtnClass,
                                style='font-size:80%')
                 )),
                 column(width=1, actionButton(ns("rstBtn"), "Reset",
                                              class = redBtnClass,
                                              style='font-size:80%')),
                 column(width=9, mod_timeline_h_ui(ns('timeline'))),
                 column(width=1, actionButton(ns("nextBtn"),">>",
                                              class = PrevNextBtnClass,
                                              style='font-size:80%'))
        ),
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
    
    

### ----------------------------------------------------------------------------------------------------

###### ------------------- Code for Description (step 0) -------------------------    #####
output$Description <- renderUI({
  rv.process$tl.tags.enabled

  wellPanel(
    tagList(
      includeMarkdown( system.file("app/md", paste0(rv.process$config$name, ".md"), package="Magellan")),
      uiOutput(ns('datasetDescription')),
      if (isTRUE(rv.process$tl.tags.enabled['Description']))
        actionButton(ns('btn_validate_Description'), 
                     paste0('Start ', rv.process$config$name),
                     class = btn_success_color)
      else
      shinyjs::disabled(
        actionButton(ns('btn_validate_Description'), 
                   paste0('Start ', rv.process$config$name),
                   class = btn_success_color)
    )
    )
  )
})

observeEvent(input$btn_validate_Description, ignoreInit = T, ignoreNULL=T, {
  InitializeDataIn()
  ValidateCurrentPos()
})


###### ------------------- Code for step 1 -------------------------    #####

observeEvent(input$btn_validate_Step1, ignoreInit = T, {
  # Add your stuff code here
  ValidateCurrentPos()
})


observeEvent(input$select1,{rv.widgets$select1 <- input$select1})
observeEvent(input$select2,{rv.widgets$select2 <- input$select2})
observeEvent(input$select3,{rv.widgets$select3 <- input$select3})
observeEvent(input$select2,{rv.widgets$select2_1 <- input$select2_1})
observeEvent(input$select3,{rv.widgets$select2_2 <- input$select2_2})

# observeEvent(lapply(names(reactiveValuesToList(rv.widgets)), function(x){ input[[x]]}), ignoreInit = TRUE, {
#   #browser()
#   lapply(names(reactiveValuesToList(rv.widgets)), function(x){ 
#     rv.widgets[[x]] <- input[[x]]
#   })
# })


output$test1 <-renderUI({
  #rv.process$tl.tags.enabled
  rv.widgets$select1
    if (rv.process$tl.tags.enabled['Step1'])
    selectInput(ns('select1'), 'Select 1 in renderUI',
                choices = 1:4,
                selected = rv.widgets$select1,
                width = '150px')
  else
    shinyjs::disabled(
      selectInput(ns('select1'), 'Select 1 in renderUI',
                  choices = 1:4,
                  selected = rv.widgets$select1,
                  width = '150px')
      )
})



output$test2 <-renderUI({

  rv.process$tl.tags.enabled
  if (rv.process$tl.tags.enabled['Step1'])
    selectInput(ns('select2'), 'Select 2 in renderUI', 
                choices = 1:3,
                selected = rv.widgets$select2,
                width = '150px')
  else
    shinyjs::disabled(
      selectInput(ns('select2'), 'Select 2 in renderUI', 
                  choices = 1:4,
                  selected = rv.widgets$select2,
                  width = '150px')
    )
})




# ------------------------ STEP 1 : UI ------------------------------------
output$Step1 <- renderUI({
  #rv.process$tl.tags.enabled
  name <- 'Step1'
  wellPanel(id = ns('toto'),
    actionButton(ns('btn1'), 'Btn 1'),
    tagList(
      div(id=ns('Step1a'),
          div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
              uiOutput(ns('test1'))
          ),
          div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
              uiOutput(ns('test2'))
          ),
          div(style="display:inline-block; vertical-align: middle; padding-right: 40px;",
              if (rv.process$tl.tags.enabled['Step1'])
                selectInput(ns('select3'), 'Select step 3', 
                          choices = 1:3, 
                          selected = rv.widgets$select3,
                          width = '150px')
              else
                shinyjs::disabled(
                  selectInput(ns('select3'), 'Select step 3', 
                              choices = 1:5, 
                              selected = rv.widgets$select3,
                              width = '150px')
                )
          ),
          div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
              if (rv.process$tl.tags.enabled['Step1'])
                actionButton(ns(paste0('btn_validate_', name)), 
                             'Perform',
                             class = btn_success_color)
              else
                shinyjs::disabled(
                  actionButton(ns(paste0('btn_validate_', name)),
                               'Perform',
                               class = btn_success_color)
                  )
           )
      )
    )
  )
})

#------------- Code for step 2 ---------------

observeEvent(input$btn_validate_Step2, ignoreInit = T, {
  # Add your stuff code here
  ValidateCurrentPos()
})

output$select2_1_UI <-renderUI({
  rv.process$tl.tags.enabled
  if (rv.process$tl.tags.enabled['Step2'])
      selectInput(ns('select2_1'), 'Select 2_1 in renderUI', 
              choices = 1:3, 
              width = '150px')
  else
    shinyjs::disabled(
      selectInput(ns('select2_1'), 'Select 2_1 in renderUI', 
                  choices = 1:3, 
                  width = '150px')
    )
})

output$Step2 <- renderUI({
  rv.process$tl.tags.enabled
  name <- 'Step2'
  wellPanel(
    tagList(
      div(id=ns(name),
          div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
              uiOutput(ns('select2_1_UI'))
          ),
          div(style="display:inline-block; vertical-align: middle; padding-right: 40px;",
              if (rv.process$tl.tags.enabled['Step2'])
                selectInput(ns('select2_2'), 'Select 2_2', 
                          choices = 1, 
                          width = '150px')
              else
                shinyjs::disabled(
                  selectInput(ns('select2_2'),
                              'Select 2_2', 
                              choices = 1, 
                              width = '150px')
                  )
          ),
          div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
              if (rv.process$tl.tags.enabled['Step2'])
                actionButton(ns(paste0('btn_validate_', name)), 
                             'Perform',
                             class = btn_success_color)
              else
                shinyjs::disabled(
                actionButton(ns(paste0('btn_validate_', name)), 
                             'Perform',
                             class = btn_success_color)
                )
          )
      )
    )
  )
})


#------------- Code for step 3 ---------------

output$Step3 <- renderUI({
  rv.process$tl.tags.enabled
  tagList(
    h3('Step 3'),
    if (rv.process$tl.tags.enabled['Step3'])
      actionButton(ns('btn_validate_Step3'), 
                   'Perform',
                   class = btn_success_color)
    else
      shinyjs::disabled(
        actionButton(ns('btn_validate_Step3'), 
                     'Perform',
                     class = btn_success_color)
        )
  )
})


observeEvent(input$btn_validate_Step3, ignoreInit = T, {
  # Add your stuff code here
  rv.process$dataIn <- AddItemToDataset(rv.process$dataIn, rv.process$config$name)
  ValidateCurrentPos()
})




reactive({dataOut})


  }
)
}