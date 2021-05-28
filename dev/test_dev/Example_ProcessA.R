#Timeline_R6.R
Example_ProcessA = R6Class(
  "Example_ProcessA",
  inherit = Process,
  private = list(
    .config = list(name = 'ProcessA',
                   steps = c('Description', 'Step1', 'Step2', 'Step3'),
                   mandatory = c(TRUE, FALSE, TRUE, FALSE)
    )
  ),
  
  public = list(
    
    
    
    Global_server = function(session, input){
      self$rv$value.test <- 3
      self$rv$choices <- seq_len(6)
    },
    
    
    # ------------------------ DESCRIPTION : SERVER ------------------------------------
    
    Description_server = function(session, input, output){
      observeEvent(input$btn_validate_Description, ignoreInit = TRUE, ignoreNULL=TRUE, {
        cat(paste0(class(self)[1], "::observeEvent(input$btn_validate_Description from - ", self$id, '\n'))
        private$InitializeDataIn()
        self$ValidateCurrentPos()
      })
      
      output$datasetDescription <- renderUI({
        tagList(
          p(paste0('Dataset description: ', paste0(names(self$rv$temp.dataIn), collapse=", ")))
        )
      })
    },
    
    # ------------------------ DESCRIPTION : UI ------------------------------------
    
    Description_ui = function(){
      wellPanel(
        tagList(
          includeMarkdown( system.file("app/md", paste0(self$config$name, ".md"), package="Magellan")),
          uiOutput(self$ns('datasetDescription')),
          actionButton(self$ns('btn_validate_Description'), 
                       paste0('Start ', self$config$name),
                       class = btn_success_color)
        )
      )
    },
    
    # ------------------------ STEP 1 : SERVER ------------------------------------
    
    Step1_server = function(session, input, output){
      
      observeEvent(input$btn_validate_Step1, ignoreInit = TRUE, {
        # Add your stuff code here
        self$ValidateCurrentPos()
      })
      

      observeEvent(input$select1, {self$process.var$select1 <- input$select1})
      
      output$test1 <-renderUI({
        shinyjs::disabled(selectInput(self$ns('select1'), 'Select 1 in renderUI', 
                    choices = seq_len(input$btn1), 
                    width = '150px')
        )
      })
      
      observe({
        req(input$btn1)
        print('toto')
        updateSelectInput(session, self$ns('select2'), choices = seq_len(input$btn1))
        updateSelectInput(session, 'select3', choices = seq_len(input$btn1))
      })
      
      output$test2 <-renderUI({
       # shinyjs::disabled(
          selectInput(self$ns('select2'), 'Select 2 in renderUI', 
                                      choices = 1, 
                                      width = '150px')
       # )
      })
      
    },
    
    
    # ------------------------ STEP 1 : UI ------------------------------------
    Step1_ui = function(){
      name <- 'Step1'
      wellPanel(
        actionButton(self$ns('btn1'), 'Btn 1'),
        tagList(
          div(id=self$ns(name),
              div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                  uiOutput(self$ns('test1'))
                  ),
              div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                  uiOutput(self$ns('test2'))
              ),
              div(style="display:inline-block; vertical-align: middle; padding-right: 40px;",
                #  shinyjs::disabled(
                    selectInput(self$ns('select3'), 'Select step 3', 
                              choices = 1, 
                              selected = 1,
                              width = '150px')
                  #)
              ),
              div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                  #shinyjs::disabled(
                    actionButton(self$ns(paste0('btn_validate_', name)), 
                               'Perform',
                               class = btn_success_color))
         # )
          )
        )
      )
    },
    
    # ------------------------ STEP 2 : SERVER ------------------------------------
    
    Step2_server = function(session, input, output){
      ## Logics to implement: here, we must take the last data not null
      # in previous datas. The objective is to take account
      # of skipped steps
      observeEvent(input$btn_validate_Step2, ignoreInit = TRUE, {
        self$ValidateCurrentPos()
      })
    },
    
    Step2_ui = function(){
      name <- 'Step2'
      wellPanel(
        tagList(
          div(id=self$ns(name),
              div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                  tags$h3(name)),
              div(style="display:inline-block; vertical-align: middle;padding-right: 40px;",
                  shinyjs::disabled(selectInput(self$ns('select2'), 'Select step 2',
                              choices = seq_len(5),
                              selected = 1,
                              width = '150px'))),
              div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                  shinyjs::disabled(actionButton(self$ns(paste0('btn_validate_', name)), 
                               'Perform',
                               class = btn_success_color)))
          )
        )
      )
    },
    
    # ------------------------ STEP 3 : SERVER ------------------------------------
    
    Step3_server = function(session, input, output){
      
      observeEvent(input$btn_validate_Step3, ignoreInit = TRUE, {
        self$rv$dataIn <- AddItemToDataset(self$rv$dataIn, self$config$name)
        self$ValidateCurrentPos()
      })
    },
    
    # ------------------------ STEP 3 : UI ------------------------------------
    
    Step3_ui = function(){
      name <- 'Step3'
      wellPanel(
        tagList(
          div(id = self$ns(name),
              div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                  tags$h3(name)),
              div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                  shinyjs::disabled(actionButton(self$ns(paste0('btn_validate_', name)), 
                               'Validate',
                               class = btn_success_color)))
          )
        )
      )
    }
  )
)