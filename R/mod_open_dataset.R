#' @title Change the default functions in `MagellanNTK`
#' 
#' @description This module allows to change
#' 
#' @param id xxx
#' 
#' @name generic_mod_open_dataset
#' 
#' @examplesIf interactive()
#' shiny::runApp(open_dataset())
#' 
#' 
#' 
NULL




#' @export 
#' @rdname generic_mod_open_dataset
#' @importFrom shiny NS tagList 
#' @import shinyjs
#' 
open_dataset_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3(style="color: blue;", '-- Default open dataset module --'),
      shinyjs::useShinyjs(),
      tagList(
        selectInput(ns('chooseSource'), 'Dataset source',
          choices = c('Custom dataset' = 'customDataset',
            'package dataset' = 'packageDataset'),
          width = '200px'),

          uiOutput(ns('customDataset_UI')),
          uiOutput(ns('packageDataset_UI'))
      ),
    uiOutput(ns('datasetInfos_UI'))
    )
}


#' @rdname generic_mod_open_dataset
#' 
#' @export
#' @importFrom BiocGenerics get
#' @importFrom utils data
#' @importFrom shinyjs info
#' 
open_dataset_server <- function(id){
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    rv.open <- reactiveValues(
      dataRead = NULL,
      dataOut = NULL
    )

    output$packageDataset_UI <- renderUI({
      req(input$chooseSource == 'packageDataset')
        wellPanel(
          uiOutput(ns("choosePkg")),
          uiOutput(ns("chooseDemoDataset")),
          uiOutput(ns("linktoDemoPdf")),
          shinyjs::disabled(
            actionButton(ns('load_dataset_btn'), 'Load dataset', 
              class= actionBtnClass))
        )
    })
    
    
    output$customDataset_UI <- renderUI({
      req(input$chooseSource == 'customDataset')
      wellPanel(
        fileInput(ns("file"), "Open file", multiple = FALSE, width = "400px"),
        actionButton(ns('load_btn'), 'Load file')
      )
    })
    
    output$choosePkg <- renderUI({
      req(input$chooseSource == 'packageDataset')
      selectizeInput(ns("pkg"), "Choose package",
        choices = NULL,
        #selected = 'MagellanNTK',
        width='200px')
    })
    
    # GetPackagesWithDatasets <- reactive({
    #   x <- data(package = .packages(all.available = TRUE))$results
    #   dat <- x[which(x[,'Item'] != ''), c('Package', 'Item')]
    #   unique(dat)
    # })
    
    observeEvent(input$pkg, {

      withProgress(message = "", detail = "", value = 0, {
        incProgress(0.5, detail = "Building package list...")
        x <- data(package = .packages(all.available = TRUE))$results
        dat <- x[which(x[,'Item'] != ''), c('Package', 'Item')]
      })
      
      
    updateSelectizeInput(session, 'pkg', 
      choices = c('MagellanNTK', unique(dat)[, "Package"]), 
      selected = 'MagellanNTK',
      server = TRUE)

    }, once = TRUE)
    
    
    
    ## function for demo mode
    output$chooseDemoDataset <- renderUI({
      req(input$chooseSource == 'packageDataset')
      req(input$pkg)
      pkgs.require(input$pkg)
      
      selectInput(ns("demoDataset"),
        "Demo dataset",
        choices = utils::data(package=input$pkg)$results[,"Item"],
        selected = character(0),
        width='200px')
    })
    
    
    
    observeEvent(req(input$demoDataset != 'None'), {
      nSteps <- 1
      withProgress(message = '',detail = '', value = 0, {
        incProgress(1/nSteps, detail = 'Loading dataset')
        utils::data(list = input$demoDataset, package = input$pkg)
        rv.open$dataRead <- BiocGenerics::get(input$demoDataset)
        # if (!inherits(rv.open$dataRead, "QFeatures")) {
        #   shinyjs::info("Warning : this file is not a QFeatures file ! 
        #               Please choose another one.")
        #   return(NULL)
        # }
        shinyjs::toggleState('load_dataset_btn', condition = !is.null(rv.open$dataRead))
      }) # End withProgress
    }) # End observeEvent
    
    
    observeEvent(input$load_dataset_btn, {
      rv.open$dataOut <- rv.open$dataRead
    })
    
    output$linktoDemoPdf <- renderUI({
      req(input$demoDataset)
      req(input$chooseSource == 'packageDataset')
      
    })
    
    
    # Part of open custom dataset
    ## -- Open a MSnset File --------------------------------------------
    observeEvent(input$load_btn, ignoreInit = TRUE, {
      input$file
      rv.open$dataRead <- NULL
      tryCatch({
        # Try with readRDS()
        rv.open$dataRead <- readRDS(input$file$datapath)
      },
        warning = function(w) {
          return(NULL)
        },
        error = function(e) {
          return(NULL)
        }
      )
      
      if (is.null(rv.open$dataRead)){
        rv.open$dataRead <- tryCatch({
          load(file = input$file$datapath)
          name <- unlist(strsplit(input$file$name, split='.', fixed = TRUE))[1]
          get(name)
        },
          warning = function(w) {
            return(NULL)
          },
          error = function(e) {
            return(NULL)
          }
        )
      }
      
      rv.open$dataOut <- rv.open$dataRead
      
      # if (is.Magellan.compliant(rv.open$dataRead)){
      #   if (inherits(rv.open$dataRead, 'list'))
      #     rv.open$dataOut <- rv.open$dataRead
      #   else 
      #     rv.open$dataOut <- list(original = rv.open$dataRead)
      # } else {
      #   shinyjs::info("Dataset not compatible with MagellanNTK")
      # }
    })
    
    
    
    output$datasetInfos_UI <- renderUI({
      req(rv.open$dataOut)
        print(paste0('Dataset loaded'))
      })

    reactive({rv.open$dataOut })
  })
  
  
}



#' @export
#' @rdname generic_mod_open_dataset
#' 
#' 
open_dataset <- function(){

ui <- open_dataset_ui("demo")


server <- function(input, output, session) {
  rv <- reactiveValues(
    obj = NULL
  )
  
  rv$obj <- open_dataset_server("demo")

}

app <- shinyApp(ui = ui, server = server)
}
