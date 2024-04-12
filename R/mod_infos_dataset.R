#' @title mod_open_demo_dataset_ui and mod_open_demo_dataset_server
#' 
#' @description A shiny Module.
#' 
#' @param id xxx
#' @param obj xxx
#' 
#' @name infos_dataset
#' 
#' @examplesIf interactive()
#'   data(Exp1_R25_prot, package='DaparToolshedData')
#'   shiny::runApp(infos_dataset(Exp1_R25_prot))
#' 
#' 
NULL




#' @export 
#' @rdname infos_dataset
#' @importFrom shiny NS tagList uiOutput fluidRow h3 br column
#' @import shinyjs
#' 
infos_dataset_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3(style="color: blue;", 'Default info dataset module'),
    uiOutput(ns('title')),
    uiOutput(ns('choose_SE_ui')),
    uiOutput(ns('show_SE_ui'))
      )
}


#' @rdname infos_dataset
#' 
#' @export
#' @importFrom BiocGenerics get
#' @importFrom utils data
#' @importFrom shinyjs info
#' @importFrom shiny moduleServer observe req reactive 
#' 
infos_dataset_server <- function(
    id,
  obj = reactive({NULL})){
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    
    
    output$choose_SE_ui <- renderUI({
      req(obj())
      selectInput(ns("selectInputSE"),
        "Select a dataset for further information",
        choices = c("None", names(obj()))
      )
    })
    

    output$show_SE_ui <- renderUI({
      req(input$selectInputSE != 'None')
      req(obj())
      names(obj())
    })

  })
}




###################################################################
##                                                               ##
##                                                               ##
###################################################################
#' @export
#' @importFrom shiny shinyApp fluidPage
#' @rdname infos_dataset
#' 
infos_dataset <- function(obj){
  
ui <- fluidPage(
  infos_dataset_ui("infos")
)


server <- function(input, output, session) {
  infos_dataset_server("infos", obj = reactive({obj}))
}

app <- shinyApp(ui = ui, server = server)
}
