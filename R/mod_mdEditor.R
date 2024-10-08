#' @title Markdown editor module
#'
#' @description Displays of formatted modal-dialog with 'Cancel' and 
#' 'Ok' buttons.
#' 
#' @param id A `character(1)` which is the id of the instance of the module
#'
#' @name mdEditor
#' 
#' 
#' @examples
#' \dontrun{
#' shiny::runApp(mdEditor())
#' }
#' 
NULL


#' @rdname mdEditor
#' @export
#'
mdEditor_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    
  tagList(
    shinyjs::useShinyjs(),
    uiOutput(ns('mdEditorLayout_ui')),
    #uiOutput(ns('mdEditor_ui')),
    shinyjs::hidden(uiOutput(ns('mdEditorPanels_ui'))),
    shinyjs::hidden(uiOutput(ns('mdEditorTabs_ui'))),
  )
  )
}


#' @rdname mdEditor
#' @importFrom shinyjqui jqui_draggable
#'
#' @export
#'
#' @return A Shiny modal-dialog
#
#'
mdEditor_server <- function(id) { # height auto
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    dataOut <- reactiveVal(NULL)
    
    
    output$mdEditorLayout_ui <- renderUI({
      widget <- selectInput(ns('mdEditorLayout'), 
                            'Markdown editor layout',
                            choices = c('tabs', 'panels'),
                            width = '100px')
    })
    
    
    observeEvent(input$mdEditorLayout, {
      shinyjs::toggle('mdEditorPanels_ui', condition = input$mdEditorLayout == 'panels')
      shinyjs::toggle('mdEditorTabs_ui', condition = input$mdEditorLayout == 'tabs')
    })
    
    
    observeEvent(input$mdEditor, { dataOut(input$mdEditor)})
    
    output$mdEditorPanels_ui <- renderUI({
      req(input$mdEditorLayout == 'panels')
      shiny::div(
        #class = class,
        style = "margin-bottom: 15px;",
        fluidRow(
          column(width = 6,
                 shinyAce::aceEditor(ns("mdEditor"),
                                     value = isolate(input$mdEditor),
                                     mode = "markdown",
                                     theme = 'github',
                                     height = '300px')
          ),
          column(width = 6, uiOutput(ns('preview_ui')))
        )
      )
    })
    
    
    output$mdEditorTabs_ui <- renderUI({
      req(input$mdEditorLayout == 'tabs')
      shiny::div(
        #class = class,
        style = "margin-bottom: 15px;",
        # shiny::tags$label('label'),
        
        shiny::tabsetPanel(selected = 1,
                           type = "tabs",
                           
                           # text input:
                           shiny::tabPanel(
                             title = "Write Description md code",
                             value = 1,
                             shinyAce::aceEditor(ns("mdEditor"),
                                                 value = isolate(input$mdEditor),
                                                 mode = "markdown",
                                                 theme = 'github',
                                                 height = '300px')
                           ),
                           
                           
                           # MD preview:
                           shiny::tabPanel(
                             title = "Preview",
                             value = 2,
                             uiOutput(ns('preview_ui'))
                           )
        )
      )
    })
    
    
    output$preview_ui <- renderUI({
      req(input$mdEditor)
      wellPanel(style = "overflow-y:scroll; 
                overflow-X:scroll; 
                max-height: 300px; 
                background: white;",
                height = '100px',
                shiny::div(class = "",
                           shiny::withMathJax(
                             shiny::HTML(
                               markdown::markdownToHTML(text = input$mdEditor,
                                                        fragment.only = TRUE)
                             )
                           )
                )
      )
    })
    
    reactive({dataOut()})
  })
}



#' @export
#' @rdname mdEditor
#' @importFrom shiny shinyApp
#'
#' @export
#'
#' @return A Shiny modal-dialog
#' 
mdEditor <- function(){
  
  ui <- mdEditor_ui("tbl")

  server <- function(input, output) {
    mdEditor_server(id = "tbl")
  }
  
  app <- shiny::shinyApp(ui, server)
}
