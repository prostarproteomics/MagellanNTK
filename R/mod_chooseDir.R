

chooseDir_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyDirButton(ns("dir"), "Input directory", "Upload"),
    verbatimTextOutput(ns("dir"), placeholder = TRUE) 
  )
}
    


chooseDir_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
shinyDirChoose(input, 'dir', roots = c(home = '~'))

global <- reactiveValues(datapath = getwd())

dir <- reactive(input$dir)

output$dir <- renderText({
  global$datapath
})

observeEvent(ignoreNULL = TRUE,
             eventExpr = {
               input$dir
             },
             handlerExpr = {
               if (!"path" %in% names(dir())) return()
               home <- normalizePath("~")
               global$datapath <-
                 file.path(home, paste(unlist(dir()$path[-1]), collapse = .Platform$file.sep))
             })

reactive(global$datapath)

})
}



chooseDir<- function(){
  ui <- chooseDir_ui('test')
  
  server <- function(input, output, session) 
    chooseDir_server('test')
  
  shinyApp(ui, server)
  
}

chooseDir()
