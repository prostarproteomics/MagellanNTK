# Module UI

#' @title   mod_insert_md_ui and mod_insert_md_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param url internal
#'
#' @rdname mod_insert_md
#' 
#' @examples
#' if(interactive()){
#' base <- system.file('app/md', package = 'MagellanNTK')
#' url <- paste0(base, "presentation.Rmd")
#' shiny::runApp(insert_md(url))
#' }
#' 
#'


#' @rdname mod_insert_md
#' @export 
#' @importFrom shiny NS tagList uiOutput htmlOutput
#' 
insert_md_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('openURLButton_UI')),
    htmlOutput(ns("insertMD"))
  )
}



#' @importFrom shiny tagList uiOutput htmlOutput observeEvent
#'  tagList uiOutput htmlOutput actionLink- req includeMarkdown p
#' @importFrom shinyjs info
#' 
#' @rdname mod_insert_md
#' @export
insert_md_server <- function(id,
                             url,
                             link_URL = NULL){
  
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    output$openURLButton_UI <- renderUI({
      req(!is.null(link_URL))
      shiny::actionLink(inputId = ns("openURL"), 
                        label = "Open in new tab")
    })
    
    
    observeEvent(input$openURL,{
      browseURL(link_URL)
    })
    
    output$insertMD <- renderUI({
      tryCatch(
        {
          includeMarkdown(readLines(url))
        }
        , warning = function(w) {
          tags$p("URL not found<br>",conditionMessage(w))
        }, error = function(e) {
          shinyjs::info(paste("URL not found:", conditionMessage(e), sep=" "))
        }, finally = {
          #cleanup-code 
        })
      
    })
    
    
  })
  
}



#' @export
#' @importFrom shiny shinyApp fluidPage
#' @rdname insert_md

insert_md <- function(url){
  ui <- fluidPage(
    insert_md_ui('tree')
  )


  server <- function(input, output) {
  
  insert_md_server('tree', url)
}

app <- shinyApp(ui = ui, server = server)
}

