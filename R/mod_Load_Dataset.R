#' @title mod_Debug_Infos
#' 
#' @description  A shiny Module.
#' @param id xxx
#'
#' @rdname mod_Debug_Infos
#' 
#' @export
#' 
mod_Load_Dataset_ui <- function(id){
  ns <- NS(id)
  tagList(xxx)
}

# Module Server
#' @title xxx
#' 
#' @description xxx
#' 
#' 
#' @param id xxx
#' 
#' @export
#' @return xxx 
#' 
#' @examples 
#' \dontrun{
#' 
#' ui <- fluidPage(
#'   mod_format_DT_ui('tbl')
#' )
#' server <- function(input, output){
#'   mod_format_DT_server(id = 'tbl',table2show = reactive({head(iris)}))
#' }
#' shinyApp(ui, server)
#' }
#' 
#' @rdname mod_Debug_Infos
#' 
#' @export
#' 
mod_Load_Dataset_server <- function(id){
  
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    
    
  })
  
}

## To be copied in the UI
# mod_format_DT_ui("format_DT_ui_1")

## To be copied in the server
# callModule(mod_format_DT_server, "format_DT_ui_1")

