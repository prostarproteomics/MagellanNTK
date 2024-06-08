library(shinydashboard)
library(shinyjs)



#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @importFrom shiny shinyUI tagList 
#' @import shinydashboardPlus
#' @import shinydashboard
#' @importFrom shinyjs useShinyjs extendShinyjs
#' @noRd
#' @export
#' 
ui_MagellanNTK <- shiny::shinyUI(
    shiny::tagList(
        #launchGA(),
        shinyjs::useShinyjs(),
        shinyjs::extendShinyjs(
          text = "shinyjs.resetProstar = function() {history.go(0)}",
            functions = c("resetProstar")),
        #theme = "css/ceruleanProstar.css",
        #theme = shinythemes::shinytheme("cerulean"),
        
        shiny::titlePanel("", windowTitle = "Prostar"),
        #hidden(div(id = 'div_mainapp_module',
          mainapp_ui('mainapp_module')
  #)
    )
)

