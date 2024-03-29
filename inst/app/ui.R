library(shinydashboard)
library(shinyjs)



#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboardPlus
#' @import shinydashboard
#' @noRd
#' @export
#' 
ui_MagellanNTK <- shinyUI(
    tagList(

        #launchGA(),
        shinyjs::useShinyjs(),
        shinyjs::extendShinyjs(text = "shinyjs.resetProstar = function() {history.go(0)}",
            functions = c("resetProstar")),
        
        #theme = "css/ceruleanProstar.css",
        #theme = shinythemes::shinytheme("cerulean"),
        
        titlePanel("", windowTitle = "Prostar"),
        #hidden(div(id = 'div_mainapp_module',
          mainapp_ui('mainapp_module')
  #)

        
    )
)

