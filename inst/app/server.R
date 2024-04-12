options(shiny.maxRequestSize=300*1024^2,
        encoding = "UTF-8",
        shiny.fullstacktrace = TRUE
        )
require(compiler)
enableJIT(3)


#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @importFrom  shiny shinyServer observeEvent toggle
#' @import shinyjs
#' 
#' @noRd
server_MagellanNTK <- shiny::shinyServer( 
  
    function(input, output, session ) {
      
      shiny::observeEvent(funcs, {
        for(i in names(funcs))
          requireNamespace(unlist(strsplit(funcs[[i]], split='::'))[1])

        shinyjs::toggle('mainapp_module', condition = !is.null(funcs))
        mainapp_server('mainapp_module', funcs)
      })
    }
)


