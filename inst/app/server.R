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
#' @import shiny
#' @import shinyjs
#' 
#' @noRd
server_MagellanNTK <- shinyServer( 
  
    function(input, output, session ) {
      
      funcs <- list(convert = "MagellanNTK::convert",
        open_dataset = "MagellanNTK::open_dataset",
        open_demoDataset = "MagellanNTK::open_demoDataset",
        view_dataset = "omXplore::view_dataset",
        infos_dataset = "MagellanNTK::infos_dataset")
      
      #library(omXplore)
      
      observeEvent(funcs, {
        print(funcs)
        
        for(i in names(funcs))
          requireNamespace(unlist(strsplit(funcs[[i]], split='::'))[1])

        shinyjs::toggle('mainapp_module', condition = !is.null(funcs))
        mainapp_server('mainapp_module', funcs)

      })
      
      
    
 
    }
)


