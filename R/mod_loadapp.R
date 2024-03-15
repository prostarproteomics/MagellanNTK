#' @title Load customizable functions
#' @description  A shiny Module.which allow to choose whether to use the default
#' functions (embedded in Magellan) or functions provided by external packages.
#' The names of these functions (shiny modules) are:
#' * convert(): xxx
#' * open_dataset(): xxx
#' * open_demoDataset(): xxx
#' * view_dataset(): xxxx
#' * infos_dataset(): xxx
#' 
#' Thus, MagellanNTK seraches in the global environment if any package exports
#' one or more of these functions. If so, 
#' 
#' For each of these functions, the shiny app lists all the packages that export
#' it. Once the user has make its choices, the module returns a list containing 
#' the infos.
#' 
#' @name mod_loading_page
#' 
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' 
#' @return 
#' A list where each slot is named with the customizable functions. Each slot
#' contains the code to call this function from the package the user have
#' chosen.
#' 
#' @examples 
#' if(interactive()){
#' app <- loadApp()
#' shiny::runApp(app)
#' }
#' 
#' 
NULL



#' @rdname mod_loading_page
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList
#' @importFrom shinyjs inlineCSS
#' @importFrom shinyBS bsModal
#' 
loadapp_ui <- function(id){
    ns <- NS(id)
    uiOutput(ns('modal'))
}


#' @rdname mod_loading_page
#' @export
#' @keywords internal
#' 
loadapp_server <- function(id){
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    funcs <- c('Convert', 
      'open_dataset', 
      'open_demoDataset',
      'view_dataset',
      'infos_dataset')
    
    
    dataOut <- reactiveValues(
      pkg.loaded = FALSE,
      files.sourced = FALSE)
    
    rv <- reactiveValues(
      list.funcs = lapply(setNames(nm=funcs), function(x) NULL),
      m = NULL,
      dataOut = NULL
    )

    
    output$modal <- renderUI({
      showModal(
        modalDialog(
        
        h3('Choose packages to load'),
            hr(),
          rv$m
        ,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("ok"), "OK") # wrapped in ns()
        
        )
      )
      )
    })
    

    observeEvent(req(funcs), ignoreInit = FALSE, {
      lapply(funcs, function(x){
        find_ui_func <- find_funs(paste0(x, '_ui'))$package_name
        find_server_func <- find_funs(paste0(x, '_server'))$package_name
        ind <- match('MagellanNTK', unique(find_ui_func, find_server_func))
        pkg2add <- unique(find_ui_func, find_server_func)
        rv$list.funcs[[x]] <- append(rv$list.funcs[[x]], pkg2add)
      }
      )
      
      rv$m <- matrix(rep('', length(funcs)), 
                     nrow = length(funcs), 
                     ncol = 1, 
                     byrow = TRUE,
                     dimnames = list(funcs, 'Packages')
                     )

      rv$m <- lapply(names(rv$list.funcs), function(x){
        content <- setNames(paste0(rv$list.funcs[[x]], '::', x), nm = rv$list.funcs[[x]])
        if (!is.null(content))
          list(
            h3(x),
            radioButtons(ns(x), '', choices = content)
          )
      })
    }, priority=1000)
    

    observeEvent(input$ok, {
        dataOut$files.sourced <- TRUE
        rv$dataOut <- reactiveValuesToList(input)[funcs]
        removeModal()
    })
    
    
    reactive({rv$dataOut})

  })
  
}






#' @title Find the packages of a function
#' 
#' @description 
#' This code is extracted from https://sebastiansauer.github.io/finds_funs/
#' 
#' @param f name of function for which the package(s) are to be identified.
#' 
#' @examples 
#' find_funs('filter')
#' 
#' @return 
#' A dataframe with two columns:
# `package_name`: packages(s) which the function is part of (chr)
# `builtin_package`:  whether the package comes with standard R 
#  (a 'builtin'  package)
#' 
#' @export
#' 
#' @rdname mod_loading_page
#' 
find_funs <- function(f) {
  
  if ("tidyverse" %in% rownames(installed.packages()) == FALSE) {
    cat("tidyverse is needed for this fuction. Please install. Stopping")
    stop()}
  
  suppressMessages(library(tidyverse))
  
  
  # search for help in list of installed packages
  help_installed <- help.search(paste0("^",f,"$"), agrep = FALSE)
  
  # extract package name from help file
  pckg_hits <- help_installed$matches[,"Package"]
  
  if (length(pckg_hits) == 0) pckg_hits <- "No_results_found"
  
  
  # get list of built-in packages
  
  pckgs <- installed.packages()  %>% as_tibble
  pckgs %>%
    dplyr::filter(Priority %in% c("base","recommended")) %>%
    dplyr::select(Package) %>%
    distinct -> builtin_pckgs_df
  
  # check for each element of 'pckg hit' whether its built-in and loaded (via match). Then print results.
  
  results <- tibble(
    package_name = pckg_hits,
    builtin_pckage = match(pckg_hits, builtin_pckgs_df$Package, nomatch = 0) > 0,
    loaded = match(paste("package:",pckg_hits, sep = ""), search(), nomatch = 0) > 0
  )
  
  return(results)
  
}




#' @export
#' 
#' @rdname mod_loading_page
#' 
loadApp <- function(){
  ui <- fluidPage(
  loadapp_ui("mod_pkg")
    )

server <- function(input, output, session) {
  
  done <- loadapp_server("mod_pkg")
  
  observeEvent(req(done()), {
               print(done())
    })
}

app <- shinyApp(ui, server)

}


