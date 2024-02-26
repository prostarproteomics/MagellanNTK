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
#' if(interactive(){
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
loadapp_ui <- function(id){
    ns <- NS(id)
    tagList(
        uiOutput(ns('load_pkg'))
    )
}


#' @rdname mod_loading_page
#' @export
#' @keywords internal
#' 
loadapp_server <- function(id, 
                           funcs = NULL){
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    dataOut <- reactiveValues(
      pkg.loaded = FALSE,
      files.sourced = FALSE)
    
    rv <- reactiveValues(
      list.funcs = lapply(setNames(nm=funcs), function(x) NULL),
      m = NULL,
      dataOut = NULL
    )

    
    output$load_pkg <- renderUI({
      #req(is.null(pkg))
      
      tagList(
        h3('Choose packages to load'),
        rv$m,
        actionButton(ns('load_pkg'), 'Load package')
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
    

    observeEvent(input$load_pkg, {
        dataOut$files.sourced <- TRUE
        rv$dataOut <- reactiveValuesToList(input)[funcs]
    })
    reactive({rv$dataOut})

  })
  
}




#___________________________________________________________
ui <- fluidPage(
  loadapp_ui("mod_pkg")
)

server <- function(input, output, session) {
  funcs <- c('Convert', 
             'open_dataset', 
             'open_demoDataset',
             'view_dataset',
             'infos_dataset')
  
  done <- loadapp_server("mod_pkg", funcs = funcs)
  #done <- mod_load_package_server("mod_pkg", pkg = 'DaparToolshed')
  
  observeEvent(req(done()), {
               print(done())
    })
}

shinyApp(ui, server)


