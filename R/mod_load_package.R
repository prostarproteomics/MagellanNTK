
#' @title Change the default functions in `MagellanNTK`
#' @description  This module allows to change the default functions
#' embedded in the package `MagellanNTK`. These fucntions are the following:
#' * convert_dataset: xxx
#' * view_dataset: xxx
#' * infos_dataset: xxx
#' 
#' 
#' @param id shiny id
#' @param funcs A list
#' 
#' @examplesIf interactive()
#' funcs <- list(convert_dataset = "DaparToolshed::convert_dataset",
#' open_dataset = "MagellanNTK::open_dataset",
#' open_demoDataset = "MagellanNTK::open_demoDataset",
#' view_dataset = "omXplore::view_dataset",
#' infos_dataset = "MagellanNTK::infos_dataset",
#' download_dataset = "MagellanNTK::download_dataset",
#' export_dataset = "MagellanNTK::export_dataset",
#' addDatasets = "MagellanNTK::addDatasets",
#' keepDatasets = "MagellanNTK::keepDatasets"
#' )
#' shiny::runApp(load_package(funcs))
#' 
#' shiny::runApp(load_package())
#'
#' 
#' @name mod_load_package
#' @author Samuel Wieczorek
#' 
NULL

timeoutSeconds <- 30 * 60

inactivity <- sprintf("function idleTimer() {
var t = setTimeout(logout, %s);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions
function logout() {
Shiny.setInputValue('timeOut', '%ss')
}
function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, %s);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();", timeoutSeconds*1000, timeoutSeconds, timeoutSeconds*1000)



#' @rdname mod_load_package
#' @import shiny
#' @export
#'
mod_load_package_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3('Load package tool'),
    uiOutput(ns('update_func_ui')),
    uiOutput(ns('select_pkg_ui')),
    #actionButton(ns('update_btn'), 'Update value'),
    uiOutput(ns('show_table'))
    #actionButton(ns('validate_btn'), 'Validate')
  )
}





#'
#' @export
#' @import shiny
#' @rdname mod_load_package
#'
mod_load_package_server <- function(id, 
  funcs = reactive({NULL})) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    dataOut <- reactiveValues(
      done = FALSE)
    
    
    rv <- reactiveValues(
      list.funcs = default.funcs()
    )

    observeEvent(req(funcs()), {
    
      rv$list.funcs <- funcs()
    }, priority = 1000)
    
      output$show_table <- renderUI({
        req(rv$list.funcs)
        
        lapply(names(rv$list.funcs), function(x){
          find_ui_func <- find_funs(paste0(x, '_ui'))$package_name
          find_server_func <- find_funs(paste0(x, '_server'))$package_name
  #browser()
          .choices <- unique(unique(find_ui_func, find_server_func))
          #.tmp <- .tmp[-grep('MagellanNTK', .tmp)]
          #.choices <- c(.tmp, 'MagellanNTK')
          
          .selected <- 'MagellanNTK'
          .found <- sum(unlist(lapply(.choices, function(y) 
            MagellanNTK::is.substr(y, rv$list.funcs[[x]]))))
          if (.found == 1){
            .pkg <- unlist(strsplit(rv$list.funcs[[x]], split = '::'))[1]
            .choices <- c(.pkg, .choices[-grep(.pkg, .choices)])
            .selected <- .pkg
          }
            
            
          # reordering list to put the target
          fluidRow(
            div(style = "align: center;display:inline-block; vertical-align: middle;padding-right: 10px;",
              p(x)),
            div(style = "align: center;display:inline-block; vertical-align: middle;padding-right: 10px;",
              selectInput(ns(paste0(x, '_ui')), 
            NULL,
            choices = .choices,
            selected = .selected
          )
          )
          )
        })
      })

    observeEvent(lapply(names(rv$list.funcs), function(x) input[[paste0(x, '_ui')]]), {
      #req(rv$list.funcs)
      ll <- list()
      for (c in names(rv$list.funcs))
        ll[[c]] <- paste0(input[[paste0(c, '_ui')]], '::', c)
      
      dataOut$value <- ll
    })
    
    reactive({dataOut$value})
  })
  
}


#' @export
#' @import shiny
#' @rdname mod_load_package
#' 
load_package <- function(funcs = NULL){
  ui <- mod_load_package_ui("mod_pkg")

server <- function(input, output, session) {
  
  done <- mod_load_package_server("mod_pkg", 
    funcs = reactive({funcs}))
  
  observeEvent(done(), {
    print(done())
  })
  }
  app <- shinyApp(ui, server)
}
