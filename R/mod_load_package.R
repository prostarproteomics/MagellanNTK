
#' @title   mod_choose_pipeline_ui and mod_choose_pipeline_server
#' @description  This module is used to load the package which contains all the code
#' to work with prostar.2.0.
#' 
#' @param id shiny id
#' @param pkg xxx
#' 
#' @examplesIf interactive()
#' funcs <- list(convert_dataset = "DaparToolshed::convert_dataset",
#' open_dataset = "MagellanNTK::open_dataset",
#' open_demoDataset = "MagellanNTK::open_demoDataset",
#' view_dataset = "omXplore::view_dataset",
#' infos_dataset = "MagellanNTK::infos_dataset",
#' addDatasets = "MagellanNTK::addDatasets",
#' keepDatasets = "MagellanNTK::keepDatasets"
#' )
#' shiny::runApp(load_package(funcs))
#'
#' 
#' @name mod_load_package
#' @author Samuel Wieczorek
#' 
NULL

timeoutSeconds <- 30*60

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
mod_load_package_server <- function(id, funcs = NULL) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    dataOut <- reactiveValues(
      done = FALSE)
    
    
    rv <- reactiveValues(
      list.funcs = funcs
    )

      output$show_table <- renderUI({
        req(rv$list.funcs)
        
        lapply(names(rv$list.funcs), function(x){
          
          find_ui_func <- find_funs(paste0(x, '_ui'))$package_name
          find_server_func <- find_funs(paste0(x, '_server'))$package_name

          fluidRow(
            div(style = "align: center;display:inline-block; vertical-align: middle;padding-right: 10px;",
              p(x)),
            div(style = "align: center;display:inline-block; vertical-align: middle;padding-right: 10px;",
              selectInput(ns(paste0(x, '_ui')), 
            NULL,
            choices = unique(find_ui_func, find_server_func))
          )
          )
        })
      })

    # 
    # output$update_func_ui <- renderUI({
    #   selectInput(ns('update_func'), 'Function to update',
    #     choices = names(funcs))
    # })
    # 
    # output$select_pkg_ui <- renderUI({
    #   req(input$update_func)
    #   find_ui_func <- find_funs(paste0(input$update_func, '_ui'))$package_name
    #   find_server_func <- find_funs(paste0(input$update_func, '_server'))$package_name
    #   selectInput(ns('choosepkg'), 
    #     'Package',
    #     choices = unique(find_ui_func, find_server_func)
    #   )
    # })

    # 
    # observeEvent(req(input$update_btn), {
    #   
    #   ind <- which(rv$list.funcs['Function' ] == input$update_func)
    # 
    #   rv$list.funcs[ind, 'Package'] <- paste0(input$choosepkg, '::', input$update_func)
    # 
    #   })
    # 
    observeEvent(lapply(names(rv$list.funcs), function(x) input[[paste0(x, '_ui')]]), {
      ll <- list()
      for (c in names(rv$list.funcs))
        ll[[c]] <- paste0(input[[paste0(c, '_ui')]], '::', c)
      
      dataOut$value <- ll
    })
    
    # 
    # observeEvent(input$validate_btn, {
    #   ll <- list()
    #   for (c in names(rv$list.funcs))
    #     ll[[c]] <- paste0(input[[paste0(c, '_ui')]], '::', c)
    #   
    #   dataOut$value <- ll
    # })
    
    reactive({dataOut$value})
  })
  
}



load_package <- function(funcs){
  ui <- mod_load_package_ui("mod_pkg")

server <- function(input, output, session) {
  
  
  done <- mod_load_package_server("mod_pkg", funcs = funcs)
  #done <- mod_load_package_server("mod_pkg", pkg = 'DaparToolshed')
  
  observeEvent(done(), {
    print(done())
  })
  }
  app <- shinyApp(ui, server)
}
