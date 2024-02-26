#' @title   mod_loading_page_ui and mod_loading_page_server
#' @description  A shiny Module.
#' 
#' @name mod_loading_page
#' 
#' @examples 
#' if(interactive(){
#' library(shinyjs)
#' library(shiny)
#' ui <- mod_loading_page_ui("mod_info")
#' server <- function(input, output, session) {
#'   mod_loading_page_server("mod_info")
#' }
#' 
#' shinyApp(ui, server)
#' }
NULL


#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname loadapp_module
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList
#' @importFrom shinyjs inlineCSS
loadapp_ui <- function(id){
  ns <- NS(id)
  tagList(
      shinyjs::useShinyjs(),
      div(id=ns('div_splashscreen'), 
          uiOutput(ns('splashscreen'))),
      shinyjs::hidden(div(id=ns('div_load_pkg'), uiOutput(ns('load_pkg'))))
      #hidden(div(id = 'div_mainapp_module', mainapp_ui('mainapp_module')))

  )
}

# Module Server

#' @param id xxx
#' @rdname loadapp_module
#' @export
#' @keywords internal

loadapp_server <- function(id, 
                           pkg = NULL,
                           funcs = NULL){
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    
    observe({
      shinyjs::toggle('div_splashscreen', condition = is.null(rv$m))
      shinyjs::toggle('div_load_pkg', condition = !is.null(rv$m) && !dataOut$pkg.loaded)
    })
    
    
    output$splashscreen <- renderUI({
      tagList(
        shinyjs::inlineCSS(progressBar_css),
        absolutePanel(
          id  = ns("AbsolutePanel"),
          class = "panel panel-default",
          style= "text-align: center; background-color: #25949A;",
          top = '30%',
          left = '25%',
          width = "50%",
          height = "150px",
          draggable = FALSE,
          fixed = TRUE,
          tagList(
            tags$h1(style='text-align: center; color: white', "Prostar is loading, please wait..."),
            br(),
            tags$div(class="progress", tags$div(class="indeterminate"))
          )
        )
      )
    })
    
    
    rv.pkg <- reactiveVal(pkg)
    
    dataOut <- reactiveValues(
      pkg.loaded = FALSE,
      files.sourced = FALSE)
    
    rv <- reactiveValues(
      list.funcs = lapply(setNames(nm=funcs), function(x) NULL),
      m = NULL,
      dataOut = NULL
    )
    
    
    
    output$load_pkg <- renderUI({
      req(is.null(pkg))
      
      tagList(
        h3('Choose packages to load'),
        #uiOutput(ns('checkUi'))
        rv$m,
        actionButton(ns('load_pkg'), 'Load package')
      )
    })
    
    
    observeEvent(req(funcs), ignoreInit = FALSE, {
      #rv$list.funcs <- lapply(setNames(nm=funcs), function(x) 'Prostar.2.0')
      
      lapply(funcs, function(x){
        find_ui_func <- find_funs(paste0(x, '_ui'))$package_name
        find_server_func <- find_funs(paste0(x, '_server'))$package_name
        ind <- match('Prostar.2.0', unique(find_ui_func, find_server_func))
        pkg2add <- unique(find_ui_func, find_server_func)
        rv$list.funcs[[x]] <- append(rv$list.funcs[[x]], pkg2add)
      }
      )
      
      # 
      # mods <- names(rv$list.funcs)
      # pkgs <- unique(unname(unlist(rv$list.funcs)))
      # n <- length(names(rv$list.funcs))
      # 
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
      
      #browser()
      # source(system.file("ProstarApp/mod_mainapp.R", package = 'Prostar.2.0'), local = TRUE)$value
      
      
       #source(system.file("ProstarApp/mod_homepage.R", package = 'Prostar.2.0'), local = TRUE)$value
      #(system.file("ProstarApp/mod_insert_md.R", package = 'Prostar.2.0'), local = TRUE)$value
      # source(file.path(".", "mod_choose_pipeline.R"), local = TRUE)$value
       #source(file.path(".", "mod_release_notes.R"), local = TRUE)$value
       #source(file.path(".", "mod_check_updates.R"), local = TRUE)$value
       #source(file.path(".", "mod_bug_report.R"), local = TRUE)$value
      # source(file.path(".", "mod_test.R"), local = TRUE)$value
       #source(system.file("ProstarApp/mod_settings.R", package = 'Prostar.2.0'), local = TRUE)$value
      dataOut$files.sourced <- TRUE
      
      rv$dataOut <- reactiveValuesToList(input)[funcs]
      
    })
    
    
    reactive({rv$dataOut})

  })
  
}
#' 
#' 
#' 
progressBar_css <- ".progress {
  position: relative;
  height: 4px;
  display: block;
  width: 100%;
  background-color: #E87352;
  border-radius: 2px;
  background-clip: padding-box;
  margin: 0.5rem 0 1rem 0;
  overflow: hidden; }


.progress .determinate {
    position: absolute;
    background-color: inherit;
    top: 0;
    bottom: 0;
    background-color: #E87352;
    transition: width .3s linear; }

.progress .indeterminate {
    background-color: blue; }
    .progress .indeterminate:before {
      content: '';
      position: absolute;
      background-color: #ffffff;
      top: 0;
      left: 0;
      bottom: 0;
      will-change: left, right;
      -webkit-animation: indeterminate 2.1s cubic-bezier(0.65, 0.815, 0.735, 0.395) infinite;
              animation: indeterminate 2.1s cubic-bezier(0.65, 0.815, 0.735, 0.395) infinite; }

.progress .indeterminate:after {
      content: '';
      position: absolute;
      background-color: #ffffff;
      top: 0;
      left: 0;
      bottom: 0;
      will-change: left, right;
      -webkit-animation: indeterminate-short 2.1s cubic-bezier(0.165, 0.84, 0.44, 1) infinite;
              animation: indeterminate-short 2.1s cubic-bezier(0.165, 0.84, 0.44, 1) infinite;
      -webkit-animation-delay: 1.15s;
              animation-delay: 1.15s; }

@-webkit-keyframes indeterminate {
  0% {
    left: -35%;
    right: 100%; }
  60% {
    left: 100%;
    right: -90%; }
  100% {
    left: 100%;
    right: -90%; } }
@keyframes indeterminate {
  0% {
    left: -35%;
    right: 100%; }
  60% {
    left: 100%;
    right: -90%; }
  100% {
    left: 100%;
    right: -90%; } }
@-webkit-keyframes indeterminate-short {
  0% {
    left: -200%;
    right: 100%; }
  60% {
    left: 107%;
    right: -8%; }
  100% {
    left: 107%;
    right: -8%; } }
@keyframes indeterminate-short {
  0% {
    left: -200%;
    right: 100%; }
  60% {
    left: 107%;
    right: -8%; }
  100% {
    left: 107%;
    right: -8%; } }"


#___________________________________________________________
ui <- fluidPage(
  loadapp_ui("mod_pkg")
)

server <- function(input, output, session) {
  funcs <- c('convert', 
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


