options(shiny.maxRequestSize=300*1024^2) 
options(encoding = "UTF-8")
require(compiler)
enableJIT(3)


library(shinydashboard)
library(shinyjs)
library(MagellanNTK)


#' The application server-side
#' 
#' @param input,output,session  Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinyjs
#' 
#' @noRd
server <- shinyServer( 

    function( input, output, session) {
      options(shiny.fullstacktrace = dev_mode)
      
  observeEvent(input$ReloadApp, {js$resetApp()})
      
  tmp <- reactiveVal()
  
  rv.core <- reactiveValues(
    path = NULL,
    workflow = NULL,
    pipeline.name = NULL,
    dataIn = NULL,
    result_convert = NULL,
    result_openfile = NULL,
    
    # Current QFeatures object in Prostar
    current.obj = NULL,
    
    # pipeline choosen by the user for its dataset
    current.pipeline = NULL
  )
  
  notificationeData <- data.frame(
    text = c(
      "Sales are steady this month.",
      "How do I register?",
      "The new server is ready."
    ),
    status = c('success', 'success', 'warning'),
    stringsAsFactors = FALSE
  )
  
  output$messageMenu <- renderMenu({
    msgs <- apply(notificationeData, 1, function(row) {
      notificationItem(text = row[["text"]], status = row[["status"]])
    })
    
    dropdownMenu(type = "notifications", .list = msgs)
  })
  
  
  GetTitle <- reactive({
    h3('toto')
  })
 
    tmp.workflow <- mod_load_workflow_server("openwf")
    
    observeEvent(req(tmp.workflow$workflow()),{
      
      rv.core$path <- tmp.workflow$folder()
      rv.core$workflow <- tmp.workflow$workflow()
      
      path <- file.path(rv.core$path, 'R')
      print(paste0('dev_mode = ', dev_mode))
      #path <- system.file("extdata/module_examples", package = "MagellanNTK")
      #data(data_na)
      #rv.core$current.obj <- data_na
      tl.layout <- c('v', 'h')
      #isolate({
      mod_run_workflow_server(id = rv.core$workflow,
                              dataIn = reactive({rv.core$current.obj}),
                              tl.layout = tl.layout)
    })
    
   
    
    
    
    
    ###
    ### Openfile
    ###
    
    GetModuleType <- reactive({
      req(rv.core$workflow)
      type <- 'default_openfile'
      if (module.exists('custom_openfile')) 
        type <- 'custom_openfile'
      type
    })
    
     
    output$openFileUI <- renderUI({
      req(rv.core$workflow)

      rv.core$result_openfile <- do.call(paste0(GetModuleType(), '_server'), list(id = 'openfile'))
      do.call(paste0(GetModuleType(), '_ui'), list(id = 'openfile'))
    })
    
    
    observe({
      req(rv.core$result_openfile)
      rv.core$current.obj <- rv.core$result_openfile$data()
      rv.core$current.obj.name <- rv.core$result_openfile$name()
      
    })
    
    ###
    ### Export file
    ###
    output$exportUI <- renderUI({
      req(rv.core$current.obj)
      type <- 'default_export'
      if (module.exists('custom_export')) 
        type <- 'custom_export'

      do.call(
        paste0(type, '_server'), 
        list(id = 'exportdataset', object = reactive({rv.core$current.obj})))
      
      do.call(paste0(type, '_ui'), list(id = 'exportdataset'))
      
    })
    
    output$convertUI <- renderUI({
      
      type <- 'default_convert'
      if (module.exists('custom_convert')) 
        type <- 'custom_convert' 

      
      rv.core$result_convert <- do.call(
        paste0(type, '_server'), 
        list(id = 'convertdataset'))()
      
      do.call(paste0(type, '_ui'), list(id = 'convertdataset'))

    })
    
    output$plotsUI <- renderUI({
      req(rv.core$current.obj)
      type <- 'default_plots'
      if (module.exists('custom_plots')) 
        type <- 'custom_plots'
      
      do.call(
        paste0(type, '_server'), 
        list(id = 'plotdataset', object = reactive({rv.core$current.obj}) )
        )
      
      do.call(paste0(type, '_ui'), 
              list(id = 'plotdataset'))
      
    })
    
    
    
    output$run_workflowUI <- renderUI({
      rv.core$workflow
      
      if (is.null(rv.core$workflow) ){
        h3('There is no workflow for the moment')
      } else {
        
          mod_run_workflow_ui(id =rv.core$workflow)
         # })
        }
      
      
      # observe({
      #   req(rv.core$workflow)
      #   path <- file.path(rv.core$path, 'R')
      #   
      #   path <- system.file("extdata/module_examples", package = "MagellanNTK")
      #   data(data_na)
      #   rv.core$current.obj <- data_na
      #   tl.layout <- c('v', 'h')
      #   #isolate({
      #   mod_run_workflow_server(id = rv.core$workflow,
      #                           dataIn = reactive({rv.core$current.obj}),
      #                           tl.layout = tl.layout)
      # })
      

    })
    
    
    
    mod_insert_md_server("homepage", "http://www.prostar-proteomics.org/md/presentation.md")
    mod_insert_md_server("wf_faq", "http://www.prostar-proteomics.org/md/FAQ.md")
    mod_insert_md_server("wf_links", "http://www.prostar-proteomics.org/md/links.md")
    
    
    ###
    ### Help menu
    ###
    mod_insert_md_server("magellan_faq", 
                         file.path(system.file('app/md', package='MagellanNTK'), 'magellan_faq.md'))
    mod_insert_md_server("magellan_about", 
                         file.path(system.file('app/md', package='MagellanNTK'), 'magellan_about.md'))
    mod_bug_report_server('bug_report')
    
 
  
    observeEvent(rv.core$result_openfile,{
      rv.core$current.obj <- rv.core$result_openfile
      #   rv.core$current.pipeline <- rv.core$tmp_dataManager$convert()$pipeline
    })
    
  observeEvent(rv.core$result_convert, ignoreNULL = TRUE, {
    rv.core$current.obj <- rv.core$result_convert
    #   rv.core$current.pipeline <- rv.core$tmp_dataManager$convert()$pipeline
  })
  
  
  observeEvent(input$browser,{browser()})
  
  
  # https://github.com/daattali/shinyjs/issues/74
  #output$show_pipeline <- renderUI({
  #  req(rv.core$pipeline)
  #  rv.core$pipeline$ui()
    # if (!is.null(rv.core$dataIn))
    #   rv.core$pipeline$ui()
    # else
    #   shinyjs::disabled(rv.core$pipeline$ui())
  #})
  
  
  # mimics loading data > body content and inactivation of import menus in sidebar
  # observeEvent(rv.core$current.pipeline, ignoreNULL=FALSE, { 
  #   #https://stackoverflow.com/questions/48278111/disable-enable-click-on-dashboard-sidebar-in-shiny
  #   
  #   if(is.null(rv.core$current.pipeline)){
  #     # show sidebar and button sidebar
  #     shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
  #     shinyjs::runjs("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'visible';")
  #     
  #     # enable import menus
  #     shinyjs::removeCssClass(selector = "a[data-value='openFile']", class = "inactiveLink")
  #     shinyjs::removeCssClass(selector = "a[data-value='convert']", class = "inactiveLink")
  #     shinyjs::removeCssClass(selector = "a[data-value='demoData']", class = "inactiveLink")
  #   }
  #   else{ # "after data loaded"
  #     # hide sidebar/button sidebar
  #     shinyjs::addClass(selector = "body", class = "sidebar-collapse")
  #     shinyjs::runjs("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'hidden';")
  #     
  #     # disable import menus
  #     shinyjs::addCssClass(selector = "a[data-value='openFile']", class = "inactiveLink")
  #     shinyjs::addCssClass(selector = "a[data-value='convert']", class = "inactiveLink")
  #     shinyjs::addCssClass(selector = "a[data-value='demoData']", class = "inactiveLink")
  #   } 
  # })
  
  
  #---------------------------Server modules calls---------------------------------------------------#
  #DaparViz::mod_all_ds_server('daparviz', reactive({rv.core$current.obj}))
  
  #mod_test_server('tutu')
  mod_homepage_server('home')
  #mod_settings_server("global_settings", obj = reactive({Exp1_R25_prot}))
  #mod_release_notes_server("rl")
  #mod_check_updates_server("check_updates")
  #mod_insert_md_server("links_MD", URL_links)
  #mod_insert_md_server("FAQ_MD", URL_FAQ)
  #mod_bug_report_server("bug_report")

    }
)