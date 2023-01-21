options(shiny.maxRequestSize=300*1024^2) 
options(encoding = "UTF-8")
options(shiny.fullstacktrace = TRUE)

require(compiler)
enableJIT(3)


library(shinydashboard)
library(shinyjs)

dev_mode <<- FALSE



#' The application server-side
#' 
#' @param input,output,session  Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinyjs
#' 
#' @noRd
server <- function( input, output, session) {
      
  observeEvent(input$ReloadApp, {js$resetApp()})
      
  tmp <- reactiveVal()
  
  rv.core <- reactiveValues(
    path = NULL,
    workflow = NULL,
    pipeline.name = NULL,
    dataIn = NULL,
    result_convert = NULL,
    result_openfile = NULL,
    
    # Current object in MagellanNTK
    current.obj = NULL,
    
    # pipeline choosen by the user for its dataset
    current.pipeline = NULL,
    
    mode = 'user',
    
    notificationData = data.frame(
      text = c(
        "Sales are steady this month.",
        "How do I register?",
        "The new server is ready."
      ),
      status = c('success', 'success', 'warning'),
      stringsAsFactors = FALSE
    )
  )
  
  
  output$messageMenu <- renderMenu({
    msgs <- apply(rv.core$notificationData, 1, function(row) {
      notificationItem(text = row[["text"]], 
                       status = row[["status"]]
                       )
    })
    
    dropdownMenu(type = "notifications", 
                 badgeStatus = "warning", 
                 icon = icon('bell'),
                 .list = msgs)
  })
  
  observeEvent(input$devmode, ignoreInit = TRUE, {
    rv.core$mode <- if(isTRUE(input$devmode)) 'dev' else 'user'
    options(shiny.fullstacktrace = input$devmode)
  })
 
    tmp.workflow <- mod_load_workflow_server("openwf",
                                             mode = reactive({rv.core$mode})
                                             )
    
    ###
    ### Launch the workflow server function
    ###
    observeEvent(req(tmp.workflow$workflow()),{
      
      rv.core$path <- tmp.workflow$folder()
      rv.core$workflow <- tmp.workflow$workflow()
      rv.core$package <- tmp.workflow$package()
      
      path <- file.path(rv.core$path, 'R')
      tl.layout <- c('v', 'h')
      
      nav_server(id = rv.core$workflow,
                 dataIn = reactive({rv.core$current.obj}),
                 tl.layout = tl.layout,
                 mode = reactive({rv.core$mode})
                 )
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
    

    # 
    # output$EDA_AbsolutePanel <- renderUI({
    #   req(rv.core$current.obj)
    #   type <- 'default_EDA'
    #   if (module.exists('custom_EDA')) 
    #     type <- 'custom_EDA'
    #   
    #   do.call(
    #     paste0(type, '_server'), 
    #     list(id = 'plotdataset', object = reactive({rv.core$current.obj}) )
    #   )
    #   
    #   
    #   
    #   bsmodal_server(
    #     id = "tbl",
    #     title = "test",
    #     uiContent = do.call(paste0(type, '_ui'), list(id = 'plotdataset'))
    #   )
    #   
    #   
    #   absolutePanel(style='z-index: 1000;',
    #     tagList(
    #       h3('EDA'), 
    #       bsmodal_ui("tbl")
    #       ),
    #     top = '10px', left = NULL, right = '10px', bottom = NULL,
    #     width = NULL, height = NULL,
    #     draggable = TRUE, fixed = FALSE,
    #     cursor = c("auto", "move", "default", "inherit"))
    # 
    # })
    # 
    
    
    
    
    
    output$EDAUI <- renderUI({
      req(rv.core$current.obj)
      type <- 'default_EDA'
      if (module.exists('custom_EDA')) 
        type <- 'custom_EDA'
      
      do.call(
        paste0(type, '_server'), 
        list(id = 'plotdataset', object = reactive({rv.core$current.obj}) )
        )
      
       do.call(paste0(type, '_ui'), list(id = 'plotdataset'))

    })
    
    # output$title <- renderUI({
    #   req(rv.core$workflow)
    #   
    #   h3(rv.core$workflow)
    # })
    
    
    
    
    output$menuTitle <- renderUI({
      req(rv.core$workflow)
      
      mod_insert_md_server('faq', file.path(system.file('workflows', package = rv.core$package), 
                                             rv.core$workflow, 'md', 'faq.md'))
      bsmodal_server(id = "faqmodal", title = "test", uiContent = mod_insert_md_ui('faq'))
      
    dropdownButton(
      label = rv.core$workflow,
      icon = icon("sliders-h"),
      status = "primary",
      circle = FALSE,
      bsmodal_ui('faqmodal'), 
      tags$a(href="https://github.com/prostarproteomics/MagellanNTK", 
           target="_blank", icon("github"), title = 'Links')
    )
    })
    
    
    # Use a differente observer to catch the event to be able to
    # bypass the parameter dev_mode
    observeEvent(input$devmode, {
      shinyjs::toggle('browser', condition = input$devmode)
      shinyjs::toggle('Help_menu', condition = input$devmode)
      shinyjs::toggle('githubLink', condition = input$devmode)
      
      
    })
    
    observe({
      req(dev_mode)
      shinyjs::toggle('browser', condition = dev_mode)
      shinyjs::toggle('Help_menu', condition = dev_mode)
    })
    
    output$run_workflowUI <- renderUI({
      rv.core$workflow
      
      if (is.null(rv.core$workflow) ){
        h3('There is no workflow for the moment')
      } else {
        type <- 'default_EDA'
        if (module.exists('custom_EDA')) 
          type <- 'custom_EDA'
        
        do.call(
          paste0(type, '_server'), 
          list(id = 'plotdataset', object = reactive({rv.core$current.obj}) )
        )
        
        
        
        apModal_server(id = "tbl",
          title = "test",
          uiContent = do.call(paste0(type, '_ui'), list(id = 'plotdataset'))
        )
        
        
        
        
          tagList(
            apModal_ui("tbl"),
            nav_ui(id =rv.core$workflow)
          )
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
    
    
    output$wf_links_UI <- renderUI({
      req(rv.core$workflow)
      
      if (!is.null(rv.core$folder) && length(rv.core$folder) == 1){
        file <- file.path(rv.core$folder, 'md', 'links.md')
      } else if (!is.null(rv.core$package) && length(rv.core$package) == 1){
        file <- file.path(system.file('workflows', package = rv.core$package), 
                          rv.core$workflow, 'md', 'links.md')
      }
      mod_insert_md_server("wf_links", file)
      tagList(
      mod_insert_md_ui('wf_links')
      )
    })
    
    output$wf_faq_UI <- renderUI({
      req(rv.core$workflow)
      if (!is.null(rv.core$folder) && length(rv.core$folder) == 1){
        file <- file.path(rv.core$folder, 'md', 'faq.md')
      } else if (!is.null(rv.core$package) && length(rv.core$package) == 1){
        file <- file.path(system.file('workflows', package = rv.core$package), 
                          rv.core$workflow, 'md', 'faq.md')
      }
      mod_insert_md_server("wf_faq", file)
      mod_insert_md_ui('wf_faq')
    })
    
    mod_insert_md_server("homepage", "http://www.prostar-proteomics.org/md/presentation.md")
 
    
    
    
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
