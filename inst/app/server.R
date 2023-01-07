options(shiny.maxRequestSize=300*1024^2) 
options(encoding = "UTF-8")
options(shiny.fullstacktrace = T)
require(compiler)
enableJIT(3)


library(shinydashboard)
library(shinyjs)
library(MagellanNTK)


#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinyjs
#' 
#' @noRd
shinyServer( 

    function( input, output, session ) {
   
  observeEvent(input$ReloadApp, {js$resetApp()})
  #observeEvent(input$ReloadApp, { js$reset()})
      
  tmp <- reactiveVal()
  
  rv.core <- reactiveValues(
    path = NULL,
    workflow = NULL,
    pipeline.name = NULL,
    dataIn = NULL,
    result_convert = NULL,
    result_openDemoDataset = NULL,
    
    # Current QFeatures object in Prostar
    current.obj = NULL,
    
    # pipeline choosen by the user for its dataset
    current.pipeline = NULL
  )
  
  #rv.core$pipeline.name <- mod_choose_pipeline_server('pipe', package = 'MSPipelines')
  
  messageData <- data.frame(
    text = c(
      "Sales are steady this month.",
      "How do I register?",
      "The new server is ready."
    ),
    status = c('success', 'success', 'warning'),
    stringsAsFactors = FALSE
  )
  
  output$messageMenu <- renderMenu({
    msgs <- apply(messageData, 1, function(row) {
      notificationItem(text = row[["text"]], 
                       status = row[["status"]]
                       )
    })
    
    # This is equivalent to calling:
    #   dropdownMenu(type="messages", msgs[[1]], msgs[[2]], ...)
    dropdownMenu(type = "notifications", .list = msgs)
  })
  
  
  
 
    tmp <- Load_Workflow_server("open_wf")
    
    observe({
      rv.core$path <- tmp()$folder
      rv.core$workflow <- tmp()$workflow
    })
    
    
    rv.core$current.obj <- Load_Dataset_server("open_file", 
                                               path = reactive({rv.core$path}))
    
    
    output$describe_wf <- renderUI({
      req(rv.core$path)
      mod_insert_md_ui("description_wf")
      mod_insert_md_server("description_wf", "http://www.prostar-proteomics.org/md/presentation.md")
    })
    
    
    ###
    ### Run_workflow part
    ###
    observeEvent(req(rv.core$path),{
      #req(rv.core$current.obj(), rv.core$path())
      verbose <- TRUE
      #browser()
      path <- file.path(rv.core$path, 'R')
      
      path <- system.file("extdata/module_examples", package = "MagellanNTK")
      data(data_na)
      rv.core$current.obj <- data_na
      
    dataOut <- reactiveVal(NULL)
    
    output$debugInfos_ui <- renderUI({
      req(verbose)
      Debug_Infos_ui("debug_infos")
    })
    
    output$save_dataset_ui <- renderUI({
      req(dataOut())
      req(dataOut()$dataOut()$value)
      dl_ui("saveDataset")

      dl_server(
        id = "saveDataset",
        dataIn = reactive({dataOut()$dataOut()$value})
      )

    })
    
    observeEvent(req(rv.core$current.obj), {
      dataOut(nav_server(
        id = rv.core$workflow,
        verbose = verbose,
        dataIn = reactive({rv.core$current.obj}),
        tl.layout = c("v", "h"),
        path = path
      ))
      
      Debug_Infos_server(
        id = "debug_infos",
        title = "Infos from shiny app",
        rv.dataIn = reactive({data_na}),
        dataOut = reactive({dataOut()$dataOut()$value})
      )
      
      mod_plots_server('plots', object = reactive({rv.core$current.obj}))
      
      bsmodal_server(
        id = "tbl",
        title = "test",
        uiContent = mod_plots_ui('plots')
      )
      
    })
   
    
    ###
    ### End of run_workflow part
    ###
    ###
    })
    
    
    output$run_workflowUI <- renderUI({
      rv.core$workflow
      tagList(
        nav_ui(rv.core$workflow),
        uiOutput("debugInfos_ui")
      )
    })
    
    
    
    
    
    mod_insert_md_server("homepage", "http://www.prostar-proteomics.org/md/presentation.md")
    mod_insert_md_server("faq", "http://www.prostar-proteomics.org/md/FAQ.md")
    mod_insert_md_server("links", "http://www.prostar-proteomics.org/md/links.md")
    
    mod_bug_report_server('bug_report')
    
    
    output$tab_plots_ui <- renderUI({
      rv.core$current.obj
      tagList(
      uiOutput("chooseDataset_ui"),
      mod_plots_ui('plots')
    )
    })
    
    output$chooseDataset_ui <- renderUI({
      req(rv.core$current.obj)
      if (length(names(rv.core$current.obj)) == 0) {
        choices <- list(" " = character(0))
      } else {
        choices <- names(rv.core$current.obj)
      }
      
      selectInput("chooseDataset", "Dataset",
                  choices = choices,
                  selected = names(rv.core$current.obj)[length(rv.core$current.obj)],
                  width = 200
      )
    })
  
  #
  # Code for convert tool
  #
  # convert = Convert$new('convertTool')
  # ## Get the return values of modules in charge of loading datasets
  # observe({
  #   rv.core$result_convert <- convert$server(dataIn = reactive({rv.core$current.obj}))
  #   })
  # shinyjs::delay(1000, rv.core$current.obj <- NA)
  # 
  
  
  #
  # Code for open demo dataset
  #
  #rv.core$result_openDemoDataset <- mod_open_demoDataset_server('demo_data')
  
  #observeEvent(rv.core$result_openDemoDataset(),{
  #   rv.core$current.obj <- rv.core$result_openDemoDataset()
  #   #rv.core$current.pipeline <- rv.core$tmp_dataManager$openFile()$pipeline
  #   print('rv.core$current.obj has changed')
  # })
  
  
  # rv.core$result_openFile <- mod_open_dataset_server('moduleOpenDataset')
  # observeEvent(rv.core$tmp_dataManager$openFile(),{
  #    rv.core$current.obj <- rv.core$tmp_dataManager$openFile()$dataset
  #    rv.core$current.pipeline <- rv.core$tmp_dataManager$openFile()$pipeline
  #  })
   
  # observeEvent(rv.core$result_convert(),{
  #   #browser()
  #   rv.core$dataIn <- rv.core$result_convert()
  #   #   rv.core$current.pipeline <- rv.core$tmp_dataManager$convert()$pipeline
  # })
  
  # observe({
  #   #shinyjs::toggle('div_demoDataset', condition = !is.null(rv.core$pipeline.name()) && rv.core$pipeline.name() != 'None')
  #   shinyjs::toggle('load_dataset_btn', condition = !is.null(rv.core$result_openDemoDataset()))
  # })
  # 
  observeEvent(input$browser,{browser()})
  
  # observe({
  #   req(rv.core$pipeline.name() != 'None')
  #   print("Launch Magellan")
  #   obj <- base::get(rv.core$pipeline.name())
  #   rv.core$pipeline <- do.call(obj$new, list('App'))
  #   rv.core$pipeline$server(dataIn = reactive({rv.core$dataIn}))
  # })
  
  # observeEvent(input$load_dataset_btn, {
  #   #browser()
  #   print(names(rv.core$result_openDemoDataset()))
  #   updateTabItems(session, "sb", "pipeline")
  #   shinyjs::delay(100, rv.core$dataIn <- rv.core$result_openDemoDataset())
  # })
  
  
  
  
  #output$show_convert <- renderUI({
  #  req(convert)
  #  convert$ui()
  #})
  
  
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