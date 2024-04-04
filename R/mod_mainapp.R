#' @title   mod_main_page_ui and mod_loading_page_server
#' @description  A shiny Module.
#' 
#' @name mod_main_page
#' 
#' @examples 
#' if(interactive()){
#' 
#' shiny::runApp(mainapp())
#' 
#' }
#' 
#' @import shiny
#' @import shinyjs
#' @import shinydashboard
#' @importFrom shinydashboardPlus userOutput dashboardPage dashboardHeader 
#' dashboardBadge dashboardControlbar skinSelector dashboardSidebar renderUser
#' socialButton
#' @import shinyEffects
#' 
NULL


#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_main_page
#'
#' @keywords internal
#' @export 
#' 
mainapp_ui <- function(id){
  ns <- NS(id)
  
  div(id = "header",
    
      shinydashboardPlus::dashboardPage(
        md = FALSE,
        skin = "blue",
        
        #skin = shinythemes::shinytheme("cerulean"),
        
        # https://stackoverflow.com/questions/31711307/how-to-change-color-in-shiny-dashboard
        # orangeProstar <- "#E97D5E"
        # gradient greenblue header
        # greenblue links <- #2fa4e7
        # darker greenblue hover links <- #157ab5
        # darker greenblue titles <- #317eac
        # small titles <- #9999
        # darkest greenblue button reset+next+selected menu
        # color background arrow : #88b7d5 (bleu gris clair)
        # lightgrey #dddd
        # grey #ccc
        # bleu ceruleen #2EA8B1
        # jaune clair 'mark' #FCF8E3
        # green #468847
        # darker green #356635
        
        ##
        ## Header
        ## 
        # header = dashboardHeader(
        #   fixed = TRUE,
        #   title = dashboardthemes::shinyDashboardLogo(theme = "blue_gradient",
        #                                               boldText = "Prostar",
        #                                               badgeText = "v2"),
        #   leftUi = tagList(
        #   actionButton('browser', 'Console'),
        #   a(href="http://www.prostar-proteomics.org/"
        #       # img(src=base64enc::dataURI(
        #       #   file=system.file('ProstarApp/www/images', 'LogoProstarComplet.png', package='ProstarDev'), 
        #       #   mime="image/png"))
        #       ),
        #    a(href="https://github.com/samWieczorek/Prostar2",
        #       icon("github"),
        #       title="GitHub")
        #   )
        # ),
        shinydashboardPlus::dashboardHeader(
          fixed = TRUE,
          # titleWidth = "245px",
          # title = absolutePanel(
          #    fixed = TRUE,
          #    height = '100px',
          #    dashboardthemes::shinyDashboardLogo(theme = "blue_gradient",
          #                                        boldText = "Prostar",
          #                                        badgeText = "v2")
          #    ),
          # leftUi = tagList(
          #   tags$style(".skin-blue .main-header .navbar {background-color: rgb(20,97,117);}"),
          #   actionButton('browser', 'Console'),
          #   a(href="http://www.prostar-proteomics.org/"
          #     #       # img(src=base64enc::dataURI(
          #     #       #   file=system.file('ProstarApp/www/images', 'LogoProstarComplet.png', package='ProstarDev'), 
          #     #       #   mime="image/png"))
          #            ),
          #   a(href="https://github.com/prostarproteomics/Prostar.2.0",
          #     icon("github"),
          #     title="GitHub")
          # 
          # )
          title = 
            tagList(
              span(class = "logo-lg", 
                   absolutePanel(fixed = TRUE, "Menu workflow"))
              #absolutePanel(fixed = TRUE,  img(src = "ShinyDashboardPlus_FINAL.svg"))
              ),
          leftUi = tagList(
            h4(style = "font-weight: bold;", "MagellanNTK"), 
            shinydashboardPlus::dashboardBadge(
              GetPackageVersion('MagellanNTK'),
              color = "green"),
            uiOutput(ns('WF_Name_UI'))
            
            )
          
          
          ,dropdownMenu(
            type = "tasks",
            badgeStatus = "danger"
            # taskItem(value = 20, color = "aqua", "Refactor code"),
            # taskItem(value = 40, color = "green", "Design new layout"),
            # taskItem(value = 60, color = "yellow", "Another task"),
            # taskItem(value = 80, color = "red", "Write documentation")
            ,menuItem("Home 2", 
              tabName = "Home2", 
              icon = icon("home"),
              selected = TRUE)
            ,menuItem("User manual", 
              tabName = "usermanual", 
              icon = icon("home"),
              selected = TRUE)
          )
          # 
          #,shinydashboardPlus::userOutput(ns("user"))
        ),
        ##
        ## Sidebar
        ## 
        sidebar = shinydashboardPlus::dashboardSidebar(
          #fixed = TRUE,
          shinydashboard::sidebarMenu(id = "sb",
                      #style = "position: fixed; overflow: visible;",
            # inactiveClass for import menus inactivation 
           # tags$head(tags$style(".inactiveLink {pointer-events: none; background-color: grey;}")),
            
            # Menus and submenus in sidebar
            #br(),
            menuItem("Home", 
                     tabName = "Home", 
                     icon = icon("home"),
                     selected = TRUE),
            hr(),
            # menuItem("Data Manager",
            #          tabName = "dataManager",
            #          icon = icon("folder"),
            #          badgeLabel = "new", 
            #          badgeColor = "green"),
             h4('Dataset', style="color: green;"),
            menuItem("Open dataset",
              tabName = "openDataset",
              icon = icon("folder")
              # ,badgeLabel = "new"
              # ,badgeColor = "green"
              ),
            menuItem("Demo dataset",
              tabName = "demoDataset",
              icon = icon("folder")
              # ,badgeLabel = "new"
              # ,badgeColor = "green"
              ),
            menuItem("Convert dataset",
              tabName = "convertDataset",
              icon = icon("folder")
              # ,badgeLabel = "new"
              # ,badgeColor = "green"
              ),
            hr(),
            h4('Workflow', style="color: green;"),
            menuItem("Open", 
              tabName = "openWorkflow", 
              icon = icon("cogs")),
            menuItem("Run", 
                     tabName = "workflow", 
                     icon = icon("cogs")),
            hr(),
            h4('Vizualize data', style="color: green;"),
            menuItem("EDA", 
                     tabName = "eda", 
                     icon = icon("cogs")
              # ,badgeLabel = "new"
              # ,badgeColor = "green"
              ),
            hr(),
            menuItem(h4('Help', style="color: green;"),
 
                     #icon = icon("question-circle"),
                     menuSubItem("Useful Links", tabName = "usefulLinks"),
                     menuSubItem("FAQ", tabName = "faq"),
                     menuSubItem("Bug Report", tabName = "bugReport"),
                     menuSubItem("Global Settings", 
                                 tabName = "globalSettings", 
                                 icon = icon("cogs")),
                     menuSubItem("Release Notes", 
                                 tabName = "releaseNotes", 
                                 icon = icon("clipboard")),
                     menuSubItem("Check for Updates", 
                                 tabName = "checkUpdates", 
                                 icon = icon("wrench"))
                     )
            )
        ),
        controlbar = shinydashboardPlus::dashboardControlbar(
          skin = "dark",
          shinydashboardPlus::controlbarMenu(
            shinydashboardPlus::controlbarItem(
              title = "Tab 1",
              icon = icon("desktop"),
              active = TRUE,
              actionLink(ns('browser'), 'Console'),
              mod_settings_ui(ns('global_settings'))
            ),
            shinydashboardPlus::controlbarItem(
              icon = icon("paint-brush"),
              title = "Skin",
              shinydashboardPlus::skinSelector()
            )
          )
        ),
        body = shinydashboard::dashboardBody(
          # some styling
          tags$head(
            tags$style(
              rel = "stylesheet",
              type = "text/css",
              href = "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/styles/qtcreator_dark.min.css"
            ),
            tags$script(
              src = "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/highlight.min.js"
            ),
            tags$script(
              "$(function() {
            $('.sidebar-toggle').on('click', function() {
              $('.skinSelector-widget').toggle();
            });
          });
          "
            )
          ),

            #tags$head(
            #  tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
            #),
            
          div(style="margin-top: 40px;", 
            # body content
            tabItems(
              
              tabItem(tabName = "Home2", class="active",
                mod_homepage_ui(ns('home2'))),
              tabItem(tabName = "usermanual", class="active",
                insert_md_ui(ns('usermanual'))),
              
              
              tabItem(tabName = "Home", class="active", 
                mod_homepage_ui(ns('home'))),
              #tabItem(tabName = "dataManager", 
              #uiOutput(ns('dataManager_UI'))),
              tabItem(tabName = "openDataset", 
                uiOutput(ns('open_dataset_UI'))),
              #tabItem(tabName = "demoDataset", 
              #  uiOutput(ns('open_demo_dataset_UI'))),
              tabItem(tabName = "convertDataset", 
                uiOutput(ns('open_convert_dataset_UI'))),
              tabItem(tabName = "eda", 
                uiOutput(ns('EDA_UI'))),
              tabItem(tabName = "export", 
                h3("Export")), # export module not yet
              
              tabItem(tabName = "openWorkflow", 
                uiOutput(ns('open_workflow_UI'))),
              tabItem(tabName = "workflow", 
                uiOutput(ns('workflow_UI'))),
              
              
              #tabItem(tabName = "globalSettings", mod_settings_ui(ns('global_settings'))),
              tabItem(tabName = "releaseNotes", 
                mod_release_notes_ui(ns('rl'))),
              tabItem(tabName = "checkUpdates", 
                mod_check_updates_ui(ns('check_updates'))),
              tabItem(tabName = "usefulLinks", 
                insert_md_ui(ns('links_MD'))),
              tabItem(tabName = "faq", 
                insert_md_ui(ns('FAQ_MD'))),
              tabItem(tabName = "bugReport", 
                mod_bug_report_ui(ns("bug_report"))),
              tabItem(tabName = "pipeline", 
                uiOutput(ns('show_pipeline')))
            )
            # uiOutput('show_pipeline')
          )
        )
    )
)
}


#' @param id xxx
#' @rdname mod_main_page
#' @export
#' @keywords internal
#' 
mainapp_server <- function(id,
                           funcs = NULL,
  verbose = FALSE){
   
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    rv.core <- reactiveValues(
      pipeline = NULL,
      pipeline.name = NULL,
      dataIn = NULL,
      result_convert = reactive({NULL}),
      #result_openDemoDataset = NULL,
      result_open_dataset = NULL,
      result_open_workflow = NULL,
      result_run_workflow = NULL,
      current.obj = NULL,
      
      # pipeline choosen by the user for its dataset
      current.pipeline = NULL,
      
      workflow.name = NULL,
      workflow.path = NULL,
      
      funcs = default.funcs
    )

    
    observeEvent(id, {
      rv.core$funcs <- funcs
    }, priority = 1000)
    
    
    
    output$WF_Name_UI <- renderUI({
      
      req(rv.core$workflow.name)
      h4(rv.core$workflow.name)
      })
    
    
    # observeEvent(rv.core$current.obj, {
    #   print('base_URL')
    #   obj <- convert_to_mae(rv.core$current.obj)
    #   print(colnames(SummarizedExperiment::rowData(obj[[1]])))
    # }, priority = 1000)
    
    
    # delay(ms = 3500, show("app_title"))
    # delay(ms = 3800, show("app_slider_plot"))

    
    # output$user <- shinydashboardPlus::renderUser({
    #   shinydashboardPlus::dashboardUser(
    #     name = "Prostar proteomics", 
    #     image = 'https://raw.githubusercontent.com/prostarproteomics/Prostar_website/master/docs/favicon.ico', 
    #     #title = "Prostar-proteomics",
    #     #subtitle = "Author", 
    #     footer = fluidRow(
    #       column(width = 6, 
    #         shinydashboardPlus::socialButton(href = "https://github.com/prostarproteomics/MagellanNTK",
    #                           icon = icon("github")
    #                           )),
    #     column(width = 6,
    #       shinydashboardPlus::socialButton(href = "https://prostar-proteomics.org",
    #                         icon = icon("dropbox")
    #                         )
    #            )
    #     ),
    #     p('TODO: Describe the web site')
    #   )
    # })
    
    
    observeEvent(input$browser,{browser()})
    observeEvent(input$ReloadProstar, { js$reset()})

    
    call.func(
      fname = paste0(rv.core$funcs$infos_dataset, '_server'),
      args = list(id = 'infos',
        obj = reactive({rv.core$current.obj}))
    )
    
    output$infos_dataset_UI <- renderUI({
      req(rv.core$funcs)
      req(rv.core$current.obj)
      call.func(
        fname = paste0(rv.core$funcs$infos_dataset, '_ui'),
        args = list(id = ns('infos')))
    })
    
    

    #
    # Code for convert tool
    #
    
    
    output$open_convert_dataset_UI <- renderUI({
      req(rv.core$funcs)

      rv.core$result_convert <- call.func(
        fname = paste0(rv.core$funcs$convert_dataset, '_server'),
        args = list(id = 'Convert'))
      
      call.func(
        fname = paste0(rv.core$funcs$convert_dataset, '_ui'),
        args = list(id = ns('Convert')))
    })
    
    observeEvent(req(rv.core$result_convert()),{
      rv.core$current.obj <- rv.core$result_convert()
    })
    
    
    #
    # Code for open demo dataset
    #
    # rv.core$result_openDemoDataset <- call.func(
    #   fname = paste0(rv.core$funcs$open_demoDataset, '_server'),
    #   args = list(id = 'open_demo_dataset'))
    # 
    # output$open_demo_dataset_UI <- renderUI({
    #   req(rv.core$funcs)
    #   call.func(
    #     fname = paste0(rv.core$funcs$open_demoDataset, '_ui'),
    #     args = list(id = ns('open_demo_dataset')))
    # })
    # 
    # observeEvent(req(rv.core$result_openDemoDataset()),{
    #   rv.core$current.obj <- rv.core$result_openDemoDataset()
    # })
    
    #
    # Code for open dataset
    #
    rv.core$result_open_dataset <- call.func(
      fname = paste0(rv.core$funcs$open_dataset, '_server'),
      args = list(id = 'open_dataset'))
    
    output$open_dataset_UI <- renderUI({
      req(rv.core$funcs)
      call.func(fname = paste0(rv.core$funcs$open_dataset, '_ui'),
        args = list(id = ns('open_dataset')))
    })
    
    observeEvent(req(rv.core$result_open_dataset()),{
      rv.core$current.obj <- rv.core$result_open_dataset()
    })
    
    # observeEvent(rv.core$funcs, {
    #   browser()
    # })
    
    # Get workflow directory
    rv.core$result_open_workflow <- open_workflow_server("wf")
    
    observeEvent(req(rv.core$result_open_workflow()),{
      rv.core$workflow.name <- 
        session$userData$workflow.name <- rv.core$result_open_workflow()$wf_name
      
      rv.core$workflow.path <- 
        session$userData$workflow.path <- rv.core$result_open_workflow()$path
      
      funcs <- rv.core$funcs <- rv.core$result_open_workflow()$funcs
      
      
      # Fix NULL values
      # #browser()
      lapply(names(rv.core$funcs), function(x)
        if(is.null(rv.core$funcs[[x]]))
          rv.core$funcs[[x]] <- default.funcs[[x]]
      )

      source_wf_files(session$userData$workflow.path)
    })
    
    output$open_workflow_UI <- renderUI({
      open_workflow_ui(ns("wf"))
    })
    
    
    # Workflow code
    output$workflow_UI <- renderUI({
      req(rv.core$workflow.name)
      nav_ui(ns(basename(rv.core$workflow.name)))
      })

    observe({
      rv.core$result_run_workflow <- nav_server(
        id = rv.core$workflow.name,
        dataIn = reactive({rv.core$current.obj})
        )
    })
    
    observeEvent(req(rv.core$result_run_workflow$dataOut()$trigger), 
      ignoreInit = TRUE, {
      rv.core$current.obj <- rv.core$result_run_workflow$dataOut()$value
    })

       

    call.func(
      fname = paste0(rv.core$funcs$view_dataset, '_server'),
      args = list(id = 'view_dataset',
        obj = reactive({rv.core$current.obj}),
        useModal = FALSE,
        verbose = TRUE))

    output$EDA_UI <- renderUI({
      req(rv.core$funcs)
      call.func(
        fname = paste0(rv.core$funcs$view_dataset, '_ui'),
        args = list(id = ns('view_dataset')))
    })
    

    observe({
      filepath <- NULL
      if (!is.null(rv.core$workflow.path))
        filepath <- file.path(rv.core$workflow.path, 'md', 
          paste0(rv.core$workflow.name, '_Description.md'))
      else
        filepath <- file.path(system.file('app/md', 
          package = 'MagellanNTK'),'Presentation.Rmd')
          
          
      mod_homepage_server('home', filepath)
      mod_homepage_server('home2', filepath)
      mod_homepage_server('home3', filepath)
    })
    
    insert_md_server("usermanual", 
      file.path(rv.core$workflow.path, 'md', "FAQ.md"))
    
    
    #mod_settings_server("global_settings", obj = reactive({Exp1_R25_prot}))
    mod_release_notes_server("rl")
    mod_check_updates_server("check_updates")
    insert_md_server("links_MD", 
      file.path(rv.core$workflow.path, 'md', "links.md"))
    insert_md_server("FAQ_MD", 
      file.path(rv.core$workflow.path, 'md', "FAQ.md"))
    #mod_bug_report_server("bug_report")
  })
  
}





#___________________________________________________________
mainapp <- function(){
  
  ui <- fluidPage(
  mainapp_ui("main")
    )

server <- function(input, output, session) {
  # funcs <- list(convert = "DaparToolshed::convert",
  #               open_dataset = "DaparToolshed::open_dataset",
  #               open_demoDataset = "DaparToolshed::open_demoDataset",
  #               view_dataset = "DaparViz::view_dataset",
  #               infos_dataset = "DaparToolshed::infos_dataset")
  # 
  # funcs <- list(convert = "MagellanNTK::convert",
  #   open_dataset = "MagellanNTK::open_dataset",
  #   open_demoDataset = "MagellanNTK::open_demoDataset",
  #   view_dataset = "MagellanNTK::view_dataset",
  #   infos_dataset = "MagellanNTK::infos_dataset")
  #   
  
  #mainapp_server("main", funcs = funcs)
  mainapp_server("main")
}


app <- shiny::shinyApp(ui, server)
}


