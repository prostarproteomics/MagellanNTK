#' @export
InsertBody <- function(id, session){
  ns <- session$ns
  shinydashboard::dashboardBody(
    # some styling
    tags$head(
      #tags$style(".content-wrapper {background-color: white;}"),
      
      # .path <- file.path(system.file('app/www/css', package = 'MagellanNTK'),'prostar.css'),
      # includeCSS(.path),
      # .path_sass <- file.path(system.file('app/www/css', package = 'MagellanNTK'),'sass-size.scss'),
      # tags$style(sass::sass(
      #   sass::sass_file(.path_sass),
      #   sass::sass_options(output_style = "expanded")
      # )),
      
      # tags$style(
      #   rel = "stylesheet",
      #   type = "text/css",
      #   href = "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/styles/qtcreator_dark.min.css"
      # ),
      # tags$script(
      #   src = "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/highlight.min.js"
      # ),
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
      shinydashboard::tabItems(
        
        shinydashboard::tabItem(tabName = "Home2", class="active",
          mod_homepage_ui(ns('home2'))),
        shinydashboard::tabItem(tabName = "usermanual", class="active",
          insert_md_ui(ns('usermanual'))),
        
        
        shinydashboard::tabItem(tabName = "Home", class="active", 
          actionLink(ns('launch_demo'), 'New to MagellanNTK? Launch demo' ),
          mod_homepage_ui(ns('home'))),
        #tabItem(tabName = "dataManager", 
        #uiOutput(ns('dataManager_UI'))),
        shinydashboard::tabItem(tabName = "openDataset", 
          uiOutput(ns('open_dataset_UI')), width = '200px' ,
          uiOutput(ns('infos_dataset_UI')), width = '200px' 
        ),
        
        shinydashboard::tabItem(tabName = "convertDataset", 
          uiOutput(ns('open_convert_dataset_UI'))),
        
        shinydashboard::tabItem(tabName = "eda", 
          uiOutput(ns('EDA_UI'))),
        
        shinydashboard::tabItem(tabName = "tools", 
          uiOutput(ns('tools_UI'))),
        
        shinydashboard::tabItem(tabName = "export", 
          h3("Export")), # export module not yet
        
        shinydashboard::tabItem(tabName = "openWorkflow", 
          uiOutput(ns('open_workflow_UI'))),
        shinydashboard::tabItem(tabName = "workflow", 
          uiOutput(ns('workflow_UI'))),
        
        
        #tabItem(tabName = "globalSettings", mod_settings_ui(ns('global_settings'))),
        shinydashboard::tabItem(tabName = "releaseNotes", 
          mod_release_notes_ui(ns('rl'))),
        # tabItem(tabName = "checkUpdates", 
        #   mod_check_updates_ui(ns('check_updates'))),
        shinydashboard::tabItem(tabName = "usefulLinks", 
          insert_md_ui(ns('links_MD'))),
        shinydashboard::tabItem(tabName = "faq", 
          insert_md_ui(ns('FAQ_MD'))),
        shinydashboard::tabItem(tabName = "bugReport", 
          mod_bug_report_ui(ns("bug_report"))),
        shinydashboard::tabItem(tabName = "pipeline", 
          uiOutput(ns('show_pipeline')))
      )
      
    ))
}