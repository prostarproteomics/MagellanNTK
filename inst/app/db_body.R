body <- shinydashboard::dashboardBody(
  
  dashboardthemes::shinyDashboardThemes(
    theme = "blue_gradient"
  ),
  
  tagList(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    
    useShinyjs(),
    
    # body content
    tabItems(
      tabItem(tabName = "Home", class="active",
        tagList(
          h3("Home"),
          shinyjs::hidden(h3("Home2")),
          mod_homepage_ui("homepage")
        )
        #mod_homepage_ui(ns('homepage'))
      ),
      # tabItem(tabName = "openFile", h3("Open QFeature file"),
      #         mod_import_file_from_ui("open_file")),
      tabItem(tabName = "convert", 
        tagList(
          h3("Convert datas"),
          uiOutput('show_convert')
        )
      ),
      tabItem(tabName = "demoData", 
        tagList(
          h3("Load a demo dataset"),
          div(
            div(
              #style="display:inline-block; vertical-align: middle; padding-right: 20px;",
              #mod_choose_pipeline_ui("pipe")
            ),
            div(
              #style="display:inline-block; vertical-align: middle; padding-right: 20px;",
              #shinyjs::hidden(
              # div(id='div_demoDataset',
              #mod_open_demoDataset_ui('demo_data')
              # )
              # )
            ),
            div(
              style="display:inline-block; vertical-align: middle; padding-right: 20px;",
              actionButton('load_dataset_btn', 'Load dataset', class=GlobalSettings$actionBtnClass)
            )
          )
        )
      ),
      
      tabItem(tabName = "daparviz", 
        tagList(
          h3("Dapar viz")
          #DaparViz::mod_all_ds_ui('daparviz')
        )
      ),
      
      
      tabItem(tabName = "export", h3("Export")), # export module not yet
      #tabItem(tabName = "globalSettings", h3('Global settings'),
      #   mod_settings_ui('global_settings')),
      # tabItem(tabName = "releaseNotes", h3('Release notes'),
      #   mod_release_notes_ui('rl')),
      # tabItem(tabName = "checkUpdates", h3('Check for updates'),
      #   mod_check_updates_ui('check_updates')),
      tabItem(tabName = "usefulLinks",
        mod_insert_md_ui("http://www.prostar-proteomics.org/md/links.md")
      ),
      tabItem(tabName = "faq",
        mod_insert_md_ui("http://www.prostar-proteomics.org/md/links.md")
      ),
      tabItem(tabName = "bugReport", 
        h3('Bug report')
        #mod_bug_report_ui("bug_report")
      ),
      tabItem(tabName = "pipeline", 
        h3('Pipeline')
        #uiOutput('show_pipeline')
      )
    )
    # uiOutput('show_pipeline')
  )
)
