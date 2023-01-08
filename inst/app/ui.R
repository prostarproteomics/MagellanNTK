library(shinydashboard)
library(shinyjs)
library(shinyFiles)


shinyUI(
dashboardPage(
    
    dashboardHeader(title="MagellanNTK",
                    dropdownMenuOutput("messageMenu")
                    ),
    
    
    dashboardSidebar(
      useShinyjs(),
      sidebarMenu(
        # Menus and submenus in sidebar
        #br(),
        #menuItem("Home", tabName = "Home", selected = TRUE),
        #hr(),
        menuItem("Workflow", tabName = "open_workflow", icon = icon("cogs")),
        hr(),
        menuItem("Run workflow", tabName = "run_workflow", icon = icon("cogs")),
        
        menuItem("Data Manager",
          icon = icon("folder"),
          startExpanded = FALSE,
          menuSubItem("Open dataset", tabName = "openFile"),
          menuSubItem("Convert Data", tabName = "convertData"),
          menuSubItem("Export results", tabName = "export")
        ),
        hr(),
        
        menuItem("Plots", tabName = "tab_plots", icon = icon("cogs")),
        hr(),
        menuItem("Help",
          icon = icon("question-circle"),
          menuSubItem("Useful Links", tabName = "usefulLinks"),
          menuSubItem("FAQ", tabName = "faq"),
          menuSubItem("Bug Report", tabName = "bugReport")
          #menuSubItem("Global Settings", tabName = "globalSettings", icon = icon("cogs")),
          #menuSubItem("Release Notes", tabName = "releaseNotes", icon = icon("clipboard")),
          #menuSubItem("Check for Updates", tabName = "checkUpdates", icon = icon("wrench"))
        )
      )
     ), 
    
    dashboardBody(
      useShinyjs(),
        # body content
        tabItems(
          tabItem(tabName = "Home", class="active", h3('home')),
          tabItem(tabName = "open_workflow", Load_Workflow_ui("open_wf")),
          tabItem(tabName = "openFile", mod_openfile_ui('openwf')),
          tabItem(tabName = "convertData", mod_convert_ui('convert')),
          tabItem(tabName = "tab_plots",  mod_plots_ui('plots')),
          tabItem(tabName = "tab_export",  mod_export_ui('export')),
          
          tabItem(tabName = "convert", uiOutput('show_convert')),
          tabItem(tabName = "run_workflow", uiOutput('run_workflowUI')),
          
          
          #tabItem(tabName = "globalSettings", h3('Global settings'),
          #   mod_settings_ui('global_settings')),
          # tabItem(tabName = "releaseNotes", h3('Release notes'),
          #   mod_release_notes_ui('rl')),
          # tabItem(tabName = "checkUpdates", h3('Check for updates'),
          #   mod_check_updates_ui('check_updates')),
          tabItem(tabName = "usefulLinks", mod_insert_md_ui('links')),
          tabItem(tabName = "faq", mod_insert_md_ui('faq')),
          tabItem(tabName = "bugReport", mod_bug_report_ui("bug_report"))
        )
      )
    )
)