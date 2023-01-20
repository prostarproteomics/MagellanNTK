library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(dashboardthemes)
library(shinyjs)
library(shinyFiles)



# header_img <- div(
#   img(src="https://www.google.com/images/branding/googlelogo/1x/googlelogo_color_272x92dp.png", height="45px"),
#   div(
#     class = "my-title",
#     h4('Title'), h5('Subtitle'),
#     tags$style(".my-title :is(h4, h5){color: white; font-weight: bold;}")
#   ),
#   style = "display: flex;"
# )

# header <-  htmltools::tagQuery(dashboardHeader(title = ""))
# header <- header$
#   addAttrs(style = "position: relative")$ # add some styles to the header 
#   find(".navbar.navbar-static-top")$ # find the header right side
#   append(header_img)$ # inject our img
#   allTags()



ui <- shinydashboardPlus::dashboardPage(
  
  header = shinydashboardPlus::dashboardHeader(
      
      #tags$li(class='dropdown', uiOutput('title')),

      leftUi = tagList(
        tags$li(class='dropdown', uiOutput('menuTitle'))
        ),
      

      # dropdownMenu(
      #   type = "messages", 
      #   badgeStatus = "success",
      #   messageItem(from = "Support Team", message = "This is the content of a message.", time = "5 mins"),
      #   messageItem(from = "Support Team", message = "This is the content of another message.", time = "2 hours"),
      #   messageItem(from = "New User", message = "Can I get some help?", time = "Today")
      # ),
      # tagList(
      #   tags$li(uiOutput('title')),
      #   tags$li(shinyjs::hidden(actionButton('browser', 'browser()')))
      #   ),
      
    # dropdownButton(
    #     label = "Controls",
    #     icon = icon("gear"),
    #     status = "primary",
    #     circle = FALSE,
    #     sliderInput(
    #       inputId = "n",
    #       label = "Number of observations",
    #       min = 10, max = 100, value = 30
    #     ),
    #     prettyToggle(
    #       inputId = "na",
    #       label_on = "NAs kept",
    #       label_off = "NAs removed",
    #       icon_on = icon("check"),
    #       icon_off = icon("trash")
    #     )
    #   ),

        dropdownMenuOutput("messageMenu")
      ),

  #uiOutput('header'),
  #header,
  shinydashboardPlus::dashboardSidebar(
      useShinyjs(),
      sidebarMenu(
        # Menus and submenus in sidebar
        #br(),
        #menuItem("Home", tabName = "Home", selected = TRUE),
        #hr(),
        menuItem("Workflow", 
                 menuSubItem('Open', tabName = "tab_load_workflow"),
                 menuSubItem('FAQ', tabName = "tab_wf_faq"),
                 menuSubItem('Links', tabName = "tab_wf_links")
        ),
        menuItem("Open file", tabName = "tab_openfile"),
        menuItem("Run workflow", tabName = "run_workflow", icon = icon("cogs")),
        menuItem("Export to", tabName = "tab_export"),
        menuItem("EDA", tabName = "tab_EDA", icon = icon("cogs")),
        
        hidden(
          div(id = 'Help_menu',
              menuItem("Help for MagellanNTK",
          icon = icon("question-circle"),
          menuSubItem("About", tabName = "tab_about"),
          menuSubItem("FAQ", tabName = "tab_faq"),
          menuSubItem("Bug Report", tabName = "bugReport")
          #menuSubItem("Global Settings", tabName = "globalSettings", icon = icon("cogs")),
          #menuSubItem("Release Notes", tabName = "releaseNotes", icon = icon("clipboard")),
          #menuSubItem("Check for Updates", tabName = "checkUpdates", icon = icon("wrench"))
        )
      )
      )
      )
     ), 
    


controlbar = shinydashboardPlus::dashboardControlbar(
  skin = "dark",
  controlbarMenu(
    controlbarItem(
      title = "Dev mode",
      icon = icon("desktop"),
      active = TRUE,
      checkboxInput(inputId = "devmode", label = "dev mode", value = FALSE),
      hidden(actionLink('browser', 'browser()')),
      hidden(
        tags$li(id = 'githubLink',
                class='dropdown',
                tags$a(href="https://github.com/prostarproteomics/MagellanNTK", 
                       target="_blank", icon("github"),  title="GitHub"))
      )
      
    ),
    controlbarItem(
      icon = icon("paint-brush"),
      title = "Tab 2",
      numericInput(
        inputId = "inputsidebar2", 
        label = "Observations:", 
        value = 10, 
        min = 1, 
        max = 100
      )
    )
  )
),


    dashboardBody(
      useShinyjs(),
      
        # body content
        tabItems(
          tabItem(tabName = "Home", class="active", h3('home')),
          tabItem(tabName = "tab_load_workflow", 
                  fluidRow(
                    box(mod_load_workflow_ui("openwf"))
                  )),
          tabItem(tabName = "tab_wf_links", uiOutput('wf_links_UI')),
          tabItem(tabName = "tab_wf_faq", uiOutput('wf_faq_UI')),
          
          tabItem(tabName = "tab_openfile", uiOutput('openFileUI')),
          tabItem(tabName = "tab_convertData", uiOutput('convertUI')),
          tabItem(tabName = "tab_EDA", uiOutput('EDAUI')),
          tabItem(tabName = "tab_export", uiOutput('exportUI')),
          
          tabItem(tabName = "run_workflow", uiOutput('run_workflowUI')),
          
          
          #tabItem(tabName = "globalSettings", h3('Global settings'),
          #   mod_settings_ui('global_settings')),
          # tabItem(tabName = "releaseNotes", h3('Release notes'),
          #   mod_release_notes_ui('rl')),
          # tabItem(tabName = "checkUpdates", h3('Check for updates'),
          #   mod_check_updates_ui('check_updates')),
          tabItem(tabName = "tab_about", mod_insert_md_ui('magellan_about')),
          tabItem(tabName = "tab_faq", mod_insert_md_ui('magellan_faq')),
          tabItem(tabName = "bugReport", mod_bug_report_ui("bug_report"))
        )
      )
    )