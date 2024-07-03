
#' @export
Insert_Dev_Sidebar <- function(){
 # dashboardSidebar(
    #tags$style(".sidebar-menu li a { height: 40px; color: grey;}"), 
    
    shinydashboard::sidebarMenu(id = "sb_dev",
      #tags$style(".sidebar-menu li a { height: 40px; color: grey;}"), 
      minified = TRUE, collapsed = TRUE,
      #style = "position: fixed; overflow: visible;",
      # inactiveClass for import menus inactivation 
      # tags$head(tags$style(".inactiveLink {pointer-events: none; background-color: grey;}")),
      
      # Menus and submenus in sidebar
      #br(),
      shinydashboard::menuItem("Home", 
        tabName = "Home", 
        icon = icon("home"),
        selected = TRUE),
      #hr(),
      # shinydashboard::menuItem("Data Manager",
      #          tabName = "dataManager",
      #          icon = icon("folder"),
      #          badgeLabel = "new", 
      #          badgeColor = "green"),
      shinydashboard::menuItem(
        h4('Dataset', style="color: lightgrey;"),
        shinydashboard::menuSubItem("Open (qf)",
          tabName = "openDataset",
          icon = icon("folder")
          # ,badgeLabel = "new"
          # ,badgeColor = "green"
        )
        # ,shinydashboard::menuItem("Demo dataset",
        #   tabName = "demoDataset",
        #   icon = icon("folder")
        #   # ,badgeLabel = "new"
        #   # ,badgeColor = "green"
        #   )
        ,shinydashboard::menuSubItem("Import", tabName = "convertDataset"),
        shinydashboard::menuSubItem("Save As", tabName = "SaveAs"),
        shinydashboard::menuSubItem("Build report", tabName = "BuildReport")
      ),
      #hr(),
      shinydashboard::menuItem(h4('Workflow', style = "color: lightgrey;"),
        shinydashboard::menuSubItem("Load", tabName = "openWorkflow"),
        shinydashboard::menuSubItem("Run", tabName = "workflow"),
        shinydashboard::menuSubItem("Manual", tabName = "Manual"),
        shinydashboard::menuSubItem("FAQ", tabName = "faq"),
        shinydashboard::menuSubItem("Release Notes", tabName = "releaseNotes")
      ),
      #hr(),
      shinydashboard::menuItem(h4('Vizualize data', style = "color: lightgrey;"),
        shinydashboard::menuSubItem("Info", 
          tabName = "infosDataset", 
          icon = icon("cogs")
          # ,badgeLabel = "new"
          # ,badgeColor = "green"
        ),
        shinydashboard::menuSubItem("EDA", 
          tabName = "eda", 
          icon = icon("cogs")
          # ,badgeLabel = "new"
          # ,badgeColor = "green"
        )
      ),
      #hr(),
      shinydashboard::menuItem(
        h4('Help', style = "color: lightgrey;")
        
        #shinydashboard::menuSubItem("Useful Links", tabName = "usefulLinks")
        #shinydashboard::menuSubItem("Bug Report", tabName = "bugReport")
        # ,shinydashboard::menuSubItem("Check for Updates", 
        #             tabName = "checkUpdates", 
        #             icon = icon("wrench"))
      )
    )
  #)
}



#' @export
Insert_User_Sidebar <- function(){
  #dashboardSidebar(
    #tags$style(".sidebar-menu li a { height: 40px; color: grey;}"), 
    #minified = TRUE, collapsed = TRUE,
    shinydashboard::sidebarMenu(id = "sb_user",
      #tags$style(".sidebar-menu li a { height: 40px; color: grey;}"), 
      
      #style = "position: fixed; overflow: visible;",
      # inactiveClass for import menus inactivation 
      # tags$head(tags$style(".inactiveLink {pointer-events: none; background-color: grey;}")),
      
      # Menus and submenus in sidebar
      #br(),
      shinydashboard::menuItem("Home", 
        tabName = "Home", 
        icon = icon("home"),
        selected = TRUE),
      #hr(),
      
      
      shinydashboard::menuItem(
        h4('Dataset', style="color: lightgrey;"),
        shinydashboard::menuSubItem("Open file",
          tabName = "openDataset"),
        shinydashboard::menuSubItem("Save As", tabName = "SaveAs"),
        shinydashboard::menuSubItem("Import data",
          tabName = "convertDataset",
          icon = icon("folder")
          ),
        shinydashboard::menuSubItem("Build report", tabName = "BuildReport")
      ),
      #hr(),
      shinydashboard::menuItem(h4('Workflow', style="color: lightgrey;"),
        # shinydashboard::menuItem("Load",
        #   tabName = "openWorkflow",
        #   icon = icon("cogs")),
        shinydashboard::menuSubItem("Run", 
          tabName = "workflow", 
          icon = icon("cogs")),
        shinydashboard::menuSubItem("Manual", tabName = "Manual"),
        shinydashboard::menuSubItem("FAQ", tabName = "faq"),
        shinydashboard::menuSubItem("Release Notes", tabName = "releaseNotes")
      ),
      #hr(),
      shinydashboard::menuItem(h4('Vizualize data', style="color: lightgrey;"),
        shinydashboard::menuSubItem("Info", 
          tabName = "infosDataset", 
          icon = icon("cogs")
          ),
        shinydashboard::menuSubItem("EDA", tabName = "eda")
      ),
      #hr(),
      shinydashboard::menuItem(h4('Help', style="color: lightgrey;"),
        
        #icon = icon("question-circle"),
        shinydashboard::menuSubItem("Useful Links", tabName = "usefulLinks"),
        shinydashboard::menuSubItem("Bug Report", tabName = "bugReport")
        )
    )
 # )
}