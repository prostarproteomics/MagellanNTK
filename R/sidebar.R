

Insert_Dev_Sidebar <- function(){
  shinydashboardPlus::dashboardSidebar(
    shinydashboard::sidebarMenu(id = "sb_dev",
      #style = "position: fixed; overflow: visible;",
      # inactiveClass for import menus inactivation 
      # tags$head(tags$style(".inactiveLink {pointer-events: none; background-color: grey;}")),
      
      # Menus and submenus in sidebar
      #br(),
      shinydashboard::menuItem("Home", 
        tabName = "Home", 
        icon = icon("home"),
        selected = TRUE),
      hr(),
      # shinydashboard::menuItem("Data Manager",
      #          tabName = "dataManager",
      #          icon = icon("folder"),
      #          badgeLabel = "new", 
      #          badgeColor = "green"),
      shinydashboard::menuItem(h4('Dataset', style="color: green;"),
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
        ,shinydashboard::menuSubItem("Import (xlsx -> QF)",
          tabName = "convertDataset",
          icon = icon("folder")
          # ,badgeLabel = "new"
          # ,badgeColor = "green"
        ),
        shinydashboard::menuSubItem("Save As (QF)",
          tabName = "SaveAsQf",
          icon = icon("folder")
          # ,badgeLabel = "new"
          # ,badgeColor = "green"
        ),
        shinydashboard::menuSubItem("Export(QF -> xlsx)",
          tabName = "ExportQF",
          icon = icon("folder")
          # ,badgeLabel = "new"
          # ,badgeColor = "green"
        )
      ),
      hr(),
      shinydashboard::menuItem(h4('Workflow', style="color: green;"),
        shinydashboard::menuItem("Load",
          tabName = "openWorkflow",
          icon = icon("cogs")),
        shinydashboard::menuItem("Run", 
          tabName = "workflow", 
          icon = icon("cogs"))
      ),
      hr(),
      shinydashboard::menuItem(h4('Vizualize data', style="color: green;"),
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
      hr(),
      shinydashboard::menuItem(h4('Help', style="color: green;"),
        
        #icon = icon("question-circle"),
        shinydashboard::menuSubItem("Manual", tabName = "Manual"),
        shinydashboard::menuSubItem("Useful Links", tabName = "usefulLinks"),
        shinydashboard::menuSubItem("FAQ", tabName = "faq"),
        shinydashboard::menuSubItem("Bug Report", tabName = "bugReport"),
        shinydashboard::menuSubItem("Release Notes", 
          tabName = "releaseNotes", 
          icon = icon("clipboard"))
        # ,shinydashboard::menuSubItem("Check for Updates", 
        #             tabName = "checkUpdates", 
        #             icon = icon("wrench"))
      )
    )
  )
}




Insert_User_Sidebar <- function(){
  shinydashboardPlus::dashboardSidebar(
    shinydashboard::sidebarMenu(id = "sb_dev",
      #style = "position: fixed; overflow: visible;",
      # inactiveClass for import menus inactivation 
      # tags$head(tags$style(".inactiveLink {pointer-events: none; background-color: grey;}")),
      
      # Menus and submenus in sidebar
      #br(),
      shinydashboard::menuItem("Home", 
        tabName = "Home", 
        icon = icon("home"),
        selected = TRUE),
      hr(),
      # shinydashboard::menuItem("Data Manager",
      #          tabName = "dataManager",
      #          icon = icon("folder"),
      #          badgeLabel = "new", 
      #          badgeColor = "green"),
      shinydashboard::menuItem(h4('Dataset', style="color: green;"),
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
        ,shinydashboard::menuSubItem("Import (xlsx -> QF)",
          tabName = "convertDataset",
          icon = icon("folder")
          # ,badgeLabel = "new"
          # ,badgeColor = "green"
        ),
        shinydashboard::menuSubItem("Save As (QF)",
          tabName = "SaveAsQf",
          icon = icon("folder")
          # ,badgeLabel = "new"
          # ,badgeColor = "green"
        ),
        shinydashboard::menuSubItem("Export(QF -> xlsx)",
          tabName = "ExportQF",
          icon = icon("folder")
          # ,badgeLabel = "new"
          # ,badgeColor = "green"
        )
      ),
      hr(),
      shinydashboard::menuItem(h4('Workflow', style="color: green;"),
        # shinydashboard::menuItem("Load",
        #   tabName = "openWorkflow",
        #   icon = icon("cogs")),
        shinydashboard::menuItem("Run", 
          tabName = "workflow", 
          icon = icon("cogs"))
      ),
      hr(),
      shinydashboard::menuItem(h4('Vizualize data', style="color: green;"),
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
      hr(),
      shinydashboard::menuItem(h4('Help', style="color: green;"),
        
        #icon = icon("question-circle"),
        shinydashboard::menuSubItem("Manual", tabName = "Manual"),
        shinydashboard::menuSubItem("Useful Links", tabName = "usefulLinks"),
        shinydashboard::menuSubItem("FAQ", tabName = "faq"),
        shinydashboard::menuSubItem("Bug Report", tabName = "bugReport"),
        shinydashboard::menuSubItem("Release Notes", 
          tabName = "releaseNotes", 
          icon = icon("clipboard"))
        # ,shinydashboard::menuSubItem("Check for Updates", 
        #             tabName = "checkUpdates", 
        #             icon = icon("wrench"))
      )
    )
  )
}