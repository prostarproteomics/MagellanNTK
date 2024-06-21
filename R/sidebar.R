

InsertSidebar <- function(usermod = 'dev'){
  switch(usermod,
    dev = InsertSidebar_dev(),
    user = InsertSidebar_user())
}


InsertSidebar_dev <- function(){
  shinydashboard::sidebarMenu(id = "sb",
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
    h4('Dataset', style="color: green;"),
    shinydashboard::menuItem("Open",
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
    ,shinydashboard::menuItem("Convert",
      tabName = "convertDataset",
      icon = icon("folder")
      # ,badgeLabel = "new"
      # ,badgeColor = "green"
    ),
    hr(),
    h4('Workflow', style="color: green;"),
    shinydashboard::menuItem("Open",
      tabName = "openWorkflow",
      icon = icon("cogs")),
    shinydashboard::menuItem("Run", 
      tabName = "workflow", 
      icon = icon("cogs")),
    hr(),
    shinydashboard::menuItem(h4('Vizualize data', style="color: green;"),
      shinydashboard::menuSubItem("Infos", 
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
}




InsertSidebar_user <- function(){
  shinydashboard::sidebarMenu(id = "sb",
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
    h4('Dataset', style="color: green;"),
    shinydashboard::menuItem("Open",
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
    ,shinydashboard::menuItem("Convert",
      tabName = "convertDataset",
      icon = icon("folder")
      # ,badgeLabel = "new"
      # ,badgeColor = "green"
    ),
    hr(),
    h4('Workflow', style="color: green;"),
    # shinydashboard::menuItem("Open",
    #   tabName = "openWorkflow",
    #   icon = icon("cogs")),
     shinydashboard::menuItem("Run", 
      tabName = "workflow", 
      icon = icon("cogs")),
    hr(),
    shinydashboard::menuItem(h4('Vizualize data', style="color: green;"),
      shinydashboard::menuSubItem("Infos", 
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
}