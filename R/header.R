#' @export
InsertDropdownMenu<- function(){
  
    shinydashboard::dropdownMenu(
      type = "tasks",
      badgeStatus = "danger"
      # taskItem(value = 20, color = "aqua", "Refactor code"),
      # taskItem(value = 40, color = "green", "Design new layout"),
      # taskItem(value = 60, color = "yellow", "Another task"),
      # taskItem(value = 80, color = "red", "Write documentation")
      ,shinydashboard::menuItem("Home 2", 
        tabName = "Home2", 
        icon = icon("home"),
        selected = TRUE)
      ,shinydashboard::menuItem("User manual", 
        tabName = "usermanual", 
        icon = icon("home"),
        selected = TRUE)
    )

}