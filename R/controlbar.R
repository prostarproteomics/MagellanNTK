InsertControlbar <- function(){
  shinydashboardPlus::dashboardControlbar(
    skin = "dark",
    shinydashboardPlus::controlbarMenu(
      shinydashboardPlus::controlbarItem(
        title = "Configure",
        icon = icon("desktop"),
        active = TRUE,
        actionLink(ns('browser'), 'Console'),
        mod_modalDialog_ui(ns('loadPkg_modal'))
      ),
      shinydashboardPlus::controlbarItem(
        icon = icon("paint-brush"),
        title = "Settings",
        mod_settings_ui(ns('global_settings'))
      )
      # ,shinydashboardPlus::controlbarItem(
      #   icon = icon("paint-brush"),
      #   title = "Skin",
      #   shinydashboardPlus::skinSelector()
      # )
    )
  )
}