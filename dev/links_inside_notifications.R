# https://stackoverflow.com/questions/35728623/linking-notification-to-tab-in-shinydashboard
# https://stackoverflow.com/questions/30637847/how-to-use-href-in-shiny-notificationitem
# https://community.rstudio.com/t/shinydashboard-notification-item-with-link-in-new-tab/37580

library(shiny)

get_noti=function(){
  notification <- notificationItem(icon = icon("exclamation-triangle"), status = "danger", paste0("noti"))
  notification$children[[1]] <- a(href="#shiny-tab-dashboard","onclick"=paste0("clickFunction('",paste0(substr(as.character(runif(1, 0, 1)),1,6),"noti"),"'); return false;"),list(notification$children[[1]]$children))
  return(notification)
}

server <- shinyServer(function(input, output, session) {
  output$dropdown=renderMenu({dropdownMenu(get_noti())})
  observeEvent(input$linkClicked,{
    print(input$linkClicked)
    updateTabItems(session,"sidemenu",selected = "dashboard")
    output$dropdown=renderMenu({dropdownMenu(get_noti())})
  })
})



library(shiny)
library(shinydashboard)
header <- dashboardHeader(dropdownMenuOutput('dropdown'), title = "Dashboard")
sidebar <- dashboardSidebar(
  sidebarMenu(id="sidemenu",
              menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
              menuItem("Test",
                       menuSubItem("test1", tabName = "test1", href = NULL, newtab = TRUE,
                                   icon = shiny::icon("angle-double-right"), selected = F),
                       menuSubItem("test2", tabName = "test2", href = NULL, newtab = TRUE,
                                   icon = shiny::icon("angle-double-right"), selected = T)
              )))
body <- dashboardBody(
  tags$script(HTML("function clickFunction(link){ 
                       Shiny.onInputChange('linkClicked',link);
    }")),
  tabItems(
    tabItem(tabName = "dashboard",
            h2("Dashboard tab content")
    ),
    
    tabItem(tabName = "test1",
            h2("Widgets tab1 content")
    ),
    
    tabItem(tabName = "test2",
            h2("Widgets tab2 content")
    )
  )
)



shinyApp(
  ui = dashboardPage(header, sidebar, body),
  server = server)