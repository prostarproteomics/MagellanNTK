header <- shinydashboard::dashboardHeader(
  # title on top right, shrinks when sidebar collapsed
  tags$li(class = "dropdown",
    #tags$style(".main-header {max-height: 20px}"),
    #tags$style(".main-header .logo {height: 20px;}"),
    #tags$style(".sidebar-toggle {height: 20px; padding-top: 1px !important;}"),
    #tags$style(".navbar {min-height:20px !important}"),
    tags$style(".skin-blue .main-header .navbar {background-color: green;}")
  ),
  ### changing logo
  title = dashboardthemes::shinyDashboardLogo(
    theme = "blue_gradient",
    boldText = "xxx"
  ),
  # title = tagList(
  #   tags$span(
  #     class = "logo-mini", style =  "font-size : 14px","Prostar"),
  #   tags$span(
  #     class = "logo-lg", "Prostar")
  # ),
  # button to mimic data loaded
  tags$li(class="dropdown",
    actionButton('browser', 'Browser()')
  ),
  # links Prostar website and github
  tags$li(class="dropdown",
    a(href="http://www.prostar-proteomics.org/",
      img(src="logo.png",
        title="Prostar website",
        height="17px")))
)