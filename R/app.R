#' @title Main Shiny application
#' @description xxx
#' 
#' @param config A list optionaly containing th following items:
#' * title: xxx
#' * base_URL: xxx
#' 
#' @export
#' @examples
#' if(interactive()){
#' 
#' # launch without initial config
#' MagellanNTK()
#' 
#' # launch with config
#' funcs <- list(convert = "DaparToolshed::convert",
#' open_dataset = "DaparToolshed::open_dataset",
#' open_demoDataset = "DaparToolshed::open_demoDataset",
#' view_dataset = "DaparViz::view_dataset",
#' infos_dataset = "DaparToolshed::infos_dataset")
#' 
#' funcs <- list(convert = "MagellanNTK::convert",
#'   open_dataset = "MagellanNTK::open_dataset",
#'   open_demoDataset = "MagellanNTK::open_demoDataset",
#'   view_dataset = "MagellanNTK::view_dataset",
#'   infos_dataset = "MagellanNTK::infos_dataset")
#' 
#' MagellanNTK(funcs)
#' }
#' 
MagellanNTK <- function(config = default.config) {
  
  file_path_ui <- system.file("app/ui.R", package = "MagellanNTK")
  file_path_server <- system.file("app/server.R", package = "MagellanNTK")
  if (!nzchar(file_path_ui) || !nzchar(file_path_server)) 
    stop("Shiny app not found")
  
  ui <- server <- NULL # avoid NOTE about undefined globals
  source(file_path_ui, local = TRUE)
  source(file_path_server, local = TRUE)
  server_env <- environment(server)
  
  # Here you add any variables that your server can find
  # server_env$funcs <- config$funcs
  # server_env$title <- config$title
  # server_env$base_URL <- config$base_URL
  # server_env$base_URL <- config$base_URL
  # server_env$base_URL <- config$base_URL
  # server_env$VALIDATED <- 1
  
  server_env <- list(
    funcs = config$funcs,
    title = config$title,
    base_URL = config$base_URL,
    tl_v_next_icon = shiny::icon('arrow-down'),
    tl_v_prev_icon = shiny::icon('arrow-up'),
    tl_h_next_icon = shiny::icon('arrow-right'),
    tl_h_prev_icon = shiny::icon('arrow-left'),
    actionBtnClass = "btn-primary",
    optionsBtnClass = "info",
    redBtnClass = "btn-danger",
    PrevNextBtnClass = "btn-info",
    btn_success_color = "btn-success",
    btn_css_style = "display:inline-block; vertical-align: middle; padding: 7px;",
    stepStatus = list(
      VALIDATED = 1,
      SKIPPED = -1,
      UNDONE = 0
      ),
    default_pos = list(
      VALIDATED = 1,
      SKIPPED = 1,
      UNDONE = 1
      ),
    listBrewerPalettes = c("Dark2 (qualit.)" = "Dark2",
      "Accent (qualit.)"="Accent",
      "Paired (qualit.)" = "Paired",
      "Pastel1 (qualit.)" = "Pastel1",
      "Pastel2 (qualit.)" = "Pastel2",
      "Set1 (qualit.)" = "Set1",
      "Set2 (qualit.)" = "Set2", 
      "Set3 (qualit.)" = "Set3",
      "BrBG (diverging)"="BrBG",
      "PiYG (diverging)"=  "PiYG",
      "PRGn (diverging)" ="PRGn",
      "PuOr (diverging)" ="PuOr",
      "RdBu (diverging)"="RdBu",
      "RdGy (diverging)" ="RdGy",
      "RdYlBu (diverging)" ="RdYlBu",
      "RdYlGn (diverging)" ="RdYlGn",
      "Spectral (diverging)"="Spectral")
  )
  
  app <- shiny::shinyApp(ui_MagellanNTK, server_MagellanNTK)
  shiny::runApp(app)
  
  
}