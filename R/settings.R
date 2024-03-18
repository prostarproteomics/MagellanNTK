#' #' #' @title xxx
#' #' #' @description xxx
#' #' #' @export
#' G_Settings <- list(
#'   tl_v_next_icon = shiny::icon('arrow-down'),
#'   tl_v_prev_icon = shiny::icon('arrow-up'),
#'   tl_h_next_icon = shiny::icon('arrow-right'),
#'   tl_h_prev_icon = shiny::icon('arrow-left'),
#'   actionBtnClass = "btn-primary",
#'   optionsBtnClass = "info",
#'   redBtnClass = "btn-danger",
#'   PrevNextBtnClass = "btn-info",
#'   btn_success_color = "btn-success",
#'   btn_css_style = "display:inline-block; vertical-align: middle; padding: 7px;",
#'   stepStatus = list(
#'     VALIDATED = 1,
#'     SKIPPED = -1,
#'     UNDONE = 0
#'   ),
#'   default_pos = list(
#'     VALIDATED = 1,
#'     SKIPPED = 1,
#'     UNDONE = 1
#'   ),
#'   listBrewerPalettes = c("Dark2 (qualit.)" = "Dark2",
#'     "Accent (qualit.)"="Accent",
#'     "Paired (qualit.)" = "Paired",
#'     "Pastel1 (qualit.)" = "Pastel1",
#'     "Pastel2 (qualit.)" = "Pastel2",
#'     "Set1 (qualit.)" = "Set1",
#'     "Set2 (qualit.)" = "Set2",
#'     "Set3 (qualit.)" = "Set3",
#'     "BrBG (diverging)"="BrBG",
#'     "PiYG (diverging)"=  "PiYG",
#'     "PRGn (diverging)" ="PRGn",
#'     "PuOr (diverging)" ="PuOr",
#'     "RdBu (diverging)"="RdBu",
#'     "RdGy (diverging)" ="RdGy",
#'     "RdYlBu (diverging)" ="RdYlBu",
#'     "RdYlGn (diverging)" ="RdYlGn",
#'     "Spectral (diverging)"="Spectral")
#' )
#' 
#' 
#' 
#' #' @title Default config values
#' #' @description xxx
#' #' @export
#' #' 
#'   default.funcs<- list(
#'     convert = "MagellanNTK::convert",
#'     open_dataset = "MagellanNTK::open_dataset",
#'     open_demoDataset = "MagellanNTK::open_demoDataset",
#'     view_dataset = "MagellanNTK::view_dataset",
#'     infos_dataset = "MagellanNTK::infos_dataset")
#' 
#'   default.base.URL <- system.file('app/md', package = 'MagellanNTK')
#' 
