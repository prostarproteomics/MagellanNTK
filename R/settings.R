#' @title xxx
#' @description xxx
#' @export
GlobalSettings <- list(
  tl_v_next_icon = shiny::icon('arrow-down'),
  tl_v_prev_icon = shiny::icon('arrow-up'),
  tl_h_next_icon = shiny::icon('arrow-right'),
  tl_h_prev_icon = shiny::icon('arrow-left'),
  actionBtnClass = "btn-primary",
  optionsBtnClass = "info",
  redBtnClass = "btn-danger",
  PrevNextBtnClass = "btn-info",
  btn_success_color = "btn-success",
  btn_css_style = "display:inline-block; vertical-align: middle; padding: 7px;"
)

#' @export
#' 
default.config <- list(
  funcs = list(convert = "MagellanNTK::convert",
    open_dataset = "MagellanNTK::open_dataset",
    open_demoDataset = "MagellanNTK::open_demoDataset",
    view_dataset = "MagellanNTK::view_dataset",
    infos_dataset = "MagellanNTK::infos_dataset"),
  title = 'MagellanNTK',
  base_URL = ""
)
