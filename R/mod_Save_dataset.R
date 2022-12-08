#' @title Load dataset shiny module
#'
#' @description  A shiny Module to load a dataset.
#' @name mod_Save_Dataset
#' 
#' @example inst/examples/test_mod_save_dataset.R
NULL

#' @param id xxx
#' @rdname mod_Save_Dataset
#'
#' @export
#'
mod_Save_Dataset_ui <- function(id) {
}


#' @param id xxx
#' @param data xxx
#' 
#' @return xxxxx
#'
#' @rdname mod_Save_Dataset
#'
#' @export
#'
mod_Save_Dataset_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    rv <- reactiveValues(
      dataIn = NULL,
      dataOut = NULL)
    
    observe(data(), {
      saveRDS(data(), file = 'temp.RData')
    })
  })
}
