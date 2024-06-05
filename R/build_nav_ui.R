

#' @title Timeline UI
#'
#' @description These functions display the timeline UI with a vertical nor 
#' horizontal layout
#' 
#' @return A shiny UI
#'
#' @author Samuel Wieczorek
#' 
#' @param layout xxx
#' @param ns The namespace variable from the module that called this function.
#' 
#' @name Build_nav_X_ui
#' 
#' 
#' 
NULL



#' @rdname Build_nav_X_ui
#' 
#' @export
#'
DisplayWholeUI <- function(ns, layout = 'h'){
  
  do.call(paste0("Build_nav_", layout, "_ui"), list(ns))

}

#' @rdname Build_nav_X_ui
#' 
#' @export
#'
Build_nav_v_ui <- function(ns) {
  btn_style <- "display:inline-block; vertical-align: middle; padding: 7px"
    tagList(
        fluidRow(
            column(width = 2,
                #wellPanel(
                    div(
                        style = "padding: 10px",
                        div(style = btn_style,
                            shinyjs::disabled(
                                actionButton(ns("prevBtn"),
                                    tl_v_prev_icon,
                                    class = PrevNextBtnClass,
                                    style = "padding:4px; font-size:80%"
                                )
                            ),
                            actionButton(ns("rstBtn"), "Reset",
                                class = redBtnClass,
                                style = "padding:4px; font-size:80%"
                            )
                        ),
                        div(style = btn_style,
                            actionButton(ns("nextBtn"), 
                                tl_v_next_icon,
                                class = PrevNextBtnClass,
                                style = "padding:4px; font-size:80%"
                            )
                        ),
                        uiOutput(ns("show_TL"))
                   # )
                )
            ),
            column(width = 10,
                style = " padding-left: 20px;",
                #wellPanel(
                    div(
                        id = ns("Screens"),
                        uiOutput(ns("SkippedInfoPanel")),
                        uiOutput(ns("EncapsulateScreens_ui"))
                    )
               # )
            )
        )
    )
}



#' @rdname Build_nav_X_ui
#' 
#' @export
#'
Build_nav_h_ui <- function(ns) {
    tagList(
        fluidRow(
            style = "display: flex; align-items: top;
            justify-content: center;",
            column(width = 1, shinyjs::disabled(
                actionButton(ns("prevBtn"),
                    tl_h_prev_icon,
                    class = PrevNextBtnClass,
                  style = "font-size:60%"
                )
            )),
            column(width = 1, 
                   actionButton(ns("rstBtn"), "Reset",
                                class = redBtnClass,
                                style = "font-size:60%")
                   ),
            column(width = 9, uiOutput(ns("show_TL"))),
            column(width = 1,
                actionButton(ns("nextBtn"),
                    tl_h_next_icon,
                    class = PrevNextBtnClass,
                    style = "font-size:60%"
                )
            )
        ),
        div(
            id = ns("Screens"),
            uiOutput(ns("SkippedInfoPanel")),
            uiOutput(ns("EncapsulateScreens_ui"))
        )
    )
}
