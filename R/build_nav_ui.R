

#' @title Timeline UI
#'
#' @description These functions display the timeline UI with a vertical nor 
#' horizontal layout
#' 
#' @return A shiny UI
#'
#' @author Samuel Wieczorek
#' 
#' @name Build_nav_X_ui
#' 
#' @param ns The namespace variable from the module that called this function.
#' 
#' @examples NULL
#' 
NULL


#' @rdname Build_nav_X_ui
#'
Build_nav_v_ui <- function(ns) {
    tagList(
        fluidRow(
            column(
                width = 2,
                wellPanel(
                    div(
                        style = "padding: 10px",
                        div(
                            style = btn_style,
                            shinyjs::disabled(
                                actionButton(ns("prevBtn"),
                                    GlobalSettings$tl_v_prev_icon,
                                    class = GlobalSettings$PrevNextBtnClass,
                                    style = "padding:4px; font-size:80%"
                                )
                            ),
                            actionButton(ns("rstBtn"), "Reset",
                                class = GlobalSettings$redBtnClass,
                                style = "padding:4px; font-size:80%"
                            )
                        ),
                        div(
                            style = btn_style,
                            actionButton(ns("nextBtn"), 
                                GlobalSettings$tl_v_next_icon,
                                class = GlobalSettings$PrevNextBtnClass,
                                style = "padding:4px; font-size:80%"
                            )
                        ),
                        uiOutput(ns("show_TL"))
                    )
                )
            ),
            column(
                width = 10,
                style = " padding-left: 20px;",
                wellPanel(
                    div(
                        id = ns("Screens"),
                        uiOutput(ns("SkippedInfoPanel")),
                        uiOutput(ns("EncapsulateScreens_ui"))
                    )
                )
            )
        )
    )
}



#' @rdname Build_nav_X_ui
#'
Build_nav_h_ui <- function(ns) {
    tagList(
        fluidRow(
            style = "display: flex; align-items: center;
            justify-content: center;",
            column(width = 1, shinyjs::disabled(
                actionButton(ns("prevBtn"),
                    GlobalSettings$tl_h_prev_icon,
                    class = GlobalSettings$PrevNextBtnClass,
                    style = "font-size:80%"
                )
            )),
            column(width = 1, actionButton(ns("rstBtn"), "Reset",
                class = GlobalSettings$redBtnClass,
                style = "font-size:80%"
            )),
            column(width = 9, uiOutput(ns("show_TL"))),
            column(
                width = 1,
                actionButton(ns("nextBtn"),
                    GlobalSettings$tl_h_next_icon,
                    class = GlobalSettings$PrevNextBtnClass,
                    style = "font-size:80%"
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
