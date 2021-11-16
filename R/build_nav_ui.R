

#' @title xxxx
#' @description xxx
#'
#' @param ns xxxx
#' 
#' 
Build_nav_v_ui <- function(ns){
  tagList(
    fluidRow(
      column(width=2, 
             wellPanel(
               div(style = 'padding: 10px',
                   div(style = btn_style,
                       shinyjs::disabled(
                         actionButton(ns('prevBtn'), '<<',
                                      class = PrevNextBtnClass,
                                      style='padding:4px; font-size:80%')
                       ),
                       actionButton(ns('rstBtn'), 'Reset',
                                    class = redBtnClass,
                                    style='padding:4px; font-size:80%')
                   ),
                   div(style = btn_style,
                       actionButton(ns('nextBtn'),'>>',
                                    class = PrevNextBtnClass,
                                    style='padding:4px; font-size:80%')
                   ),
                   uiOutput(ns('mod_timeline_v_ui'))
               )
             )),
      column(width=10,
             style=' padding-left: 20px;',
             wellPanel(
               div(id = ns('Screens'),
                   uiOutput(ns('SkippedInfoPanel')),
                   uiOutput(ns('EncapsulateScreens_ui'))
                   
               )
             )
      )
      
    )
  )
}





Build_nav_h_ui <- function(ns){
  
  tagList(
    fluidRow(style = 'display: flex; align-items: center;
                      justify-content: center;',
             column(width=1, shinyjs::disabled(
               actionButton(ns('prevBtn'), '<<',
                            class = PrevNextBtnClass,
                            style='font-size:80%')
             )),
             column(width=1, actionButton(ns('rstBtn'), 'Reset',
                                          class = redBtnClass,
                                          style='font-size:80%')),
             column(width=9, uiOutput(ns('show_TL'))),
             column(width=1, 
               actionButton(ns('nextBtn'),'>>',
                            class = PrevNextBtnClass,
                            style='font-size:80%'))
    ),
    div(id = ns('Screens'),
        uiOutput(ns('SkippedInfoPanel')),
        uiOutput(ns('EncapsulateScreens_ui'))
    )
  )
  
}