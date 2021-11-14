# << process_funcs.R>>
# Contains the functions scpecific to a module process,
# in addition to the file general_nav_funcs.R

Build_process_ui <- function(ns){

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
             column(width=1, shinyjs::disabled(
               actionButton(ns('nextBtn'),'>>',
                            class = PrevNextBtnClass,
                            style='font-size:80%'))
             )
    ),
    div(id = ns('Screens'),
        uiOutput(ns('SkippedInfoPanel')),
        uiOutput(ns('EncapsulateScreens_ui'))
    )
  )

}


