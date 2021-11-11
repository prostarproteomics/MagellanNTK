ResetChildren <- function(range, 
                          resetChildren
                          ){
  if(verbose) cat(paste0(id, '::ResetChildren()\n\n'))
  
  resetChildren[range] <- 1 + resetChildren[range]
  
  resetChildren
}


Build_pipeline_ui <- function(){
  renderUI({
    
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
                     mod_timeline_v_ui(ns('timelinev'))
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
    
  })
  
}