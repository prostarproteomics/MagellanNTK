ResetChildren <- function(range, 
                          resetChildren
                          ){
  cat('ResetChildren()\n\n')
  
  resetChildren[range] <- 1 + resetChildren[range]
  
  return(resetChildren)
}




Update_Data2send_Vector <- function(rv){
  # One only update the current position because the vector has been entirely
  # initialized to NULL so the other processes are already ready to be sent
  ind.last.validated <- GetMaxValidated_BeforePos(rv = rv)
  if (is.null(ind.last.validated))
    data <- rv$temp.dataIn
  else
    data <- Keep_Datasets_from_Object(object = rv$dataIn,
                                      range = seq_len(ind.last.validated + rv$original.length -1)
    )
  return(data)
}




PrepareData2Send <- function(rv,
                             pos
                              ){
  cat('::PrepareData2Send()\n\n')
  #browser()
  # Returns NULL to all modules except the one pointed by the current position
  # Initialization of the pipeline : one send dataIn() to the
  # first module
  
  # The dataset to send is contained in the variable 'rv$dataIn'
  
  
  
  # Initialize vector to all NULL values
  data2send <- setNames(
    lapply(rv$config$steps, function(x){NULL}),
    rv$config$steps)
  
  if (is.null(rv$dataIn)){ # Init of core engine
    
    # Only the first process will receive the data
    data2send[[1]] <- rv$temp.dataIn
    
    # The other processes are by default disabled.
    # If they have to be enabled, they will be by another function later
    lapply(seq_len(length(rv$config$steps)), function(x){
      rv$steps.enabled[x] <- x==1
    })
    
  } else
    data2send[[CurrentStepName(rv$current.pos, rv$config$steps)]] <- Update_Data2send_Vector(rv)
  
  cat(crayon::blue('<----------------- Data sent to children ------------------> \n'))
  print(data2send)
  cat(crayon::blue('<----------------------------------------------------> \n'))
  
  return(
    list(data2send = data2send,
         steps.enabled = rv$steps.enabled
         )
  )
}





Build_pipeline_ui <- function(ns){
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
}