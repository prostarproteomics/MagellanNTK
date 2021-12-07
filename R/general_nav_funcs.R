


#' @title xxx
#' 
#' @description xxx
#' 
#' @param steps.status xxx
#' @param current.pos xxx
#' @param config xxx
#' 
#' @return A `wellPanel`
#' 
#' @export
#' 
Build_SkippedInfoPanel <- function(steps.status, 
                                   current.pos, 
                                   config){
  
   req(steps.status[current.pos] == global$SKIPPED)
  process_entirely_skipped <- isTRUE(sum(steps.status) == global$SKIPPED * length(config$steps))
  
  if (process_entirely_skipped){
    # This case appears when the process has been skipped from the
    # pipeline. Thus, it is not necessary to show the info box because
    # it is shown below the timeline of the pipeline
  } else {
    txt <- paste0("This ", config$type, " is skipped so it has been disabled.")
    wellPanel(
      style = "background-color: #7CC9F0; opacity: 0.72; padding: 0px;
                   align: center; vertical-align: center;",
      height = 100,
      width = 300,
      align = "center",
      p(style = "color: black;", paste0("Info: ",txt))
    )
  }
  
}



#' @title xxx
#' 
#' @description xxx
#' 
#' @param ns xxx
#' @param id xxx
#' @param config xxx
#' 
#' @return A `renderUI` function
#' 
#' @export
#' 
Build_EncapsulateScreens_ui <- function(ns, 
                                        id, 
                                        config){
  len <- length(config$ll.UI)
  renderUI({
    tagList(
      lapply(seq_len(len), function(i) {
        if (i==1)
          div(id = ns(names(config$steps)[i]),
              class = paste0('page_', id),
              config$ll.UI[[i]]
          )
        else
          shinyjs::hidden(
            div(id =  ns(names(config$steps)[i]),
                class = paste0('page_', id),
                config$ll.UI[[i]]
            )
          )
      }
      )
    )
  })
}





#' @title Last validated step before current step.
#' 
#' @description xxx
#' 
#' @param pos xxx
#' @param current.pos xxx
#' @param steps.status xxx
#' 
#' @return A `integer(1)` which is the indice in the set of steps
#' that corresponds to the last validated step before the one identified
#' by current.pos.
#' 
#' @export
#' 
GetMaxValidated_BeforePos <- function(pos = NULL,
                                      current.pos,
                                      steps.status){
  
  if (is.null(pos))
    pos <- current.pos
  
  ind.max <- NULL
  indices.validated <- which(steps.status == global$VALIDATED)
  if (length(indices.validated) > 0){
    ind <- which(indices.validated < pos)
    if(length(ind) > 0)
      ind.max <- max(ind)
  }

  return(ind.max)
}



#' @title xxx
#' 
#' @description xxx
#' 
#' @param steps.status xxx
#' 
#' @return xxx
#' 
#' @export
#' 
GetMaxValidated_AllSteps <- function(steps.status){
  val <- 0
  ind <- grep(global$VALIDATED, steps.status)
  if (length(ind) > 0)
    val <- max(ind)
  
  return(val)
}



#' @title xxx
#' 
#' @description Updates the status of steps in range
#' 
#' @param cond xxx
#' @param range xxx
#' @param is.enabled xxx
#' @param steps.info xxx
#' 
#' @return A vector for steps.enabled
#' 
#' @export
#' 
ToggleState_Screens <- function(cond, 
                                range, 
                                is.enabled,
                                steps.info){
  
  if (isTRUE(is.enabled))
    steps.info[range, 'enabled'] <- cond && !(steps.info[range, 'status'] == global$SKIPPED)
    
    return(steps.info$enabled)
}


#' @title Status to string
#' 
#' @description xxx
#' 
#' @param i xxx
#' 
#' @param title.style xxx
#' 
#' @return xxx
#' 
#' @export
#' 
GetStringStatus <- function(i, title.style = FALSE){
  txt <- names(which(global == i))
  
  if (title.style)
  txt <- paste(substr(txt, 1, 1), 
               tolower(substr(txt, 2, nchar(txt))), 
               sep="")
  txt
}



#' @title Change active page
#' 
#' @description xxx
#' 
#' @param direction xxx
#' @param current.pos xxx
#' @param len xxx
#' 
#' @return A `integer(1)` which is the new current position.
#' 
#' @export
#' 
NavPage <- function(direction, 
                    current.pos, 
                    len) {
  newval <- current.pos + direction
  newval <- max(1, newval)
  newval <- min(newval, len)
  current.pos <- newval
  
  return(current.pos)
}



#' @title Builds data modal.
#' 
#' @description xxx
#' 
#' @param ns xxx
#' @param mode xxx
#' 
#' @return A tag div for ui
#' 
#' @export
#' 
dataModal <- function(ns, mode) {
  # Used to show an explanation for the reset feature whether the navigation mode is 'process' nor 'pipeline'.
  template_reset_modal_txt <- 'This action will reset this mode. The input dataset will be the output of the last previous
validated process and all further datasets will be removed'
  
  tags$div(id = 'modal1',
           modalDialog(
             span(gsub('mode', mode, template_reset_modal_txt)),
             footer = tagList(
               actionButton(ns('closeModal'), 'Cancel', class='btn-info'),
               actionButton(ns('modal_ok'), 'OK')
             )
           )
  )
}




#' @title Discover Skipped Steps
#' 
#' @description xxx
#' 
#' @param steps.status xxx
#' 
#' @return xxx
#' 
#' @export
#' 
Discover_Skipped_Steps <- function(steps.status){
  for (i in seq_len(length(steps.status))){
    max.val <- GetMaxValidated_AllSteps(steps.status)
    if (steps.status[i] != global$VALIDATED && max.val > i)
      steps.status[i] <- global$SKIPPED
  }
  
  return(steps.status)
}




#' @title Set skipped tag to all steps
#' 
#' @description xxx
#' 
#' @param steps.status xxx
#' @param tag xxx
#' 
#' @return xxx
#' 
#' @export
#'
All_Skipped_tag <- function(steps.status, tag){
  steps.status <- rep(tag, length(steps.status))
  
  return(steps.status)
  
}

#' @title Get First Mandatory Not Validated
#' 
#' @description xxx
#' 
#' @param range xxx
#' @param steps.info xxx
#' @param config xxx
#' 
#' @return A `integer(1)` which correspond to 
#' 
#' @export
#'
GetFirstMandatoryNotValidated <- function(range, 
                                          steps.info,
                                          config){
  ind <- NULL
  first <- NULL
  first <- unlist((lapply(range,
                          function(x){config$mandatory[x] && !steps.info$status[x]})))
  ind <- if (sum(first) > 0)
    min(which(first == TRUE))
  else
    NULL
  
  return(ind)
}





#' @title Update State Screens
#' 
#' @description Updates the tag 'enabled' w.r.t. the actual state of
#' the module (timeline, validated steps, ...)
#' 
#' 
#' @param is.skipped xxx
#' @param is.enabled xxx
#' @param steps.info xxx
#' @param config xxx
#' @param current.pos xxx
#' 
#' @return A vector of enabled tag for the steps. 
#' 
#' @export
#' 
Update_State_Screens <- function(is.skipped,
                                 is.enabled,
                                 steps.info,
                                 config,
                                 current.pos){
  
  len <- nrow(steps.info)
  #browser()
  if (isTRUE(is.skipped)){
    steps.info$enabled <- ToggleState_Screens(cond = FALSE,
                                         range = seq_len(len),
                                         is.enabled = is.enabled,
                                         steps.info = steps.info)
  } else {
    
    # Ensure that all steps before the last validated one are disabled
    ind.max <- GetMaxValidated_AllSteps(steps.info$status)
    if (ind.max > 0)
      steps.info$enabled <- ToggleState_Screens(cond = FALSE, 
                                           range = seq_len(ind.max),
                                           is.enabled = is.enabled,
                                           steps.info = steps.info)
    
    if (ind.max < len){
      # Enable all steps after the current one but the ones
      # after the first mandatory not validated
      firstM <- GetFirstMandatoryNotValidated(range = (ind.max+1):len,
                                              steps.info = steps.info,
                                              config = config)
      if (is.null(firstM)){
        steps.info$enabled <- ToggleState_Screens(cond = TRUE, 
                                                       range = (1 + ind.max):(len),
                                                       is.enabled = is.enabled,
                                                       steps.info = steps.info)
      } else {
        steps.info$enabled <- ToggleState_Screens(cond = TRUE, 
                                                       range = (1 + ind.max):(ind.max + firstM),
                                                       is.enabled = is.enabled,
                                                       steps.info = steps.info )
        if (ind.max + firstM < len)
          steps.info$enabled <- ToggleState_Screens(cond = FALSE,
                                                         range = (ind.max + firstM + 1):len,
                                                         is.enabled = is.enabled,
                                                         steps.info = steps.info )
      }
    }
    
    ToggleState_NavBtns(current.pos = current.pos,
                        nSteps = len
                        )
  }
  
  return(steps.info$enabled)
}


#' @title ToggleState Navigation Buttons
#' 
#' @description xxx
#' 
#' @param current.pos xxx
#' @param nSteps xxx
#' 
#' @return NA
#' 
#' @export
#' 
ToggleState_NavBtns <- function(current.pos, 
                                nSteps){
  
  # If the cursor is not on the first position, show the 'prevBtn'
  shinyjs::toggleState(id = 'prevBtn', condition = current.pos != 1)
  
  # If the cursor is set before the last step, show the 'nextBtn'
  shinyjs::toggleState(id = 'nextBtn', condition = current.pos < nSteps)
}


#' @title ToggleState Reset Button
#' 
#' @description xxx
#' 
#' @param cond xxx
#' 
#' @return NA
#' 
#' @export
#' 
ToggleState_ResetBtn <- function(cond){
  shinyjs::toggleState('rstBtn', condition = cond)
}

