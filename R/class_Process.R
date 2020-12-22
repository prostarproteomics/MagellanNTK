#' @title
#' xxx
#' 
#' @description
#' xxxx
#' 
#' @export
#' 
#' @importFrom R6 R6Class
#' 
Process = R6Class(
  "Process",
  inherit = ScreenManager,
  
  private = list(
    
    #' @description
    #' xxx
    #'
    #' @param input A number
    #' @param output A number
    #' 
    GetScreens_server = function(input, output){
      if(self$verbose) cat(paste0(class(self)[1], '::GetScreens() from - ', self$id, '\n\n'))
      setNames(lapply(self$config$steps, function(x){
        eval(parse(text = paste0("self$", x, '_server(input, output)')))
      }),
      self$config$steps)
    }
    ),

  
  public = list(
    
    #' @field modal_txt xxx
    modal_txt = "This action will reset this process. The input dataset will be the output of the last previous
                      validated process and all further datasets will be removed",
    
    
    #' @description
    #' xxx
    #'
    #' @param cond A number
    #' @param range A number
    #' 
    ToggleState_Screens = function(cond, range){
      if(self$verbose) cat(paste0(class(self)[1], '::ToggleState_Steps() from - ', self$id, '\n\n'))
      #browser()
      lapply(range, function(x){
        cond <- cond && !(self$rv$status[x] == global$SKIPPED)
        shinyjs::toggleState(self$ns(self$config$steps[x]), condition = cond  )
        #Send to TL the enabled/disabled tags
        self$rv$tl.tags.enabled[x] <- cond
      })
    },
    

    #' @description
    #' Set to skipped all steps of the current object
    #'
    #' @return Nothing
    #' 
    Set_All_Skipped = function(){
      if(self$verbose) cat(paste0(class(self)[1], '::', 'Set_All_Skipped() from - ', self$id, '\n\n'))
      self$rv$status <- setNames(rep(global$SKIPPED, self$length), self$config$steps)
    },
    
    
    #' @description
    #' xxx
    #'
    #' @return Nothing
    #' 
    Discover_Skipped_Steps = function(){
      if(self$verbose) cat(paste0(class(self)[1], '::Discover_Skipped_Status() from - ', self$id, '\n\n'))
      for (i in 1:self$length){
        max.val <- private$GetMaxValidated_AllSteps()
        if (self$rv$status[i] != global$VALIDATED && max.val > i)
          self$rv$status[i] <- global$SKIPPED
        }
    },
    
    #' @description
    #' xxx
    #'
    #' @return Nothing
    #'
     Set_All_Reset = function(){
      if(self$verbose) cat(paste0(class(self)[1], '::', 'Set_All_Reset() from - ', self$id, '\n\n'))
      
      private$BasicReset()
    },
    
    
    
    #' @description
    #' xxx
    #'
    #' @return Nothing
    #'
    ValidateCurrentPos = function(){
      if(self$verbose) cat(paste0(class(self)[1], '::', 'ValidateCurrentPos() from - ', self$id, '\n\n'))
      #browser()
      self$rv$status[self$rv$current.pos] <- global$VALIDATED
      
      # Either the process has been validated, one can prepare data to be sent to caller
      # Or the module has been reseted
      if (self$rv$current.pos == self$length)
        private$Send_Result_to_Caller()
    },
    
    
    #' @description
    #' xxx
    #'
    #' @return Nothing
    #'
    EncapsulateScreens = function(){
      if(self$verbose) cat(paste0(class(self)[1], '::EncapsulateScreens() from - ', self$id, '\n\n'))
      lapply(1:self$length, function(i) {
        shinyjs::disabled(
          if (i==1)
            div(id = self$ns(self$config$steps[i]),
                class = paste0("page_", self$id),
                self$screens[[i]]
            )
          else
            shinyjs::hidden(
              div(id = self$ns(self$config$steps[i]),
                  class = paste0("page_", self$id),
                  self$screens[[i]]
              )
            )
        )
      }
      )
    },
    
    #' @description
    #' xxx
    #'
    #' @return Nothing
    #'
    GetScreens_ui = function(){
      if(self$verbose) cat(paste0(class(self)[1], '::GetScreens() from - ', self$id, '\n\n'))

        setNames(lapply(self$config$steps, function(x){
          eval(parse(text = paste0("uiOutput(self$ns('", x, "'))")))
         }),
        self$config$steps)
    }
  )
)
