#' @title Config class definition
#'
#' @description
#'
#' This class is used to store the configuration of any process
#' used with MagellanNTK It contains a validity function to ensure
#' that the format is correct.
#' 
#' Validity:
#' * The first step must be called 'Description', it is a mandatory step. Thus, 
#' the first item of the mandatory vector is TRUE.
#' To be continued...
#'
#' @export Config
#' @exportClass Config
#' @rdname config
#' @example examples/example_config_class.R
#' 
#' @slot name xxx
#' @slot parent xxx
#' @slot mode xxx
#' @slot steps xxx
#' @slot mandatory xxx
#' @slot ll.UI xxx
#' @slot steps.source.file xxx
#' 
#' @usage xxx
#' 
#' 
Config <- setClass("Config",
    representation(
        name = "character",
        parent = "character",
        mode = "character",
        steps = "vector",
        mandatory = "vector",
        ll.UI = "list",
        module.name = 'character',
        steps.source.file = 'vector'
    ),

    #' @param object xxx
    #' 
    validity = function(object) {
        passed <- TRUE
        nSteps <- length(object@steps)
        #
        # General conditions
        #
        if (length(object@name) != 1){
            warning(paste0("The slot 'name' must contain one string. Current value is: ", object@name))
            passed <- FALSE
        } 
        
        # if (length(object@parent) != 1){
        #     warning(paste0("The slot 'parent' must contain one string. Current value is: ", object@name))
        #     passed <- FALSE
        # } 
        
        
        if (length(object@mode) != 1){
            warning("'mode' must contain only one string")
            passed <- FALSE
        } else if (!(object@mode %in% c('process', 'pipeline'))){
            warning("'mode' must be one of the following: 'process', 'pipeline'")
            passed <- FALSE
            }

        
        if (length(object@mandatory) != nSteps){
            warning("'steps' and 'mandatory' must have the same length.")
            passed <- FALSE
        }

        
        if ((object@steps[1] != 'Description') || (object@mandatory[1] != TRUE)){
            warning("The first step of a workflow must be 'Description' and it is mandatory.")
            passed <- FALSE
        }

        #
        # Mode-specific conditions
        #
        switch (object@mode,
        
        process = {
            if (nSteps == 1){
                if (object@steps != 'Description'){
                warning("The only case where a process can contain only one step is
                        when it is the Description of a pipeline of higher level")
                passed <- FALSE
                }
            } else if (nSteps < 3){
                warning("The number of steps ('Description' and 'Save' steps included) must be greater or equal to 3.")
                passed <- FALSE
            } else{
                if ((object@steps[nSteps] != 'Save') || (object@mandatory[nSteps] != TRUE)){
                    warning("In a process workflow, the last step must be called 'Save'. It is a mandatory step.")
                    passed <- FALSE
                    }
                }
                },
        pipeline = {
            if (object@steps[nSteps] == 'Save'){
                warning("In a pipeline workflow, the last step cannot be 'Save'.")
                passed <- FALSE
            }
        }
    )

    return(passed)
    }
)

#' @title Initialization method for the class `Config`
#' @rdname config
#' 
setMethod("initialize" ,
    "Config" ,
    #' @param .Object xxx
    #' @param name xxx
    #' @param parent xxx
    #' @param mode xxx
    #' @param steps xxx
    #' @param mandatory xxx
    function(.Object, 
        name, 
        parent, 
        mode,
        steps, 
        mandatory){
        
        .Object@name <- name
        .Object@parent <- parent 
        .Object@mode <- mode
        .Object@steps <- steps
        .Object@mandatory <- mandatory 
        .Object@module.name <- ''
        
        
        if (parent == '' || is.null(parent))
          .Object@module.name <- .Object@name
        else
          .Object@module.name <- paste0(.Object@parent, '_', .Object@name)
        
        
        # Build the expected name of the module
        if (.Object@steps != ''){
          switch(.Object@mode,
            process = {
                # This is a process part of a workflow
                
                  
                  .Object@steps <- c('Description', .Object@steps, 'Save')
                  .Object@mandatory <- c(TRUE, .Object@mandatory, TRUE)
                  .Object@steps <- setNames(.Object@steps, 
                                            nm = paste0(.Object@name, '_', gsub(' ', '',.Object@steps))
                  )
                  .Object@mandatory <- setNames(.Object@mandatory, nm = names(.Object@steps))
                }, 
            pipeline = {
                # This is the higher level of a workflow
                .Object@steps <- c('Description', .Object@steps)
                .Object@mandatory <- c(TRUE, .Object@mandatory)
                .Object@steps <- setNames(.Object@steps, 
                                          nm = paste0(.Object@name, '_', gsub(' ', '',.Object@steps))
                )
                .Object@mandatory <- setNames(.Object@mandatory, nm = names(.Object@steps))
                .Object@steps.source.file <- paste0(.Object@name, '_', gsub(' ', '',.Object@steps), '.R')
                
                }
            )
        }
        
        
        # If the config represents a pipeline, builds the expected names of 
        # the source files of its steps
        return(.Object )
    }
)

#' @title Constructor of the `Config` class
#' 
#' @description Wrapper function to the constructor of the class
#' 
#' @rdname config
#' 
#' @param parent xxx
#' @param mode xxx
#' @param steps xxx
#' @param mandatory xxx
#' 
Config <- function(name, 
    parent = '', 
    mode,
    steps = '', 
    mandatory = ''){
    
    new(Class ="Config",
        name = name, 
        parent = parent, 
        mode = mode,
        steps = steps, 
        mandatory = mandatory)
}