##' @title Config class definition
##'
##' @description
##'
##' This class is used to store the configuration of any process
##' used with MagellanNTK It contains a validity function to ensure
##' that the format is correct.
##' 
##' Validity:
##' * The first step must be called 'Description', it is a mandatory step. Thus, 
##' the first tiem of the mandaotry vector is TRUE.
##'
##' @export Config
##' @exportClass Config
##' @rdname config
##' @examples NULL
Config <- setClass("Config",
    representation(
        name = "character",
        mode = "character",
        steps = "vector",
        mandatory = "vector",
        path_to_md_file = "character",
        ll.UI = "list"
    ),
    prototype(
        name = character(0),
        mode = 'process',
        steps = c('Description', 'Save'),
        mandatory = c(TRUE, TRUE),
        path_to_md_file = '.',
        ll.UI = list()
    ),
    
    
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

        if (length(object@path_to_md_file) != 1){
            warning(paste0("The slot 'path_to_md_file' must contain one string. Current value is: ", object@path_to_md_file))
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
