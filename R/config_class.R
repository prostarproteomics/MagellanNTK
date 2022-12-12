##' @title Config class definition
##'
##' @description
##'
##' This class is used to store the configuration of any process
##' used with Magellan. It contains a validity function to ensure
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
        path_to_md_dir = "character",
        ll.UI = "list"
    ),
    prototype(
        name = character(0),
        mode = 'process',
        steps = c('Description', 'Save'),
        mandatory = c(TRUE, TRUE),
        path_to_md_dir = '.',
        ll.UI = list()
    ),
    
    
    validity = function(object) {
        passed <- TRUE
        if (length(object@name) != 1){
            warning('xxxx')
            passed <- FALSE
        }
        
        if (length(object@mode) != 1){
            warning('xxxx')
            passed <- FALSE
        }
        
        if (length(object@steps) <= 1){
            warning("The number of steps ('Description' step included) must be greater or equal to 2.")
            passed <- FALSE
        }
        
        if (length(object@mandatory) != length(object@steps)){
            warning("'steps' and 'mandatory' must have the same length.")
            passed <- FALSE
        }

        if (length(object@path_to_md_dir) != 1){
            warning('xxxx')
            passed <- FALSE
        }
        
        if (object@steps[1] != 'Description'){
            warning('xxxx')
            passed <- FALSE
        }
        
        if (object@mandatory[1] != TRUE){
            warning('xxxx')
            passed <- FALSE
        }
        

    if (object@mode == 'process'){
        if (object@steps[length(object@steps)] != 'Save'){
            warning("In a process workflow, the last step must be called 'Save'")
            passed <- FALSE
        }
        
        if (object@mandatory[length(object@steps)] != TRUE){
            warning('In a process workflow, the last step is mandatory.')
            passed <- FALSE
        }
    }

    return(passed)
    }
)
