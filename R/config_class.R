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
#' ## Initialization
#'  ### Generic process
#'  
#'  A generic process
#'  * Generic pipeline : xxxx
#'  * Description pipeline: This case is for a process -called 'Description' which is 
#'  the first process module of a pipeline
#'
#' @export Config
#' @exportClass Config
#' @rdname config
#' @example examples/example_config_class.R
#' 
#' @slot fullname xxxx
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
      fullname = "character",
      name = "character",
      parent = "character",
      mode = "character",
      steps = "vector",
      mandatory = "vector",
      ll.UI = "list",
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
        
        
        
        # Check if mode exists and is an available keyword
        if (length(object@mode) != 1){
            warning("'mode' must contain only one string")
            passed <- FALSE
        } else if (!(object@mode %in% c('process', 'pipeline'))){
            warning("The 'mode' must be one of the following: 'process', 'pipeline'")
            passed <- FALSE
        }
        
        
        # Check if process has a parent
        if (object@mode == 'process' && object@parent == ''){
            warning("A process workflow must have a parent process.")
            passed <- FALSE
        }
        
        # Check if mandatory ans steps vectors have the same lenght
        if (length(object@mandatory) != nSteps){
            warning("'steps' and 'mandatory' must have the same length.")
            passed <- FALSE
        }

        
        if ((object@steps[1] != 'Description') || (object@mandatory[1] != TRUE)){
            warning("The first step of a workflow must be 'Description' and it is mandatory.")
            passed <- FALSE
        }
        
        # Check validity for Description module
        if (object@name == 'Description'){
          if (object@steps != '' || object@mandatory != ''){
            warning("A 'Description' module does not have no steps nor mandatory info.")
            passed <- FALSE
          }
          if (object@mode != 'process'){
            warning("A 'Description' module is a process and has a 'pipeline' module as parent.")
            passed <- FALSE
          }
          if (object@parent == ''){
            warning("A 'Description' module is a process and thus has a 'pipeline' module as parent. 'Parent' cannot be empty.")
            passed <- FALSE
          }
          }
          


    return(passed)
    }
)



###
### Check functions
###

is.GenericProcess <- function(.Object){
  passed <- TRUE
  
  passed <- passed && (.Object@mode == 'process')
  passed <- passed && (.Object@parent != '')
  passed <- passed && (length(.Object@steps) >= 1 && .Object@name != 'Description')
  
  return(passed)
}

is.GenericPipeline <- function(.Object){
  passed <- TRUE
  passed <- passed && (.Object@mode == 'pipeline')
  passed <- passed && (length(.Object@steps) >= 1 && .Object@name != 'Description')
  
  return(passed)
}

is.RootPipeline <- function(.Object){
  passed <- TRUE
  passed <- passed && is.GenericPipeline(.Object)
  passed <- passed && .Object@parent == ''
  
  return(passed)
}


is.DescriptionProcess <- function(.Object){
  passed <- TRUE
  passed <- passed && (.Object@mode == 'process')
  passed <- passed && (.Object@parent != '')
  passed <- passed && (.Object@steps == '' && .Object@mandatory == '')
  passed <- passed && grepl('Description', .Object@name)
  
  return(passed)
}


###
### Initialization functions
###

init.GenericProcess <- function(.Object){

  # A process has a parent
  
  .Object@steps <- c('Description', .Object@steps, 'Save')
  .Object@mandatory <- c(TRUE, .Object@mandatory, TRUE)
  
  # .Object@steps <- setNames(.Object@steps,
  #                           nm = paste0(.Object@fullname, '_', 
  #                                       gsub(' ', '',.Object@steps, fixed=TRUE)))
  
  .Object@steps <- setNames(.Object@steps, nm = gsub(' ', '',.Object@steps, fixed=TRUE))
  
  .Object@mandatory <- setNames(.Object@mandatory, nm = names(.Object@steps))
  
  return(.Object)
}


init.RootPipeline <- function(.Object){
  
  # A pipeline may have a parent or not (in this case, it is the first node 
  # level of the whole workflow
  
  .Object@steps <- c('Description', .Object@steps)
  .Object@mandatory <- c(TRUE, .Object@mandatory)
  
  # .Object@steps <- setNames(.Object@steps,
  #                           nm = paste0(.Object@fullname, '_',
  #                                       gsub(' ', '',.Object@steps, fixed=TRUE)))
  
  .Object@steps <- setNames(.Object@steps, nm = gsub(' ', '',.Object@steps, fixed=TRUE))
  
  
  .Object@mandatory <- setNames(.Object@mandatory, nm = names(.Object@steps))
  
  
  # This line comes after the other ones because in the case of a pipeline, 
  # the description step is a module itself and must be loaded in memory
  # as well as the other steps
  .Object@steps.source.file <- paste0(names(.Object@steps), '.R')

  return(.Object)
}




init.GenericPipeline <- function(.Object){
  
  # A pipeline may have a parent or not (in this case, it is the first node 
  # level of the whole workflow
  
  .Object@steps <- c('Description', .Object@steps)
  .Object@mandatory <- c(TRUE, .Object@mandatory)
  
  # .Object@steps <- setNames(.Object@steps,
  #                           nm = paste0(.Object@fullname, '_',
  #                                       gsub(' ', '',.Object@steps, fixed=TRUE))
  # )
  
  .Object@steps <- setNames(.Object@steps, nm = gsub(' ', '',.Object@steps, fixed=TRUE))
  
  .Object@mandatory <- setNames(.Object@mandatory, nm = names(.Object@steps))
  
  # This line comes after the other ones because in the case of a pipeline, 
  # the description step is a module itself and must be loaded in memory
  # as well as the other steps
  .Object@steps.source.file <- paste0(names(.Object@steps), '.R')
  
  
  return(.Object)
}


#' @description A Description process has only one step called 'Description'
#' 
init.DescriptionProcess <- function(.Object){
  # A process has a parent
  .Object@steps <- c('Description')
  .Object@mandatory <- c(TRUE)
  # .Object@steps <- setNames(.Object@steps,
  #                           nm = paste0(.Object@fullname, '_',
  #                                       gsub(' ', '',.Object@steps, fixed=TRUE))
  # )
  .Object@steps <- setNames(.Object@steps, nm = gsub(' ', '',.Object@steps, fixed=TRUE))
  
  .Object@mandatory <- setNames(.Object@mandatory, nm = names(.Object@steps))
  
  # This line comes after the other ones because in the case of a pipeline, 
  # the description step is a module itself and must be loaded in memory
  # as well as the other steps
  .Object@steps.source.file <- paste0(names(.Object@steps), '.R')
  
  return(.Object)
}



setMethod("show", 'Config',
          function(.Object){
            cat(crayon::green('\t ------- Config -------\n'))
            cat(crayon::green(paste0('\tfullname: ', .Object@fullname, '\n')))
            cat(crayon::green(paste0('\tname: ', .Object@name, '\n')))
            cat(crayon::green(paste0('\tparent: ', .Object@parent, '\n')))
            cat(crayon::green(paste0('\tmode: ', .Object@mode, '\n')))
              
              cat(crayon::green('\tnames(steps): '))
              cat(crayon::green(names(.Object@steps)))
              cat(crayon::green('\n'))

              cat(crayon::green('\tsteps: '))
              cat(crayon::green(.Object@steps))
              cat(crayon::green('\n'))
              
              cat(crayon::green('\tmandatory: '))
              cat(crayon::green(.Object@mandatory))
              cat(crayon::green('\n'))
              
              cat(crayon::green('\tnames(ll.UI): '))
              cat(crayon::green(names(.Object@ll.UI)))
              cat(crayon::green('\n'))
              
              cat(crayon::green('\tll.UI: '))
              cat(crayon::green(.Object@ll.UI))
              cat(crayon::green('\n'))
              }
)

#' @title Initialization method for the class `Config`
#' @rdname config
#' 
setMethod("initialize" ,
    "Config" ,
    #' @param .Object xxx
    #' @param fullname xxx
    #' @param mode xxx
    #' @param steps xxx
    #' @param mandatory xxx
    function(.Object, 
             fullname, 
        mode,
        steps, 
        mandatory){
        
        # Basic init of slots
      
      .Object@fullname <- fullname
      
        tmp <- unlist(strsplit(fullname, '_'))
        if (length(tmp) == 2){
          .Object@name <- tmp[2]
          .Object@parent <- tmp[1]
        } else {
          .Object@name <- tmp[1]
        }
         
        .Object@mode <- mode
        .Object@steps <- steps
        .Object@mandatory <- mandatory 
        
        
        if (is.GenericProcess(.Object))
          .Object <- init.GenericProcess(.Object)
        else if (is.GenericPipeline(.Object))
          .Object <- init.GenericPipeline(.Object)
        else if (is.DescriptionProcess(.Object))
          .Object <- init.DescriptionProcess(.Object)
        
        else if (is.RootPipeline(.Object))
          .Object <- init.RootPipeline(.Object)

        
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
#' @param fullname xxx
#' @param mode xxx
#' @param steps xxx
#' @param mandatory xxx
#' 
Config <- function(fullname, 
    mode,
    steps = '', 
    mandatory = ''){
    
    new(Class ="Config",
        fullname = fullname,
        mode = mode,
        steps = steps, 
        mandatory = mandatory)
}