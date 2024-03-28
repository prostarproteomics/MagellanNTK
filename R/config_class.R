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
#'  * Description pipeline: This case is for a process called 'Description' which is 
#'  the first process module of a pipeline
#'
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
#' @example inst/extdata/funcs_examples/example_config_class.R
#'
#' @name Config-class
#' @rdname Config-class
#' @export Config
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
      #,dirpath_to_md_file = 'vector'
    ),

    validity <- function(.Object) {
        passed <- TRUE
        nSteps <- length(.Object@steps)
        #
        # General conditions
        #
        if (length(.Object@fullname) != 1 && .Object@fullname != ''){
          warning(paste0("The slot 'fullname' must contain one string. Current value is: ", object@name))
          passed <- FALSE
        } 
        
        # if (length(.Object@name) != 1){
        #     warning(paste0("The slot 'name' must contain one string. Current value is: ", object@name))
        #     passed <- FALSE
        # } 
        
        
        
        # Check if mode exists and is an available keyword
        if (!(.Object@mode %in% c('process', 'pipeline'))){
            warning("The 'mode' must be one of the following: 'process', 'pipeline'")
            passed <- FALSE
        }
        
        
        # Check if process has a parent
        # if (object@mode == 'process' && object@parent == ''){
        #     warning("A process workflow must have a parent process.")
        #     passed <- FALSE
        # }

        # Check validity for Description module
        # if (object@name == 'Description'){
        #   if (object@steps != '' || object@mandatory != ''){
        #     warning("A 'Description' module does not have no steps nor mandatory info.")
        #     passed <- FALSE
        #   }
        #   if (object@mode != 'process'){
        #     warning("A 'Description' module is a process and has a 'pipeline' module as parent.")
        #     passed <- FALSE
        #   }
        #   if (object@parent == ''){
        #     warning("A 'Description' module is a process and thus has a 'pipeline' module as parent. 'Parent' cannot be empty.")
        #     passed <- FALSE
        #   }
        #   }
          


    return(passed)
    }
)



###
### Check functions
###

is.GenericProcess <- function(.Object){
  passed <- validity(.Object)
  
  passed <- passed && (.Object@mode == 'process')
  passed <- passed && (length(.Object@parent) >= 1)
  passed <- passed && (length(.Object@steps) >= 1 && .Object@name != 'Description')
  
  return(passed)
}


is.RootProcess <- function(.Object){
  passed <- validity(.Object)
  passed <- passed && is.GenericProcess(.Object)
  passed <- passed && .Object@parent == ''
  
  return(passed)
}


is.GenericPipeline <- function(.Object){
  passed <- validity(.Object)
  passed <- passed && (.Object@mode == 'pipeline')
  passed <- passed && (length(.Object@steps) >= 1 && .Object@name != 'Description')
  
  return(passed)
}

is.RootPipeline <- function(.Object){
  passed <- validity(.Object)
  passed <- passed && is.GenericPipeline(.Object)
  passed <- passed && .Object@parent == ''
  
  return(passed)
}


is.DescriptionProcess <- function(.Object){
  passed <- validity(.Object)
  passed <- passed && (.Object@mode == 'process')
  passed <- passed && (length(.Object@parent) >= 1 && .Object@parent != '')
  passed <- passed && all(.Object@steps == '')
  passed <- passed && all(.Object@mandatory == '')
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



init.RootProcess <- function(.Object){
  
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


#' @title xxx
#' @description xxx
#' @param object xxx
#' 
setMethod("show", 'Config',
          function(object){
            cat(crayon::green('\t ------- Config -------\n'))
            cat(crayon::green(paste0('\tfullname: ', object@fullname, '\n')))
            cat(crayon::green(paste0('\tname: ', object@name, '\n')))
            cat(crayon::green(paste0('\tparent: ', object@parent, '\n')))
            cat(crayon::green(paste0('\tmode: ', object@mode, '\n')))
              
              cat(crayon::green('\tnames(steps): '))
              cat(crayon::green(names(object@steps)))
              cat(crayon::green('\n'))

              cat(crayon::green('\tsteps: '))
              cat(crayon::green(object@steps))
              cat(crayon::green('\n'))
              
              cat(crayon::green('\tmandatory: '))
              cat(crayon::green(object@mandatory))
              cat(crayon::green('\n'))
              
              cat(crayon::green('\tnames(ll.UI): '))
              cat(crayon::green(names(object@ll.UI)))
              cat(crayon::green('\n'))
              
              cat(crayon::green('\tll.UI: '))
              cat(crayon::green(object@ll.UI))
              cat(crayon::green('\n'))
              }
)

#' @title Initialization method for the class `Config`
#' 
#' @param .Object xxx
#' @param fullname xxx
#' @param mode xxx
#' @param steps xxx
#' @param mandatory xxx
#' @param steps.source.file xxx
#' 
#' @rdname Config-class
#' 
setMethod("initialize" ,
    "Config" ,
    function(.Object,
             fullname,
             mode,
             steps,
             mandatory,
             steps.source.file
      #,dirpath_to_md_file
      ){
        
        # Basic init of slots


      .Object@fullname <- fullname
      
      if (!is.null(steps.source.file))
        .Object@steps.source.file <- steps.source.file
      
      
        tmp <- unlist(strsplit(fullname, '_'))
        if (length(tmp) == 2){
          .Object@name <- tmp[2]
          .Object@parent <- tmp[1]
        } else {
          .Object@name <- tmp[1]
          .Object@parent <- ''
        }
         
        .Object@mode <- mode
        .Object@steps <- steps
        .Object@mandatory <- mandatory 
        
        
        if (is.GenericProcess(.Object))
          .Object <- init.GenericProcess(.Object)
        else if (is.RootProcess(.Object))
          .Object <- init.RootProcess(.Object)
        else if (is.GenericPipeline(.Object))
          .Object <- init.GenericPipeline(.Object)
        else if (is.DescriptionProcess(.Object))
          .Object <- init.DescriptionProcess(.Object)
        else if (is.RootPipeline(.Object))
          .Object <- init.RootPipeline(.Object)
        else 
          .Object <- NULL

        #.Object@dirpath_to_md_file <- dirpath_to_md_file
        
        # If the config represents a pipeline, builds the expected names of 
        # the source files of its steps
        return(.Object)
    }
)

#' @title Constructor of the `Config` class
#' 
#' @description Wrapper function to the constructor of the class
#' 
#' @rdname Config-class
#' 
#' @param fullname xxx
#' @param mode xxx
#' @param steps xxx
#' @param mandatory xxx
#' @param steps.source.file xxx
#' 
#' @export
#' 
Config <- function(fullname = '', 
  mode = '',
  steps = '', 
  mandatory = '',
  steps.source.file = NULL
  #,dirpath_to_md_file = NULL
  ){
    
    new(Class ="Config",
        fullname = fullname,
        mode = mode,
        steps = steps, 
        mandatory = mandatory,
        steps.source.file = steps.source.file
      #,dirpath_to_md_file = dirpath_to_md_file
      )
}