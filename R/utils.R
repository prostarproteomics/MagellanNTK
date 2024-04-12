
#' @title Read pipelines configuration files
#' 
#' @description xxx
#' 
#' @param path xxx
#' 
#' @examples
#' path <- system.file("workflow/PipelineA", package = 'MagellanNTK')
#' readCustomizableFuncs(file.path(path, 'config.txt'))
#' 
#' @export
#'
readCustomizableFuncs <- function(path){

  config.file <- normalizePath(file.path(path))
  if(!file.exists(config.file))
    stop('file does not exist')
  
  prepare_data <- function(lines, pattern){
    record <- lines[grepl(paste0(pattern, ":"), lines)]
    if(length(record) == 1){
      ll <- unlist(strsplit(record, split = ': ', fixed = TRUE))
      as.character(paste0(ll[2], '::', ll[1]))}
    else NULL
  }
  
  lines <- readLines(config.file)
  
  funcs <- list(
    convert_dataset = NULL,
    open_dataset = NULL,
    open_demoDataset = NULL,
    view_dataset = NULL,
    infos_dataset = NULL,
    addDatasets = NULL,
    keepDatasets = NULL
  )
  
  
  tmp <- lapply(names(funcs), 
    function(x) prepare_data(lines, x))
  names(tmp) <- names(funcs)
  
  return(funcs = tmp)
}


#' @title Source workflow files
#' 
#' @description xxx
#' 
#' @param dirpath xxx
#' @param verbose A boolean
#' 
#' @examples
#' path <- system.file("workflow/PipelineA", package = 'MagellanNTK')
#' source_wf_files(path)
#' 
#' @export
#'
source_wf_files <- function(dirpath, 
  verbose = FALSE){
R.dirpath <- file.path(dirpath, 'R')
files <- list.files(R.dirpath, full.names = FALSE)
for(f in files){
  if(verbose)
    cat('sourcing ', file.path(R.dirpath, f), '...')
  source(file.path(R.dirpath, f), local = FALSE, chdir = FALSE)
}


# if config.txt exists, update some funcs
config.file <- normalizePath(file.path(dirpath, 'config.txt'))
file.exists(config.file)
#browser()

}



#' @title Source ui.R, server.R ang global.R files
#' 
#' @description xxx
#' 
#' @examples
#' source_shinyApp_files()
#' 
#' @export
#'
source_shinyApp_files <- function(){
# Checks if app can be found
file_path_ui <- system.file("app/ui.R", package = "MagellanNTK")
file_path_server <- system.file("app/server.R", package = "MagellanNTK")
file_path_global <- system.file("app/global.R", package = "MagellanNTK")
if (!nzchar(file_path_ui) || 
    !nzchar(file_path_server) || 
    !nzchar(file_path_global)) 
  stop("Shiny app not found")

# Source add files
ui <- server <- NULL # avoid NOTE about undefined globals
source(file_path_ui, local = FALSE)
source(file_path_server, local = FALSE)
source(file_path_global, local = FALSE)

}


#' @title xxx
#' @param pattern xxx
#' @param target xxx
#' 
#' @return A boolean
#' @export
#' 
is.substr <- function(pattern, target){
  length(grep(pattern, target, fixed = TRUE)) > 0
}



#' # example code
#' #' @title Checks if the object is compliant with MagellanNTK
#' @description
#' Checks and accept the following data formats:
#' * An instance of class `MSnSet`
#' * An instance of class `MultiAssayExperiment`
#' * An instance of class `SummarizedExperiment`
#' * An instance of class `data.frame`
#' * An instance of class `matrix`
#' * A list of instances of class `MSnSet`
#' * A list of instances of class `SummarizedExperiment`
#' * A list of instances of class `data.frame`
#' * A list of instances of class `matrix`
#' 
#' 
#' @param obj xxx
#' 
#' @export
#' 
#' @examples
#' is.Magellan.compliant(data.frame())
#' 
#' 
#' ll <- list(data.frame(), data.frame())
#' is.Magellan.compliant(ll)
#' 
is.Magellan.compliant <- function(obj){
  passed <- FALSE
  
  
  is.listOf <- function(object, obj.class = NULL){
    res <- NULL
    if(is.null(obj.class)){
      ll <- unlist(lapply(object, function(x) class(x)[[1]]))
      if (length(unique(ll)) == 1)
        res <- unique(ll)
    } else {
      res <- TRUE
      res <- res && inherits(object, 'list')
      res <- res && all(unlist(lapply(object, 
          function(x) class(x)[[1]] == obj.class)))
    }
    return(res)
  }
  
  
  
  passed <- passed || inherits(obj, 'MSnset') 
  passed <- passed || inherits(obj, 'QFeatures') 
  passed <- passed || inherits(obj, 'SummarizedExperiment') 
  passed <- passed || inherits(obj, 'MultiAssayExperiment') 
  passed <- passed || inherits(obj, 'data.frame')
  passed <- passed || inherits(obj, 'matrix')
  
  
  if (inherits(obj, 'list')){
    passed <- passed || is.listOf(obj, 'matrix')
    passed <- passed || is.listOf(obj, 'data.frame')
    passed <- passed || is.listOf(obj, 'SummarizedExperiment')
    passed <- passed || is.listOf(obj, 'MSnset')
  }
  
  return(passed)
}

#' @title Checks if a Shiny module exists
#' @description This function checks if the ui() and server() parts of a 
#' Shiny module are available in the global environment.
#' @param base_name The name of the module (without '_ui' nor '_server' suffixes)
#' 
#' @return A boolean
#' @export
#'
module.exists <- function(base_name){
  server.exists <- exists(paste0(base_name, '_server'), 
                          envir = .GlobalEnv, mode = "function")
  ui.exists <- exists(paste0(base_name, '_ui'), 
                      envir = .GlobalEnv, mode = "function")
  
  return(server.exists && ui.exists)
}






#' @title
#' Basic check workflow directory
#'
#' @description
#' This function checks if the directory contains well-formed directories and files
#' It must contains 3 directories: 'md', 'R' and 'data'. 
#' The 'R' directory must contains two directories:
#' * 'workflows' that contains the source files for workflows,
#' * 'other' that contains additional source files used by workflows. This directory 
#' can be empty. For each
#' file in the 'R/workflows' directory, there must exists a *.md file with the same filename
#' in the 'md' directory.
#' The 'data' directory can be empty.
#' 
#' For a full description of the nomenclature of workflows filename, please refer
#' to xxx.
#'
#' @param path A `character(1)`
#' 
#' @return A `boolean(1)`
#' 
#' @export
#' 
CheckWorkflowDir <- function(path){
  
  is.valid <- TRUE
  
  # Checks if 'path' contains the 3 directories
  dirs <- list.files(path)
  cond <- all.equal(rep(TRUE, 3), c('R', 'md', 'data') %in% dirs)
  is.valid <- is.valid && cond
  if (!cond) message('atat')
  
  dirs <- list.files(file.path(path, 'R'))
  cond <- all.equal(rep(TRUE, 2), c('workflows', 'other') %in% dirs)
  is.valid <- is.valid && cond
  if (!cond) message('atat')
  
  # Checks the correspondance between files in 'R' and 'md' directories
  files.R <- list.files(file.path(path, 'R/workflows'))
  files.md <- list.files(file.path(path, 'md'))

  # Remove the definition of root pipelines which does not have a 
  # corresponding md file (their description is contained in a separate file)
  files.R <- files.R[grepl('_', files.R)]
  
  
  files.R <- gsub('.R', '', files.R)
  files.md <- gsub('.md', '', files.md)
  n.R <- length(files.R)
  n.md <- length(files.md)
  
  cond <- n.R == n.md
  is.valid <- is.valid && cond
  if (!cond) {
    message('Lengths differ between xxx')
    } else {
      cond <- all.equal(rep(TRUE, n.R), c('R', 'md', 'data') %in% dirs)
    if (!cond) message('titi')
    is.valid <- is.valid && cond
    }

  return(is.valid) 
}



#' @title
#' Hide/show a widget w.r.t a condition.
#'
#' @description
#' Wrapper for the toggleWidget function of the package `shinyjs`
#'
#' @param widget The id of a `Shiny` widget
#' @param condition A `logical(1)` to hide/show the widget.
#'
#' @return NA
#'
#' @author Samuel Wieczorek
#'
#' @export
#' 

toggleWidget <- function(widget, condition) {
    tagList(
        shinyjs::useShinyjs(),
        if (isTRUE(condition))
            widget
        else
            shinyjs::disabled(widget)
    )
}




#' @title
#' Timestamp in UNIX format.
#'
#' @description
#' Returns the date and time in timestamp UNIX format.
#'
#' @return A `integer()`.
#' @export
#'
Timestamp <- function()
    as.numeric(Sys.time())




#' @title
#' Datasets processing
#'
#' @description
#' This manual page describes manipulation methods using [list] objects. In 
# 'the following functions, if `object` is of class `list`, and optional array
#' index or name `i` can be specified to define the array (by name of
#' index) on which to operate.
#'
#' The following functions are currently available:
#'
#' - `keepDatasets(object, range)` keep datasets in object which
#' are in range
#'
#' - `addDatasets(object, dataset, name)` add the 'dataset' to the 
#' object (of type list)
#'
#' - `Save(object, file)` stores the object to a .RData file
#'
#' @details
#' The object must be of type list. Thetwo functions are implemented here for 
# 'a simple list. For other dataset classes, their implementation must be part 
#' of the package which uses MagellanNTK
#'
#' @param object An object of class `list`.
#'
#' @param range A xxxx
#'
#' @param dataset `character(1)` providing the base with respect to which
#'     logarithms are computed. Default is log2.
#'
#' @param name A `character(1)` naming the new array name.
#'
#' @return An processed object of the same class as `object`.
#'
#' @aliases keepDatasets keepDatasets,list-method
#' @aliases addDatasets addDatasets,list-method
#'
#' @name dataset-processing
#'
#' @importFrom methods setMethod new
#' 
#'
NULL

## -------------------------------------------------------
##   Keep datasets from object
## -------------------------------------------------------

#' 
#' #' @rdname dataset-processing
#' keepDatasets <- function(object, range) {
#'   #stopifnot(!is.list(object, 'list'))
#'   if (missing(range))
#'     stop("Provide range of array to be processed")
#'   
#'   if (is.null(object)) {
#'     return()
#'     }
#'   
#'   if (is.numeric(range))
#'     range <- names(object)[range]
#'   
#'   object[range]
#'   }


# #' @rdname dataset-processing
# setMethod("keepDatasets",
#           "QFeatures",
#           function(object, range) {
#             if (missing(range))
#               stop("Provide range of array to be processed")
#
#             if (is.numeric(range)) range <- names(object)[[range]]
#
#             object[ , , range]
#           })




## -------------------------------------------------------
##   Add datasets to object
## -------------------------------------------------------


#' #' @rdname dataset-processing
#' addDatasets <- function(object, dataset, name) {
#'   #stopifnot(!inherits(object, 'list'))
#'   if (is.null(object))
#'     setNames(list(dataset), nm = name)
#'   else
#'     append(object, setNames(list(dataset), nm = name))
#'   }


# #' @rdname dataset-processing
# setMethod("addDatasets",
#           "QFeatures",
#           function(object, dataset) {
#             if (missing(dataset))
#               stop("Provide a dataset to add.")
#
#             if (is.numeric(range)) range <- names(object)[[range]]
#             addAssay(dataset,
#                      dataset[[length(dataset)]],
#                      name = name)
#           })

