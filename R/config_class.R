##' @export Config
##' @exportClass Config
##' @rdname config
Config <- setClass("Config",
         representation(name = "character", 
                        mode = "character",
                        steps = "vector",
                        mandatory = "vector",
                        path_to_md_dir = "character",
                        ll.UI = "list"
                        ),
         prototype(name = character(0), 
                   mode = character(0),
                   steps = character(0),
                   mandatory = character(0),
                   path_to_md_dir = character(0),
                   ll.UI = list()
                   ),
         validity = function(object) {
           passed <- TRUE
           passed <- length(object@name) == 1
           passed <- length(object@mode)==1
           passed <- length(object@steps) > 1
           passed <- length(object@mandatory) > 1
           passed <- length(object@path_to_md_dir) == 1
           
           if (length(object@steps) != length(object@mandatory)){
             passed <- FALSE
             msg <- "The length of 'steps' and 'mandatory' must be equal."
           }

           
           return(passed)
         })

