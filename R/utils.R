
#' @title 
#' xxx
#' 
#' @description 
#' Returns the date and time in timestamp UNIX format.
#' 
#' @return NA
#' 
#' @export
#' 
Timestamp <- function(){ 
  if(verbose) cat(paste0('::Timestamp()'))
  as.numeric(Sys.time())
}



#' @title Datasets processing
#'
#' @description
#'
#' This manual page describes manipulation methods using [list] objects. In the following
#' functions, if `object` is of class `list`, and optional assay
#' index or name `i` can be specified to define the assay (by name of
#' index) on which to operate.
#'
#' The following functions are currently available:
#'
#' - `Keep_Datasets_from_Object(object, range)` keep datasets in object which
#' are in range
#'
#' - `Add_Datasets_to_Object(object, dataset, name)` add the 'dataset' to the object (of type list)
#' 
#' @details
#' The object must be of type list. Thetwo functions are implemented here for a simple list.
#' For other dataset classes, their implementation must be part of the package which uses Magellan.
#'
#' @param object An object of class `list`.
#'
#' @param range A xxxx
#'
#' @param dataset `character(1)` providing the base with respect to which
#'     logarithms are computed. Defaults is 2.
#'
#' @param name A `character(1)` naming the new assay name.
#'
#' @return An processed object of the same class as `object`.
#' 
#' @aliases Keep_Datasets_from_Object Keep_Datasets_from_Object,list-method Keep_Datasets_from_Object,QFeatures-method
#' @aliases Add_Datasets_to_Object Add_Datasets_to_Object,list-method Add_Datasets_to_Object,QFeatures-method
#'
#' @name dataset-processing
#'
#' @rdname dataset-processing
#'
#' @examples
#'
NULL

## -------------------------------------------------------
##   Keep datasets from object
## -------------------------------------------------------

#' @exportMethod Keep_Datasets_from_Object
#' @rdname dataset-processing
setMethod("Keep_Datasets_from_Object",
          "list",
          function(object, range) {
            if (missing(range))
              stop("Provide range of assays to be processed")
            
            if (is.numeric(range)) range <- names(object)[[range]]
            object[range]
          })

#' @rdname dataset-processing
setMethod("Keep_Datasets_from_Object",
          "QFeatures",
          function(object, range) {
            if (missing(range))
              stop("Provide range of assays to be processed")

            if (is.numeric(range)) range <- names(object)[[range]]

            object[ , , range]
          })




## -------------------------------------------------------
##   Add datasets to object
## -------------------------------------------------------

#' @exportMethod Add_Datasets_to_Object
#' @rdname dataset-processing
setMethod("Add_Datasets_to_Object",
          "list",
          function(object, dataset, name) {
            append(object, setNames(list(dataset), nm = name))
          })

# #' @rdname dataset-processing
# setMethod("Add_Datasets_to_Object",
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
