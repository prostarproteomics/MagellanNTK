
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
#' @value A `boolean(1)`
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
 browser()
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
#' @param widget A `Shiny` widget
#' @param condition A `logical(1)` to hide/show the widget.
#'
#' @return NA
#'
#' @author Samuel Wieczorek
#' 
#' @examples
#' if (interactive()) {
#'     ui <- shiny::fluidPage(uiOutput('foo'))
#'     
#'     server <- function(input, output, session) {
#'     wgt <- actionButton('foo_btn', 'foo')
#'     output$foo <- renderUI({toggleWidget(wgt, FALSE)})
#'          }
#'     shiny::shinyApp(ui, server)
#' }
#'
#' @export
#' 

toggleWidget <- function(widget, condition) {
    tagList(
        shinyjs::useShinyjs(),
        if (isTRUE(condition)) {
            widget
        } else {
            shinyjs::disabled(widget)
        }
    )
}




#' @title
#' Timestamp in UNIX format.
#'
#' @description
#' Returns the date and time in timestamp UNIX format.
#'
#' @return A `integer()`.
#' @examples Timestamp()
#' 
#' @export
#'
Timestamp <- function() {
    as.numeric(Sys.time())
}



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
#' - `Keep_Datasets_from_Object(object, range)` keep datasets in object which
#' are in range
#'
#' - `Add_Datasets_to_Object(object, dataset, name)` add the 'dataset' to the 
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
#' @aliases Keep_Datasets_from_Object Keep_Datasets_from_Object,list-method
#' @aliases Add_Datasets_to_Object Add_Datasets_to_Object,list-method
#'
#' @name dataset-processing
#'
#' @importFrom methods setMethod new
#' 
#' @examples 
#' NA
#'
NULL

## -------------------------------------------------------
##   Keep datasets from object
## -------------------------------------------------------

#' @exportMethod Keep_Datasets_from_Object
#' @rdname dataset-processing
setMethod(
    "Keep_Datasets_from_Object",
    "NULL",
    function(object, range) {
        return()
    }
)

#' @rdname dataset-processing
setMethod(
    "Keep_Datasets_from_Object",
    "list",
    function(object, range) {
        if (missing(range)) {
            stop("Provide range of array to be processed")
        }
        if (is.null(object)) {
            return()
        }

        if (is.numeric(range)) range <- names(object)[range]
        object[range]
    }
)

# #' @rdname dataset-processing
# setMethod("Keep_Datasets_from_Object",
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

#' @exportMethod Add_Datasets_to_Object
#'
#' @rdname dataset-processing
setMethod(
    "Add_Datasets_to_Object",
    "NULL",
    function(object, dataset, name) {
        return()
    }
)


#' @rdname dataset-processing
setMethod(
    "Add_Datasets_to_Object",
    "list",
    function(object, dataset, name) {
        if (is.null(object)) {
            setNames(list(dataset), nm = name)
        } else {
            append(object, setNames(list(dataset), nm = name))
        }
    }
)

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






# function to read DT inputs
#' @export
shinyValue <- function(id,num) {
    unlist(lapply(seq_len(num),function(i) {
        value <- input[[paste0(id,i)]]
        if (is.null(value)) NA else value
    }))
}


#' @export
shinyOutput <- function(FUN,id,num,...) {
    inputs <- character(num)
    for (i in seq_len(num)) {
        inputs[i] <- as.character(FUN(paste0(id,i),label=NULL,...))
    }
    inputs
}


# function for dynamic inputs in DT
#' @export
shinyInput <- function(FUN, id ,num,...) {
    inputs <- character(num)
    for (i in seq_len(num)) {
        inputs[i] <- as.character(FUN(paste0(id, i),label=NULL,...))
    }
    inputs
}




# Call this function with all the regular navbarPage() parameters, plus a text parameter,
# if you want to add text to the navbar
#' @export
navbarPageWithText <- function(..., text) {
    navbar <- navbarPage(...)
    textEl <- tags$p(class = "navbar-text", text)
    navbar[[3]][[1]]$children[[1]] <- htmltools::tagAppendChild(
        navbar[[3]][[1]]$children[[1]], textEl)
    navbar
}

# Call this function with an input (such as `textInput("text", NULL, "Search")`) if you
# want to add an input to the navbar
#' @export
navbarPageWithInputs <- function(..., inputs) {
    navbar <- navbarPage(...)
    form <- tags$form(class = "navbar-form", inputs)
    navbar[[3]][[1]]$children[[1]] <- htmltools::tagAppendChild(
        navbar[[3]][[1]]$children[[1]], form)
    navbar
}




###-------------------------------------
#' @export
#' @importFrom shiny reactive
Compute_PCA_nbDimensions <- shiny::reactive({
    # ncp should not be greater than...
    nmax <- 12  
    # pour info, ncp = nombre de composantes ou de dimensions dans les r?sultats de l'ACP
    
    y <- Biobase::exprs(rv$current.obj)
    nprot <- dim(y)[1]
    # If too big, take the number of conditions.
    n <- dim(y)[2] 
    
    if (n > nmax){
        n <- length(unique(Biobase::pData(rv$current.obj)$Condition))
    }
    
    
    ncp <- min(n, nmax)
    ncp
})




#' @export
GetOnlineZipVersion <- function(){
    
    thepage <- readLines('http://prabig-prostar.univ-lyon1.fr/ProstarZeroInstall/')
    substr(thepage[12], regexpr("Prostar_",thepage[12])[1], 2+regexpr("zip",thepage[12])[1])
    
    
    thetable <- XML::readHTMLTable('http://prabig-prostar.univ-lyon1.fr/ProstarZeroInstall/', stringsAsFactors=FALSE)
    onlineZipVersion <- thetable[[1]]$Name[3]
    
    return(onlineZipVersion)
}



#' @export
launchGA <- function(){
    if (system('hostname')=="prabig-prostar"){
        tags$head(includeScript("www/google-analytics.js"))
    } else {
        #tags$head(includeScript("www/google-analytics-ProstarZeroInstall.js"))
    }
    
}


# Dans mod_msnset_explorer.R
#' @export
initComplete <- function(){
    return (JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': 'darkgrey', 'color': 'black'});",
        "}"))
} #comonFunc.R de prostar 2.0