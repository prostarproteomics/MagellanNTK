#' @export
call.func <- function(fname,
                      args){
  do.call(eval(parse(text=fname)), args)
}

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


#' @export
GetExtension <- function(name) {
  temp <- unlist(strsplit(name, ".", fixed = T))
  return(temp[length(temp)])
}



#' @title Loads packages
#' 
#' @description Checks if a package is available to load it
#' 
#' @param ll.deps A `character()` vector which contains packages names
#' 
#' @examples 
#' pkgs.require('DAPAR')
#' 
#' @export
#' 
#' @author Samuel Wieczorek
#' 
pkgs.require <- function(ll.deps){
  lapply(ll.deps, function(x) {
    if (!requireNamespace(x, quietly = TRUE)) {
      stop(paste0("Please install ", x, ": BiocManager::install('", x, "')"))
    }
  })
}




#' @title Find the packages of a function
#' 
#' @description This code is extractd from https://sebastiansauer.github.io/finds_funs/
#' 
#' @param f name of function for which the package(s) are to be identified.
#' 
#' @examples 
#' find_funs('filter')
#' 
#' @return 
#' A dataframe with two columns:
# `package_name`: packages(s) which the function is part of (chr)
# `builtin_package`:  whether the package comes with standard R (a 'builtin'  package)
#' 
#' @export

#' 
find_funs <- function(f) {
  
  if ("tidyverse" %in% rownames(installed.packages()) == FALSE) {
    cat("tidyverse is needed for this fuction. Please install. Stopping")
    stop()}
  
  suppressMessages(library(tidyverse))
  
  
  # search for help in list of installed packages
  help_installed <- help.search(paste0("^",f,"$"), agrep = FALSE)
  
  # extract package name from help file
  pckg_hits <- help_installed$matches[,"Package"]
  
  if (length(pckg_hits) == 0) pckg_hits <- "No_results_found"
  
  
  # get list of built-in packages
  
  pckgs <- installed.packages()  %>% as_tibble
  pckgs %>%
    dplyr::filter(Priority %in% c("base","recommended")) %>%
    dplyr::select(Package) %>%
    distinct -> builtin_pckgs_df
  
  # check for each element of 'pckg hit' whether its built-in and loaded (via match). Then print results.
  
  results <- tibble(
    package_name = pckg_hits,
    builtin_pckage = match(pckg_hits, builtin_pckgs_df$Package, nomatch = 0) > 0,
    loaded = match(paste("package:",pckg_hits, sep = ""), search(), nomatch = 0) > 0
  )
  
  return(results)
  
}