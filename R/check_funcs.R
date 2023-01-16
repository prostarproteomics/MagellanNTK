

#' @title xxx
#'
#' @description xxx
#'
#' @param id xxx
#'
#' @return NA
#'
#' @examples
#' NULL
#' 
#' @export
#'
Found_Mod_Funcs <- function(id) {
    
    config.func <- paste0(id, "_conf")
    config.exists <- exists(config.func, envir = .GlobalEnv, mode = "function")
    
    server.func <- paste0(id, "_server")
    server.exists <- exists(server.func, envir = .GlobalEnv, mode = "function")

    ui.func <- paste0(id, "_ui")
    ui.exists <- exists(ui.func, envir = .GlobalEnv, mode = "function")

    if (!server.exists) {
        warning(paste0("Cannot find ", server.func, "()"))
    }

    if (!ui.exists) {
        warning(paste0("Cannot find ", ui.func, "()"))
    }
    
    if (!config.exists) {
        warning(paste0("Cannot finnd ", config.exists, "()"))
    }

    return(server.exists && ui.exists && config.exists)
}



#' @title Clean source code before syntax analysis
#'
#' @description xxx
#'
#' @param file A vector in which each element is a line read from source
#' code file
#'
#' @return A string which contains the source code condensed
#'
#' @author Samuel Wieczorek
#'
#' @examples
#' file <- system.file("extdata/module_examples", "mod_PipelineA_Process1.R", package = "MagellanNTK")
#' CleanSourceCode(file)
#' 
#' @export
#'
CleanSourceCode <- function(file = NULL) {
    if (is.null(file) || file == ''){
        warning (paste0("Argument 'file' is incorrect (current value is '", file, "')."))
        return(NULL)
    }
    
    source <- readLines(file)

    source <- unlist(lapply(source, function(x) gsub(" ", "", x)))

    # Replace " by '
    source <- unlist(lapply(source, function(x) gsub("\"", "'", x)))


    # Remove empty lines
    source <- source[-which(source == "")]

    # Remove comments lines
    res <- which(unlist(lapply(source, function(x) unlist(gregexpr("#", x))[1] == 1)))

    source <- source[-res]

    # Concatenate in one vector
    source <- paste0(source, collapse = "")

    source
}


#' @title xxx
#'
#' @param text xxx
#' @param openPos A `integer`
#'
#' @examples
#' text <- "myfunc <- function(a, b){ return (a+b)}"
#' posParam <- 19
#' FindClosingParenthesis(text, posParam)
#'
#' @return A `integer`
#' 
#' @export
#'
FindClosingParenthesis <- function(text, openPos) {
    closePos <- openPos
    counter <- 1
    while (counter > 0) {
        c <- substr(text, closePos + 1, closePos + 1)
        if (c == "(") {
            counter <- counter + 1
        } else if (c == ")") {
            counter <- counter - 1
        }
        closePos <- closePos + 1
    }
    return(closePos)
}




#' @title Check source code of a module process
#'
#' @description Check if the source code of a process module is correct
#'
#' @details xxxx
#' xxx
#' xxxx
#'
#' @param sourcefile xxx
#'
#' @return A list of two items:
#' * `passed`: A boolean that indicates if the config is correct or not.
#' * `msg`: A `character(1)` as message.
#'
#'
#' @author Samuel Wieczorek
#'
#' @examples
#' f <- system.file("extdata/module_examples", "example_module_PipelineA_Process1.R",
#'     package = "MagellanNTK"
#' )
#' CheckProcessCode(f)
#' 
#' @export
#'
CheckProcessCode <- function(sourcefile = NULL) {
    s.code <- readLines(file(sourcefile))
    # remove every white space from the file so as to make easier the search
    # of keywords
}
