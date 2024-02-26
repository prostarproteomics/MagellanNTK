#' @title This function exports a data.frame to a Excel file.
#'
#' @param df An data.frame
#'
#' @param tags xxx
#'
#' @param colors xxx
#'
#' @param tabname xxx
#'
#' @param filename A character string for the name of the Excel file.
#'
#' @return A Excel file (.xlsx)
#'
#' @author Samuel Wieczorek
#'
#' @export
#'
#'
#' @examples
#' data(Exp1_R25_pept, package="DAPARdata")
#' df <- Biobase::exprs(Exp1_R25_pept[seq_len(100)])
#' tags <- GetMetacell(Exp1_R25_pept[seq_len(100)])
#' colors <- list(
#'     "Missing POV" = "lightblue",
#'     "Missing MEC" = "orange",
#'     "Quant. by recovery" = "lightgrey",
#'     "Quant. by direct id" = "white",
#'     "Combined tags" = "red"
#' )
#' write.excel(df, tags, colors, filename = "toto")
#' 
write.excel <- function(df,
                        tags = NULL,
                        colors = NULL,
                        tabname = "foo",
                        filename = NULL) {
  pkgs.require(c('openxlsx', 'tools'))
  
  if (is.null(filename)) {
    filename <- paste("data-", Sys.Date(), ".xlxs", sep = "")
  } else if (tools::file_ext(filename) != "") {
    if (tools::file_ext(filename) != "xlsx") {
      stop("Filename extension must be equal to 'xlsx'. Abort...")
    } else {
      fname <- filename
    }
  } else {
    fname <- paste(filename, ".xlsx", sep = "")
  }
  
  unique.tags <- NULL
  if (!is.null(tags) && !is.null(colors)) {
    unique.tags <- unique(as.vector(as.matrix(tags)))
    if (!isTRUE(
      sum(unique.tags %in% names(colors)) == length(unique.tags))) {
      warning("The length of colors vector must be equal to the number 
            of different tags. As is it not the case, colors are ignored")
    }
  }
  
  wb <- openxlsx::createWorkbook(fname)
  openxlsx::addWorksheet(wb, tabname)
  openxlsx::writeData(wb, sheet = 1, df, rowNames = FALSE)
  
  
  # Add colors w.r.t. tags
  if (!is.null(tags) && !is.null(colors)) {
    if (isTRUE(
      sum(
        unique.tags %in% names(colors)) == length(unique.tags)
    )
    ) {
      lapply(seq_len(length(colors)), function(x) {
        list.tags <- which(names(colors)[x] == tags, arr.ind = TRUE)
        openxlsx::addStyle(wb,
                           sheet = 1,
                           cols = list.tags[, "col"],
                           rows = list.tags[, "row"] + 1,
                           style = openxlsx::createStyle(fgFill = colors[x])
        )
      })
    }
  }
  
  openxlsx::saveWorkbook(wb, fname, overwrite = TRUE)
}