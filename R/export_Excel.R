

#' @title This function exports a single data.frame to a Excel file.
#'
#' @param df A data.frame
#'
#' @param style xxx
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
#' # Colorize the cells with missing values
#' data(data_na)
#' n <- nrow(data_na$array1)*ncol(data_na$array1)
#' style <- data.frame(matrix(rep('lightgrey', n), nrow=nrow(data_na$array1)))
#' style[is.na(data_na$array1)] <- 'orange'
#' write.excel(data_na$array1, style, tabname = 'foo', filename = "test")
#' 
write.excel <- function(df,
  style = NULL,
  tabname = "foo",
  filename = NULL,
  rownames = FALSE,
  colnames = TRUE) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Please install openxlsx: BiocManager::install('openxlsx')")
  }
  
  if (!requireNamespace("tools", quietly = TRUE)) {
    stop("Please install tools: BiocManager::install('tools')")
  }
  
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

  wb <- openxlsx::createWorkbook(fname)
  openxlsx::addWorksheet(wb, tabname)
  openxlsx::writeData(wb, 
    sheet = 1, 
    df, 
    rowNames = rownames,
    colNames = colnames
    )
  
  if(!is.null(style)){
    if (sum(dim(style) == dim(df))==2){
   for (i in 1:nrow(df)){
     for(j in 1:ncol(df)){
       openxlsx::addStyle(wb,
         sheet = 1,
         cols = j + isTRUE(rownames),
         rows = i + isTRUE(colnames),
         style = openxlsx::createStyle(fgFill = style[i,j])
       )
     }
   }
    } else {
    warning('Malformed style data.frame. Cannot use it.')
    }
  }
  openxlsx::saveWorkbook(wb, fname, overwrite = TRUE)
}

