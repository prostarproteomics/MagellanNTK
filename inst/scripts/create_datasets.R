

#' @title Build an example list
#' @description Creates a list which contains example info to be
#' used to create instances of `MultiAssayExperiment` 
#' @export
#' @name build_example_datasets
#' @return A list
#' 
#' @examples
#' build_toy_example()
#' 
#' create_list_data()
#' 
#' 
NULL



#' @export
#' @rdname build_example_datasets
#' 
build_toy_example <- function(name = 'original'){
  
  data <- data.frame(
    matrix(sample.int(30, 30), ncol = 6, 
      dimnames = list(1:5, LETTERS[1:6]))
    )

  save(data, file = 'data/data.rda')
  return(data)
}


## ---------------------------------------------------------
## Create the vdata dataset
## ---------------------------------------------------------
create_list_data <- function(){

  lldata <- list(
    data1 = build_toy_example(), 
    data2 = build_toy_example(), 
    data3 = build_toy_example())

  save(lldata, file = 'data/lldata.rda')
}


## ---------------------------------------------------------
## Create small datasets based on `DAPARdata` package
## ---------------------------------------------------------
# create_sub_R25 <- function(){
#   library(omXplore)
#   
#   
#   data("Exp1_R25_pept", package = 'DAPARdata')
#   data("Exp1_R25_prot", package = 'DAPARdata')
# 
#   sub_R25 <- convert_to_mae(
#     list(
#       peptide = Exp1_R25_pept[150:170],
#       protein = Exp1_R25_prot[1:21]
#       )
#   )
# 
# save(sub_R25, file = 'data/sub_R25.rda')
# }


