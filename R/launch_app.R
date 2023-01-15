launch_app <- function(dev.mode = FALSE){
  shinyOptions(dev.mode = dev.mode)
  source(system.file("app.R", package = "MagellanNTK", local = TRUE, chdir = TRUE))$value   
  
}