appDir <- getwd()
dev.mode.default <- FALSE
dev.mode <- getShinyOption("dev.mode", dev.mode.default)

#shinyApp(ui = ui, server = server)