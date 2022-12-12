
if (interactive()){
library(shinyjqui)
library(DT)
library(shinyjs)
library(shiny)
library(Magellan)

data(data_na)

ui <- fluidPage(
  tagList(
    mod_format_DT_ui("dt_demo_nocolor"),
    br(),br(),br(),
    mod_format_DT_ui("dt_demo_NA_colored"),
    br(),br(),br(),
    mod_format_DT_ui("dt_demo_virtual_cols")
  )
)

server <- function(input, output, session) {

  # Example 1
  mod_format_DT_server("dt_demo_nocolor",
    data = reactive({data_na$array1})
  )

  # Example 2
  .style <- list(
    cols = colnames(data_na$array1),
    vals = colnames(data_na$array1),
    unique = c(NA),
    pal = 'lightgrey'
  )
  mod_format_DT_server("dt_demo_NA_colored",
    data = reactive({data_na$array1}),
    withDLBtns = TRUE,
    style = reactive({
      list(
        cols = colnames(data_na$array1),
        vals = colnames(data_na$array1),
        unique = c(NA),
        pal = 'lightgrey'
      )
    })
  )


  #
  # # Example 3
  # # Compute values, store them in virtual columns and
  # # compute colors based on these virtual values
  # #
  virtual_cols <- data_na$array1 < 10
  colnames(virtual_cols) <- paste0('virt_', colnames(data_na$array1))
  df <- cbind(data_na$array1, virtual_cols)

  .style <- list(
    cols = colnames(df)[1:(ncol(df)/2)],
    vals = colnames(df)[(1+(ncol(df) / 2)):ncol(df)],
    unique = c(0,1),
    pal = c('orange', 'lightblue')
  )

  mod_format_DT_server("dt_demo_virtual_cols",
    data = reactive({df}),
    style = reactive({.style}),
    hideCols = reactive({(1+((ncol(df)) / 2)):(ncol(df))})
  )
}

shinyApp(ui, server)
}