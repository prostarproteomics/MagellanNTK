Build_SkippedInfoPanel <- function(steps.status, current.pos, config){
  
  renderUI({
    current_step_skipped <- steps.status[current.pos] == global$SKIPPED
  req(current_step_skipped)
  process_entirely_skipped <- isTRUE(sum(steps.status) == global$SKIPPED * length(config$steps))

  if (process_entirely_skipped){
    # This case appears when the process has been skipped from the
    # pipeline. Thus, it is not necessary to show the info box because
    # it is shown below the timeline of the pipeline
  } else {
    txt <- paste0("This ", config$type, " is skipped so it has been disabled.")
    wellPanel(
      style = "background-color: #7CC9F0; opacity: 0.72; padding: 0px;
                   align: center; vertical-align: center;",
      height = 100,
      width = 300,
      align = "center",
      p(style = "color: black;", paste0("Info: ",txt))
    )
  }
})
  
}