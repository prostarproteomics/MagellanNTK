splash_ui <- function(id) {
  ns <- NS(id)
  div(id = ns("splash_screen"), 
      style = "text-align:center; padding-top:250px;",
      uiOutput(ns('splashscreen')))
}

splash_server <- function(input, output, session) {
  ns <- session$ns
  hide("splash_screen", anim = TRUE, animType = "fade", time = 3)
  
  output$splashscreen <- renderUI({
    tagList(
      shinyjs::inlineCSS(progressBar_css),
      absolutePanel(
        id  = ns("AbsolutePanel"),
        class = "panel panel-default",
        style= "text-align: center; background-color: #25949A;",
        top = '30%',
        left = '25%',
        width = "50%",
        height = "150px",
        draggable = FALSE,
        fixed = TRUE,
        tagList(
          tags$h1(style='text-align: center; color: white', "Prostar is loading, please wait..."),
          br(),
          tags$div(class="progress",
                   tags$div(class="indeterminate")
          )
        )
      )
    )
  })
}



progressBar_css <- ".progress {
  position: relative;
  height: 4px;
  display: block;
  width: 100%;
  background-color: #E87352;
  border-radius: 2px;
  background-clip: padding-box;
  margin: 0.5rem 0 1rem 0;
  overflow: hidden; }
  
  
.progress .determinate {
    position: absolute;
    background-color: inherit;
    top: 0;
    bottom: 0;
    background-color: #E87352;
    transition: width .3s linear; }
    
.progress .indeterminate {
    background-color: blue; }
    .progress .indeterminate:before {
      content: '';
      position: absolute;
      background-color: #ffffff;
      top: 0;
      left: 0;
      bottom: 0;
      will-change: left, right;
      -webkit-animation: indeterminate 2.1s cubic-bezier(0.65, 0.815, 0.735, 0.395) infinite;
              animation: indeterminate 2.1s cubic-bezier(0.65, 0.815, 0.735, 0.395) infinite; }
    
.progress .indeterminate:after {
      content: '';
      position: absolute;
      background-color: #ffffff;
      top: 0;
      left: 0;
      bottom: 0;
      will-change: left, right;
      -webkit-animation: indeterminate-short 2.1s cubic-bezier(0.165, 0.84, 0.44, 1) infinite;
              animation: indeterminate-short 2.1s cubic-bezier(0.165, 0.84, 0.44, 1) infinite;
      -webkit-animation-delay: 1.15s;
              animation-delay: 1.15s; }

@-webkit-keyframes indeterminate {
  0% {
    left: -35%;
    right: 100%; }
  60% {
    left: 100%;
    right: -90%; }
  100% {
    left: 100%;
    right: -90%; } }
@keyframes indeterminate {
  0% {
    left: -35%;
    right: 100%; }
  60% {
    left: 100%;
    right: -90%; }
  100% {
    left: 100%;
    right: -90%; } }
@-webkit-keyframes indeterminate-short {
  0% {
    left: -200%;
    right: 100%; }
  60% {
    left: 107%;
    right: -8%; }
  100% {
    left: 107%;
    right: -8%; } }
@keyframes indeterminate-short {
  0% {
    left: -200%;
    right: 100%; }
  60% {
    left: 107%;
    right: -8%; }
  100% {
    left: 107%;
    right: -8%; } }"
