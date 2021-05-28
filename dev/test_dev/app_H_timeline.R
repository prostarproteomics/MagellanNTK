library(shiny)
library(shinyWidgets)
library(shinyjs)
library(R6)
library(tibble)

options(shiny.fullstacktrace = TRUE)

#------------------------ Class TimelineDraw --------------------------------------
source(file.path('../../R', 'class_TimelineDraw.R'), local=TRUE)$value
#source(file.path('../../R', 'class_vTimeline.R'), local=TRUE)$value


config <- list(
  name = "test_TL_verticale",
  steps = c('step1', 'step2', 'step3', 'step4'),
  mandatory = c(TRUE, FALSE, FALSE, TRUE)
)


#tl <- TimelineDraw$new('TL', mandatory = config$mandatory)

ui <- fluidPage(
  tagList(
    sliderInput('setActiveBtn', 'Set active', value=1,
                min=1, max=4, step=1, width = '150px'),
    actionButton('randDone_btn', 'rand done'),
    shinyjs::inlineCSS(sass::sass(sass::sass_file('www/h_timeline.sass'))),
    br(), br(),

    uiOutput('show_vTL')
    
  )
  
)


server <- function(input, output){
  
  rv <- reactiveValues(
    status = c(0, 0, 0, 0),
    current.pos = 1,
    tl.tags.enabled = c(1, 1, 1, 1),
    classForDiv = rep("undone", 4)
  )

  observeEvent(input$setActiveBtn, {
    rv$classForDiv <- rep("undone", 4)
    rv$classForDiv[input$setActiveBtn] <- paste0(rv$classForDiv[input$setActiveBtn], " active")
  })
  
  output$show_vTL <- renderUI({
    
     txt <-  tags$ul(
         tags$div(class='timeline', id='timeline',
             tags$li(class=paste0('li ',rv$classForDiv[1]),
                     tags$div(class='timestamp'),
                     tags$div(class='status',
                              tags$h4('Step1'))
                     ),
             tags$li(class=paste0('li ',rv$classForDiv[2]),
                     tags$div(class='timestamp'),
                     tags$div(class='status',
                              tags$h4('Step2'))
                     ),
             tags$li(class=paste0('li ',rv$classForDiv[3]),
                     tags$div(class='timestamp'),
                     tags$div(class='status',
                              tags$h4('Step3'))
                     ),
             tags$li(class=paste0('li ',rv$classForDiv[4]),
                     tags$div(class='timestamp'),
                     tags$div(class='status',
                              tags$h4('Step4'))
                     )
             )
       )
     print(txt)
     txt
})
}


shinyApp(ui, server)
