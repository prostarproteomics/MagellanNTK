if(interactive()){
  library(shiny)
library(shinyjs)
options(shiny.fullstacktrace = TRUE)

config <- Config(
  name = "test_TL_verticale",
  #mode = 'process',
  steps = c('Description', 'step2', 'step3', 'Save'),
  mandatory = c(TRUE, FALSE, FALSE, TRUE)
)

ui <- fluidPage(
  fluidRow(
    column(width=4,
           
           tagList(
             sliderInput('setActiveBtn', 'Set active', value=1,
                min=1, max=4, step=1, width = '150px'),
             actionButton('randDone_btn', 'rand done'),
             actionButton('randSkipped_btn', 'rand skipped'),
             actionButton('randDisabled_btn', 'rand disabled'),
             shinyjs::inlineCSS(
               sass::sass(
                 sass::sass_file(
                   system.file('app/www/sass/v_timeline.sass', package='Magellan')
                 )))
           )
    ),
    column(width = 4,
           uiOutput('show_vTL')
    )
  
  )
  
)


server <- function(input, output){
  
  rv <- reactiveValues(
    status = c(0, 0, 0, 0),
    current.pos = 1,
    tl.tags.enabled = c(1, 1, 1, 1),
    classForDiv = rep("sub_box", 4)
  )

  observeEvent(input$setActiveBtn, {
    rv$classForDiv <- rep("sub_box", 4)
    rv$classForDiv[input$setActiveBtn] <- paste0(rv$classForDiv[input$setActiveBtn], " active")
  })
  
  observeEvent(input$randDone_btn, {
    ind <- sample(seq_len(4), sample(seq_len(4),1))
     rv$status[ind] <- !rv$status[ind]
     print(rv$status)
  })
  
  observeEvent(input$randDisabled_btn, {
    ind <- sample(seq_len(4), sample(seq_len(4),1))
    rv$tl.tags.enabled[ind] <- !rv$tl.tags.enabled[ind]
    print(rv$tl.tags.enabled)
  })
  
  observeEvent(input$randSkipped_btn, {
    ind <- sample(seq_len(4), sample(seq_len(4),1))
    rv$status[ind] == -rv$status[ind]
    print(rv$status)
  })
  
  
  
  output$show_vTL <- renderUI({
    
    tags$div(class="box",
             tags$div(class=rv$classForDiv[1],p('Filtration')),
             tags$div(class=rv$classForDiv[2],p('Normalization')),
             tags$div(class=rv$classForDiv[3], p('Imputation')),
             tags$div(class=rv$classForDiv[4],p('Aggregation'))
    )
  })
}


shinyApp(ui, server)
}