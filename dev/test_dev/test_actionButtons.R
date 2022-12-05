library(shiny)
library(shinyWidgets)
library(shinyjs)

options(shiny.fullstacktrace = TRUE)



ui <- fluidPage(
  
  tagList(
    actionButton('change', 'Change buttons style'),
    br(),br(),
    actionButton("nextBtn",
               "Step 1",
               style='padding:4px;
               margin-bottom: 30px;
               font-size:80%'),
    br(),
  actionButton("nextBtn",
               "Step 2",
               style='padding:4px;
               margin-bottom: 30px;
               font-size:80%;
               box-shadow: 0 0 0 3px black;'),
  br(),
  uiOutput('button3')
  )
)


server <- function(input, output){
  
  observeEvent(input$change, {
    
  })
  
  
  output$button3 <- renderUI({
    
    .style <- paste0("padding:4px;
               margin-bottom: 30px;
               font-size: 18px;
               font-weight: 600;
               width: 100px;
               height: 50px;
               background-color: white;
               border: ", input$change, "px solid red")
    actionButton("nextBtn",
                 "Step 3",
                 style=.style)
    
  })
}


shinyApp(ui, server)
