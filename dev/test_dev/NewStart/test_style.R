library(shiny)

ui <- fluidPage(
  uiOutput('dynTL')
)


server <- function(input, output){

  output$dynTL <- renderUI({
    n <- 5
      tags$div(style="text-align: center; ",
               tags$div( style="display:inline-block;",
                         div(style="
                         padding-left: auto;
                         padding-right: auto;
                         padding-bottom: 0;
                         border-bottom: 5px solid black;
                        ",
                        div(class = 'toto',
                        style="position: relative;
                        z-index: 1;
                        display:inline-block;
                        border-radius: 50%;
                        width: 30px;
                        height: 30px;
                        background: yellow;
                        border: 3px solid red;
                        margin-bottom: 10px;"),
                        div(class='before',
                            style="border-top: 2px solid #dfdfdf;
        content:'titi';
        margin: 0 auto; 
        position: absolute; 
        top: 50%; left: 0; right: 0; bottom: 0;
        width: 95%;
        z-index: -1;"),
                        div(class='toto::after',
                            style="display:inline-block;
                            border: 3px solid blue;")),
                        
                         div(style="display:inline-block;
                         text-align: center;
                         width: 0;
                         height: 0;
                         border-left: 10px solid transparent;
                         border-right: 10px solid transparent;
                           border-bottom: 15px solid grey;
                           margin-top: 5px;
                           "
               )
               ))
  })
  
}


shinyApp(ui, server)
