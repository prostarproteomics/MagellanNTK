#' @title Module debugging
#'
#' @description  A shiny Module which shows the values of the datasets variables
#' along the different processes of a process nor pipeline.
#' 
#' @name Debug_Infos
#' 
#' @examples 
#' #'
#' @examples examples/test_mod_debug_infos.R
#' 
NULL



#' @param id xxx
#'
#' @rdname Debug_Infos
#' 
#' @export
#'
Debug_Infos_ui <- function(id) {
    ns <- NS(id)
    wellPanel(
      uiOutput(ns('title')),
    uiOutput(ns("show_is_enabled")),
      fluidRow(
        column(width = 4, DT::DTOutput(ns("show_steps_infos"))),
        column(width = 4, DT::DTOutput(ns("show_varContent")))
      )
    )
}


#' @param id xxx
#' @param title The title of the Panel which contains the debugging tables
#' @param config An instance of the class `Config`
#' @param rv.dataIn xxx
#' @param dataIn A `list()` of data.frames
#' @param dataOut A `list()` of data.frames
#' @param steps.enabled A `logical()` xxxx
#' @param steps.status A `logical()` xxxx
#' @param steps.skipped A `logical()` xxxx
#' @param current.pos A `integer(1)` which is the indice of the active step.
#' @param is.enabled A `logical(1)` xxxx
#'
#' @return NA
#' 
#' @export
#'
#' @rdname Debug_Infos
#'
#' @importFrom DT renderDT DTOutput formatStyle %>% styleEqual
#' @importFrom S4Vectors DataFrame
#'
Debug_Infos_server <- function(id,
    title = NULL,
    config = reactive({NULL}),
    rv.dataIn = reactive({NULL}),
    dataIn = reactive({NULL}),
    dataOut = reactive({NULL}),
    steps.enabled = reactive({NULL}),
    steps.status = reactive({NULL}),
    steps.skipped = reactive({NULL}),
    current.pos = reactive({NULL}),
    is.enabled = reactive({NULL})
    ) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        output$title <- renderUI({
          title <- 'Title'
          if (!is.null(title))
            title <- title
          
          h3(title)
        })
        
        
        output$show_is_enabled <- renderUI({
          txt <- 'NA'
          if (!is.null(is.enabled()))
            txt <- as.numeric(is.enabled())
          
          p(paste0("is.enabled() = ", txt))
        })


        GetVariableContent <- reactive({
          
          dataIn <- 'NA'
          rv.dataIn <- 'NA'
          dataOut <- 'NA'
          
          if(!is.null(rv.dataIn())){
            dataIn <- names(dataIn())
            rv.dataIn <- names(rv.dataIn())
            dataOut <- names(dataOut()$value)
          }
            
            VC <- data.frame(
                paste0(dataIn, collapse = "<br>"),
                paste0(rv.dataIn, collapse = "<br>"),
                paste0(dataOut, collapse = "<br>")
            )
            colnames(VC) <- c(
                '<span style="color:red">dataIn()</span>',
                '<span style="color:red">rv$dataIn</span>',
                '<span style="color:red">dataOut()$value</span>'
            )
            
            VC
        })


        GetData <- reactive({
          df <- NULL
          # It is not necessary th test the value NLUU/not NULL of all variables
            # because if a dataset is loaded, then all these variables are not
            # NULL. Thus, one test only one of these variables
          if (!is.null(steps.status())){

           df <- data.frame(
                status = unlist(lapply(steps.status(), function(x) {
                    paste0(GetStringStatus(x, TRUE), " (", x, ")")
                })),
                enabled = steps.enabled(),
                skipped = steps.skipped(),
                currentPos = unlist(
                    lapply(
                        seq_len(length(steps.status())),
                        function(x) current.pos() == x
                    )
                )
            )
            rownames(df) <- names(steps.status())
          }
          
          
            df
        })

        output$show_steps_infos <- DT::renderDT({
          req(GetData())
          df <- as.data.frame(GetData())
            DT::datatable(df,
                escape = FALSE,
                rownames = TRUE,
                class = "compact",
                options = list(
                    dom = "t",
                    autoWidth = FALSE,
                    columnDefs = list(
                        list(
                            targets = c(4), visible = FALSE
                        )
                    )
                )
            ) %>%
                DT::formatStyle(
                    "currentPos",
                    target = "row",
                    color = DT::styleEqual(c(FALSE, TRUE), c("grey", "blue")),
                    backgroundSize = "98% 48%",
                    backgroundRepeat = "no-repeat",
                    backgroundPosition = "center"
                )
        })



        output$show_varContent <- DT::renderDT({
            req(GetVariableContent())
          
          DT::datatable(GetVariableContent(),
                escape = FALSE,
                rownames = FALSE,
                class = "compact",
                options = list(
                    dom = "t",
                    autoWidth = FALSE
                )
            )
        })
    })
}
