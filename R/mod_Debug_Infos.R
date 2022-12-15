#' @title Module debugging
#'
#' @description  A shiny Module which shows the values of the datasets variables
#' along the different processes of a process nor pipeline.
#' 
#' @name mod_Debug_Infos
#' 
#' @examples 
#' #'
#' @examples
#' if (interactive()) {
#'     ui <- fluidPage(
#'         mod_Debug_Infos_ui("tbl")
#'     )
#'     server <- function(input, output) {
#'         mod_Debug_Infos_server(
#'         id = "tbl", 
#'         data = reactive({head(iris)
#'         }))
#'     }
#'     shinyApp(ui, server)
#' }
#' 
NULL



#' @param id xxx
#'
#' @rdname mod_Debug_Infos
#' 
#' @export
#'
mod_Debug_Infos_ui <- function(id) {
    ns <- NS(id)
    uiOutput(ns("show_Debug_Infos"))
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
#' @rdname mod_Debug_Infos
#'
#' @importFrom DT renderDT DTOutput formatStyle %>% styleEqual
#' @importFrom S4Vectors DataFrame
#'
mod_Debug_Infos_server <- function(id,
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

        # The following functions are only there for dev and debugging reasons
        # They will not be part of the final code

        output$show_Debug_Infos <- renderUI({
            wellPanel(
                h3(title),
                # div(DT::DTOutput(ns('show_steps_infos')),
                #     style = "font-size: 100%; width: 30%"),
                # div(DT::DTOutput(ns('show_varContent')),
                #     style = "font-size: 100%; width: 30%"),
                #
                uiOutput(ns("show_is_enabled")),
                fluidRow(
                    column(width = 4, DT::DTOutput(ns("show_steps_infos"))),
                    column(width = 4, DT::DTOutput(ns("show_varContent")))
                )
            )
        })



        output$show_is_enabled <- renderUI({
            p(paste0("is.enabled() = ", as.numeric(is.enabled())))
        })


        GetVariableContent <- reactive({
            VC <- data.frame(
                paste0(names(dataIn()), collapse = "<br>"),
                paste0(names(rv.dataIn()), collapse = "<br>"),
                paste0(names(dataOut()$value), collapse = "<br>")
            )
            colnames(VC) <- c(
                '<span style="color:red">dataIn()</span>',
                '<span style="color:red">rv$dataIn</span>',
                '<span style="color:red">dataOut()$value</span>'
            )

            VC
        })


        GetData <- reactive({
            req(c(steps.enabled(), steps.status(), steps.skipped()))

            # browser()
            df <- DataFrame(
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
            df
        })

        output$show_steps_infos <- DT::renderDT({
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
            df <- GetVariableContent()
            DT::datatable(df,
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
