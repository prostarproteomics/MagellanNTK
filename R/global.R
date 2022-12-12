#' @importFrom utils globalVariables
utils::globalVariables(
    c(
        "Change_Current_Pos",
        "Update_Data2send_Vector",
        "Update_State_Screens",
        "PrepareData2Send",
        "Send_Result_to_Caller",
        "GetStringStatus",
        "GetMaxValidated_AllSteps",
        "GetMaxValidated_BeforePos",
        "GetFirstMandatoryNotValidated",
        "Set_All_Skipped",
        "Discover_Skipped_Steps",
        "Unskip_All_Steps",
        "dataModal",
        "ToggleState_ResetBtn",
        "NavPage",
        "LocalReset",
        "ToggleState_Screens",
        "ToggleState_NavBtns",
        "ActionOn_Data_Trigger",
        "Init_process_Server",
        "Init_pipeline_Server",
        "ActionOn_NewPosition",
        "Get_Code_Declare_reactiveValues",
        "Get_Code_Declare_Pipeline_reactiveValues",
        "dataOut",
        "rv"
    )
)

#' @title Describe tags for steps state
#' @field global xxxx
#' @export
global <- list(
    VALIDATED = 1,
    SKIPPED = -1,
    UNDONE = 0
)

#' @title Describe tags for steps state
#' @field default_pos xxxx
#' @export
default_pos <- list(
    VALIDATED = 1,
    SKIPPED = 1,
    UNDONE = 1
)

#' @title CSS class defining a red button
#' @field redBtnClass xxxx
#' @export
redBtnClass <- "btn-danger"


#' @title Css class defining the previous and next buttons
#' @field PrevNextBtnClass xxxx
#' @export
PrevNextBtnClass <- "btn-info"


#' @title Css class defining the success state
#' @field btn_success_color xxxx
#' @export
btn_success_color <- "btn-success"


#' @title Css class defining the info state
#' @field optionsBtnClass xxxx
#' @export
optionsBtnClass <- "info"


#' @title Css style defining general layout
#' @field btn_style xxxx
#' @export
btn_style <- "display:inline-block; vertical-align: middle; padding: 7px"


#' @title Css style defining the action buttons
#' @field actionBtnClass xxxx
#' @export
actionBtnClass <- "btn-primary"


GlobalSettings <- list(
    tl_v_next_icon = shiny::icon('arrow-down'),
    tl_v_prev_icon = shiny::icon('arrow-up'),
    tl_h_next_icon = shiny::icon('arrow-right'),
    tl_h_prev_icon = shiny::icon('arrow-left')
)

