#' @importFrom utils globalVariables
utils::globalVariables(
  c("Change_Current_Pos",
    'Update_Data2send_Vector',
    'Update_State_Screens', 
    'PrepareData2Send',
    'Send_Result_to_Caller',
    'GetStringStatus', 
    'GetMaxValidated_AllSteps',
    'GetMaxValidated_BeforePos', 
    'GetFirstMandatoryNotValidated',
    'Set_All_Skipped', 
    'Discover_Skipped_Steps',
    'Unskip_All_Steps', 
    'dataModal', 
    'ToggleState_ResetBtn',
    'NavPage',
    'LocalReset', 
    'ToggleState_Screens',
    'ToggleState_NavBtns', 
    'ActionOn_Data_Trigger',
    'Init_process_Server',
    'Init_pipeline_Server',
    'ActionOn_NewPosition',
    'Get_Code_Declare_reactiveValues',
    'Get_Code_Declare_Pipeline_reactiveValues',
    'dataOut',
    'rv'
  )
)

#' @title xxxx
#' @description xxx
#' @field btn_style xxxx
#' @export
global = list(VALIDATED = 1,
              SKIPPED = -1,
              UNDONE = 0
)

#' @title xxxx
#' @description xxx
#' @field btn_style xxxx
#' @export
default_pos =list(VALIDATED = 1,
                  SKIPPED = 1,
                  UNDONE = 1
)

#' @title xxxx
#' @description xxx
#' @field btn_style xxxx
#' @export
redBtnClass <- "btn-danger"

#' @title xxxx
#' @description xxx
#' @field btn_style xxxx
#' @export
PrevNextBtnClass <- "btn-info"

#' @title xxxx
#' @description xxx
#' @field btn_style xxxx
#' @export
btn_success_color <- "btn-success"

#' @title xxxx
#' @description xxx
#' @field btn_style xxxx
#' @export
optionsBtnClass <- "info"

#' @title xxxx
#' @description xxx
#' @field btn_style xxxx
#' @export
btn_style <- "display:inline-block; vertical-align: middle; padding: 7px"
