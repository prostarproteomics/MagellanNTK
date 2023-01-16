#' @export
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

#' @title xxx
#' @description xxx
#' @export
global <- list(
    VALIDATED = 1,
    SKIPPED = -1,
    UNDONE = 0
)

#' @title xxx
#' @description xxx
#' @export
default_pos <- list(
    VALIDATED = 1,
    SKIPPED = 1,
    UNDONE = 1
)


#' @title xxx
#' @description xxx
#' @export
GlobalSettings <- list(
    tl_v_next_icon = shiny::icon('arrow-down'),
    tl_v_prev_icon = shiny::icon('arrow-up'),
    tl_h_next_icon = shiny::icon('arrow-right'),
    tl_h_prev_icon = shiny::icon('arrow-left'),
    actionBtnClass = "btn-primary",
    optionsBtnClass = "info",
    redBtnClass = "btn-danger",
    PrevNextBtnClass = "btn-info",
    btn_success_color = "btn-success"
)




#DAPAR.loc <- DAPARdata.loc <- Prostar.loc <- NULL


library(fresh)
#library(DaparToolshed)
#library(QFeatures)
#library(DaparViz)
#library(MagellanNTK)


## URLs for the .md files stored in the website github directory
# base_URL <- "http://www.prostar-proteomics.org/md/"
# URL_FAQ <- paste0(md.dir, "FAQ.md")
# URL_links <- paste0(md.dir, "links.md")
# URL_ProstarPresentation <- paste0(md.dir, "presentation.md")
# URL_formerReleases <-paste0(md.dir, "formerReleases.md")
# URL_versionNotes <- paste0(md.dir, "versionNotes.md")


actionBtnClass <- "btn-primary"

# Bootstrap colors for buttons

# ="btn btn-primary">Primary</button>
#   <button type="button" class="btn btn-secondary">Secondary</button>
#   <button type="button" class="btn btn-success">Success</button>
#   <button type="button" class="btn btn-danger">Danger</button>
#   <button type="button" class="btn btn-warning">Warning</button>
#   <button type="button" class="btn btn-info">Info</button>
#   <button type="button" class="btn btn-light">Light</button>
#   <button type="button" class="btn btn-dark">Dark</button>
#   
#   <button type="button" class="btn btn-link">Link</button>

redBtnClass <- "btn-danger"
PrevNextBtnClass <- "btn-info"
btn_success_color <- "btn-success"
optionsBtnClass <- "info"

btn_style <- "display:inline-block; vertical-align: middle; padding: 7px;"

listBrewerPalettes <- c("Dark2 (qualit.)" = "Dark2",
    "Accent (qualit.)"="Accent",
    "Paired (qualit.)" = "Paired",
    "Pastel1 (qualit.)" = "Pastel1",
    "Pastel2 (qualit.)" = "Pastel2",
    "Set1 (qualit.)" = "Set1",
    "Set2 (qualit.)" = "Set2", 
    "Set3 (qualit.)" = "Set3",
    "BrBG (diverging)"="BrBG",
    "PiYG (diverging)"=  "PiYG",
    "PRGn (diverging)" ="PRGn",
    "PuOr (diverging)" ="PuOr",
    "RdBu (diverging)"="RdBu",
    "RdGy (diverging)" ="RdGy",
    "RdYlBu (diverging)" ="RdYlBu",
    "RdYlGn (diverging)" ="RdYlGn",
    "Spectral (diverging)"="Spectral")