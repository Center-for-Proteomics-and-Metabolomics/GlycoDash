#' add_sample_ids UI Function
#'
#' @description A shiny Module to add sample ID's to the data by uploading 
#' either a plate design or a sample list. 
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_add_sample_ids_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$style(HTML(paste0(
      "#",
      ns("box_header"),
      " .awesome-checkbox {padding-top: 7px}",
      "#",
      ns("box_header"),
      " .popover {max-width: 400px !important; color: #333}",
      "#",
      ns("box"),
      " .box-title {width: 100%}",
      "#",
      ns("box_header"),
      # changed all .fa to .fas  because of fontawesome version update
      " .fas {float: right; margin-right: 5px; font-size: 18px}",
      "#",
      ns("box_header"),
      " .direct-chat-contacts {right: 0; background: #222d32!important}",
      "#",
      ns("box_header"),
      " .btn {float: right; border-width: 0px; margin-right: 10px}",
      "#",
      ns("box"),
      " .dropdown {display: inline-block; float: right; width: 330px}",
      "#",
      ns("box_header"),
      " .dropdown-menu {background: #333; right: -30px; left: auto; top: 28px;}"
    ))
    ),
    shinydashboardPlus::box(
      id = ns("box"),
      title = div(
        id = ns("box_header"),
        "Add sample ID's",
        icon("info-circle",
             class = "ml") %>% 
          bsplus::bs_embed_popover(
            title = "Explanation",
            content = HTML(paste0(
              tags$p(paste(
                "Adding sample ID's to your data allows you to see", 
                "which measurement corresponds to which sample.")),
              "Sample ID's will in later steps be used to:",
              tags$ul(tags$li(paste(
                "determine the sample type (e.g. blank, standard,", 
                "negative control, patient, etc.)"
              )), 
              tags$li(paste(
                "link metadata to your data (e.g. age and", 
                "gender of the study subjects)"
              ))
              ))),
            # Don't use body = container here, because then the custom CSS
            # styling for .popover won't be applied
            trigger = "hover", # if trigger = "focus" use tabindex: 0 on icon
            placement = "right",
            html = "true"),
        shinyWidgets::dropdownButton(
          tags$style(HTML(paste0(
            "#",
            ns("dropdown_content"),
            " .fas {float: left}",
            "#",
            ns("dropdown_content"),
            " .btn {float: none; border-width: 1px; width: 280px; margin: 10px}"
          ))),
          div(id = ns("dropdown_content"),
              downloadButton(ns("download_ex_plate_design"),
                             "Download a plate design example file"),
              downloadButton(ns("download_ex_sample_list"),
                             "Download a sample list example file")),
          icon = icon("paperclip",
                      class = "ml"),
          tooltip = shinyWidgets::tooltipOptions(placement = "top",
                                                 title = "Examples"),
          width = "330px",
          size = "xs"
        )),
      width = NULL,
      solidHeader = TRUE,
      status = "primary",
      selectInput(ns("sample_id_method"),
                  "Choose a method to add sample ID's to your data:",
                  choices = c("Upload a plate design",
                              "Upload a sample list")) %>% 
        bsplus::bs_embed_popover(
          title = "Method to add sample ID's",
          content = HTML(paste0(
            tags$b("Plate design:"),
            tags$p(paste(
              "You can only use this method when your sample names contain",
              "information on the plate and well position of the sample,", 
              "in the correct format:"
            )),
            "<p>plate<i>[insert plate number here]</i>_<i>[insert well position here]</i></p>",
            "An example of a valid sample name is:",
            "<p>\"38160_38161_IM5_<b>plate1_A8</b>_01_Spike 20210409_000237.raw\"</P",
            br(),
            br(),
            tags$p(tags$b("Sample list:"),
                   br(),
                   paste(
                     "Use this method when your samples were not measured on plates",
                     "or when your sample names don't meet the requirements described above."
                   ))
          )),
          html = "true",
          trigger = "hover",
          placement = "right"
        ),
      shinyWidgets::materialSwitch(ns("switch_two_plate_designs"),
                                   "Add separate plate design files for specific and and for total Ig samples.",
                                   status = "primary",
                                   right = TRUE),
      div(id = ns("one_plate_design"),
          mod_process_plate_design_ui(
            id = ns("plate_design"),
            fileInput_label = "Upload a plate design Excel file:",
            popover_width = "400px",
            popover_title = "Plate design format:",
            popover_content_html = HTML(paste0(
              tags$p(paste(
                "The top-left cell of the Excel sheet should contain",
                "the plate number (e.g. \"Plate 1\"). The cells to the",
                "right of the top-left cell need to be labelled 1-12,", 
                "while the cells below the top-left cell need to be",
                "labelled A-H (for a 96-well plate). The cells",
                "within the plate should contain the sample ID's.",
                "Sample ID's must not contain commas (,) or line breaks."
              )),
              tags$p(paste("At the bottom of the plate, leave one row", 
                           "blank and then add the next plate in the", 
                           "same format.")),
              tags$p("For an example, click on the paperclip icon.")
            ))
          )
      ),
      div(id = ns("two_plate_designs"),
          mod_process_plate_design_ui(
            id = ns("plate_design_specific"),
            fileInput_label = "Upload a plate design Excel file for the specific Ig samples:",
            popover_width = "400px",
            popover_title = "Plate design format:",
            popover_content_html = HTML(paste0(
              tags$p(paste(
                "The top-left cell of the Excel sheet should contain",
                "the plate number (e.g. \"Plate 1\"). The cells to the",
                "right of the top-left cell need to be labelled 1-12,", 
                "while the cells below the top-left cell need to be",
                "labelled A-H (for a 96-well plate). The cells",
                "within the plate should contain the sample ID's.",
                "Sample ID's must not contain commas (,) or line breaks."
              )),
              tags$p(paste("At the bottom of the plate, leave one row", 
                           "blank and then add the next plate in the", 
                           "same format.")),
              tags$p("For an example, click on the paperclip icon.")
            ))
          ),
          mod_process_plate_design_ui(
            id = ns("plate_design_total"),
            fileInput_label = "Upload a plate design Excel file for the total Ig samples:"
          )
      ),
      div(id = ns("sample_list_ui"),
          mod_process_sample_list_ui(
            id = ns("sample_list"),
            fileInput_label = "Upload an Excel file with your sample list:",
            popover_width = "400px",
            popover_title = "Sample list format:",
            popover_content_html = HTML(paste0(
              tags$p(paste(
                "The Excel file should contain only one sheet.",
                "This sheet should contain one column named \"sample_name\"",
                "and one column named \"sample_id\"."
              )),
              tags$p("For an example, click on the paperclip icon.")
            ))
          )
      ),
      fluidRow(
        column(
          width = 12,
          actionButton(ns("button"), 
                       "Add sample ID's to the data")
        ))
      )
  )
}
    
#' add_sample_ids Server Functions
#'
#' @noRd 
mod_add_sample_ids_server <- function(id, keyword_specific, keyword_total, contains_total_and_specific_samples, LacyTools_summary,
                                      lacytools_fileInput, read_lacytools_button){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observe({
      shinyjs::toggle("sample_list_ui", 
                      condition = input$sample_id_method == "Upload a sample list")
      shinyjs::toggle("one_plate_design",
                      condition = all(
                        input$sample_id_method == "Upload a plate design",
                        !isTruthy(input$switch_two_plate_designs)
                      ))
      shinyjs::toggle("two_plate_designs",
                      condition = all(
                        input$sample_id_method == "Upload a plate design",
                        isTruthy(input$switch_two_plate_designs)
                      ))
      shinyjs::toggle("switch_two_plate_designs",
                      condition = all(
                        contains_total_and_specific_samples() == "Yes",
                        input$sample_id_method == "Upload a plate design"
                      ))
    })
    
    r <- reactiveValues(resetter = 0,
                        show_reset_warning = FALSE)
    
    # Whenever a new (correct) LacyTools summary file is uploaded and sample
    # ID's had already been added to the old summary, the resetter counter is
    # increased with 1 and show_reset_warning is set to TRUE:
    observe({
      if (is_truthy(LacyTools_summary()) & is_truthy(data_with_sample_ids())) {
        r$resetter <- isolate(r$resetter) + 1
        r$show_reset_warning <- TRUE
      }
    }) %>% bindEvent(lacytools_fileInput()) # I use the lacytools fileInput 
    # instead of LacyTools_summary(), because LacyTools_summary() also changes when total and 
    # specific keywords are entered/changed.
    
    observe({
      if (r$show_reset_warning == TRUE) {
        showNotification("The sample ID's need to be readded to the data.",
                         type = "warning")
      }
    }) %>% bindEvent(read_lacytools_button())
    
    observe({
      # When sample ID's have been readded to the data (r$show_reset_warning is TRUE and
      # data_with_sample_ids() exists) r$show_reset_warning should be reset to FALSE, so
      # that the warning is not shown again when the 'load lacytools summary'
      # button is clicked but no new lacytools file has been uploaded:
      if (is_truthy(data_with_sample_ids()) & is_truthy(r$show_reset_warning)) {
        r$show_reset_warning <- FALSE
      }
    })
    
    plate_design <- mod_process_plate_design_server(
      id = "plate_design",
      allowed = c("xlsx", "xls"),
      with_info_icon = TRUE,
      reset = r
    )
    
    plate_design_specific <- mod_process_plate_design_server(
      id = "plate_design_specific",
      allowed = c("xlsx", "xls"),
      with_info_icon = TRUE,
      reset = r
    )
    
    plate_design_specific_with_group <- reactive({
      req(plate_design_specific$plate_design(),
          keyword_specific())
      
      plate_design_specific$plate_design() %>% 
        dplyr::mutate(group = keyword_specific())
    })
    
    plate_design_total <- mod_process_plate_design_server(
      id = "plate_design_total",
      allowed = c("xlsx", "xls"),
      with_info_icon = FALSE,
      reset = r
    )
    
    plate_design_total_with_group <- reactive({
      req(plate_design_total$plate_design(),
          keyword_total())
      
      plate_design_total$plate_design() %>% 
        dplyr::mutate(group = keyword_total())
    })
    
    plate_design_combined <- reactive({
      req(plate_design_specific_with_group(),
          plate_design_total_with_group())
      
      dplyr::full_join(plate_design_specific_with_group(),
                       plate_design_total_with_group())
    })
    
    plate_design_filenames <- reactive({
      req(input$sample_id_method == "Upload a plate design")
      if(is_truthy(input$switch_two_plate_designs)) {
        comma_and(c(plate_design_specific$filename(), 
                    plate_design_total$filename()))
      } else {
        plate_design$filename()
      }
    })
    
    sample_list <- mod_process_sample_list_server(
      id = "sample_list",
      allowed = c("xlsx", "xls"),
      reset = r
    )
    
    data_with_sample_ids <- reactive({
      req(LacyTools_summary(),
          read_lacytools_button() > 0) # Showing the plate_well_NAs feedback 
      # before the "Load LacyTools summary" button has been clicked might be
      # confusing for the user.
      
      shinyFeedback::hideFeedback("sample_id_method")
      
      if (input$sample_id_method == "Upload a plate design") {
        summary_with_plate_well <- tryCatch(
          expr = {
            detect_plate_and_well(LacyTools_summary())
          },
          well_precedes_plate = function(c) {
            shinyFeedback::showFeedbackDanger(
              "sample_id_method",
              c$message)
            NULL
          },
          plate_well_NAs = function(c) {
            shinyFeedback::showFeedbackDanger(
              "sample_id_method",
              "Incorrect sample name format, please use the sample list method.")
            NULL
          })
        if (is_truthy(input$switch_two_plate_designs)) {
          req(plate_design_combined(),
              !is.null(summary_with_plate_well))
          with_sample_ids <- dplyr::left_join(summary_with_plate_well,
                                              plate_design_combined())
        } else {
          req(plate_design$plate_design(),
              !is.null(summary_with_plate_well))
          with_sample_ids <- dplyr::left_join(summary_with_plate_well,
                                              plate_design$plate_design())
        }
      } else {
        req(sample_list$sample_list())
        with_sample_ids <- dplyr::left_join(LacyTools_summary(),
                                            sample_list$sample_list())
      }
      
      # TODO: convert code below into a function
      replicates <- with_sample_ids %>% 
        dplyr::select(tidyselect::any_of(c("sample_name", "sample_id", "group"))) %>% 
        dplyr::distinct() %>% 
        dplyr::group_by(dplyr::across(tidyselect::any_of("group"))) %>% 
        dplyr::add_count(sample_id, name = "number_of_replicates") %>% 
        dplyr::mutate(number_of_replicates = ifelse(sample_id == "empty cell in plate design",
                                                    1,
                                                    number_of_replicates)) %>% 
        dplyr::mutate(replicates = ifelse(number_of_replicates > 1, 
                                          TRUE, 
                                          FALSE)) %>% 
        dplyr::ungroup(.)
      
      with_sample_ids <- dplyr::full_join(replicates, with_sample_ids)
      
      return(with_sample_ids)
      
    })
    
    # This observe call ensures that the add_sample_ids actionButton is only
    # enabled under the right circumstances
    observe({
      shinyjs::toggleState("button",
                           condition = is_truthy(data_with_sample_ids()))
    })
    
    output$download_ex_plate_design <- downloadHandler(
      filename = "Example plate design file.xlsx",
      content = function(file) {
        example_file <- system.file("app",
                                    "www",
                                    "Example plate design file.xlsx",
                                    package = "glycodash")
        file.copy(example_file, file)
      }
    )
    
    output$download_ex_sample_list <- downloadHandler(
      filename = "Example sample list file.xlsx",
      content = function(file) {
        example_file <- system.file("app",
                                    "www",
                                    "Example sample list file.xlsx",
                                    package = "glycodash")
        file.copy(example_file, file)
      }
    )
    
    return(list(
      data = data_with_sample_ids,
      button = reactive({ input$button }),
      filenames_plate_design = plate_design_filenames,
      filename_sample_list = sample_list$filename
    ))
    
  })
}

## To be copied in the UI
# mod_add_sample_ids_ui("add_sample_ids_ui_1")
    
## To be copied in the server
# mod_add_sample_ids_server("add_sample_ids_ui_1")
