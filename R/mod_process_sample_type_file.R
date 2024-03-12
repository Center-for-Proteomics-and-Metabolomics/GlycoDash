#' process_sample_type_file UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_process_sample_type_file_ui <- function(id, 
                                            fileInput_label, 
                                            popover_width, 
                                            popover_title, 
                                            popover_content_html){
  ns <- NS(id)
  
  fluidRow(
    column(
      width = 11,
      fileInput(ns("file"), fileInput_label)),
    column(
      width = 1,
      tags$style(
        HTML(paste0(
          "#",
          ns("info_icon_div"),
          " .fas {margin-top:28px; color: #3c8dbc;}",
          " .popover {width: ",
          popover_width,
          "}",
          " .col-sm-1 {padding-left: 0px}"
        ))
      ),
      div(id = ns("info_icon_div"),
          icon("info-circle",
               class = "fa-2x") %>% 
            bsplus::bs_embed_popover(
              title = popover_title,
              content = popover_content_html,
              trigger = "hover",
              placement = "right",
              html = "true",
              container = "body")))
  )
}
    
#' process_sample_type_file Server Functions
#'
#' @noRd 
mod_process_sample_type_file_server <- function(id, allowed){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    sample_type_list <- reactive({
      req(input$file)
      
      shinyFeedback::hideFeedback("file")
      
      sample_types <- tryCatch(
        expr = {
          read_sample_type_file(input$file$datapath,
                                input$file$name)
        },
        wrong_extension = function(c) {
          shinyFeedback::feedbackDanger("file",
                                        show = TRUE,
                                        text = c$message)
          NULL
        },
        missing_columns = function(c) {
          error_message_first_sentence <- stringr::str_replace(c$message,
                                                               "(.+\\.).+",
                                                               "\\1")
          
          shinyFeedback::feedbackDanger("file",
                                        show = TRUE,
                                        text = error_message_first_sentence)
          
          showNotification(
            paste(
              "Please check that your sample types file is formatted correctly.",
              "Click on the information icon to find the required format."
            ),
            type = "error",
            duration = NULL
          )
          
          NULL
          
        })
      
      return(sample_types)
    }) %>% bindEvent(input$file)
    
    
    return(list(
      list = sample_type_list,
      filename = reactive({ input$file$name })
      ))
    
  })
}
    
## To be copied in the UI
# mod_process_sample_type_file_ui("process_sample_type_file_ui_1")
    
## To be copied in the server
# mod_process_sample_type_file_server("process_sample_type_file_ui_1")
