#' process_sample_list UI Function
#'
#' @description A shiny Module to upload and process a sample list file. This
#' module is used within the module mod_add_sample_ids.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_process_sample_list_ui <- function(id, 
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
    
#' process_sample_list Server Functions
#'
#' @noRd 
mod_process_sample_list_server <- function(id, allowed, reset){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    extension <- reactive({
      req(input$file)
      tools::file_ext(input$file$name)
    })
    
    wrong_extension_warning <- paste("Please upload a",
                                     comma_or(paste0(".", allowed)),
                                     "file.")
    
    observe({
      req(extension())
      shinyFeedback::hideFeedback("file")
      shinyFeedback::feedbackDanger("file",
                                    !(extension() %in% allowed),
                                    text = wrong_extension_warning)
    })
    
    r <- reactiveValues()
    
    observe({
      shinyjs::reset("file")
      r$sample_list <- NULL
    }) %>% bindEvent(reset$resetter > 0)
    
    
    observe({
      req(extension() %in% allowed)
      
      shinyFeedback::hideFeedback("file")
      
      r$sample_list <- tryCatch(
        expr = {
          process_sample_list(input$file$datapath)
        },
        wrong_column_names = function(c) {
          error_message_first_sentence <- stringr::str_replace(c$message,
                                                               "(.+\\.).+",
                                                               "\\1")
          shinyFeedback::feedbackDanger(inputId = "file",
                                        show = TRUE,
                                        text = error_message_first_sentence)
          
          showNotification(
            paste(
              "Please check that your sample list file is formatted correctly.",
              "Click on the information icon to find the required format."
            ),
            type = "error",
            duration = NULL
          )
          
          NULL
          
        })
    })
    
    return(list(
      sample_list = reactive({ r$sample_list }),
      filename = reactive({ input$file$name })
      ))
    
  })
}
    
## To be copied in the UI
# mod_process_sample_list_ui("process_sample_list_ui_1")
    
## To be copied in the server
# mod_process_sample_list_server("process_sample_list_ui_1")
