#' process_sample_list UI Function
#'
#' @description A shiny Module.
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
          " .fa {margin-top:28px; color: #3c8dbc;}",
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
mod_process_sample_list_server <- function(id, allowed){
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
      shinyFeedback::feedbackDanger("file",
                                    !(extension() %in% allowed),
                                    text = wrong_extension_warning)
    })
    
    sample_list <- reactive({
      req(input$file)
      
      shinyFeedback::hideFeedback("file")
      
      sample_list <- tryCatch(
        expr = {
          process_sample_list(input$file$datapath)
        },
        wrong_column_names = function(c) {
          shinyFeedback::feedbackDanger("file",
                                        show = TRUE,
                                        text = c$message)
        })  
      
      return(sample_list)
    })
    
    return(sample_list)
    
  })
}
    
## To be copied in the UI
# mod_process_sample_list_ui("process_sample_list_ui_1")
    
## To be copied in the server
# mod_process_sample_list_server("process_sample_list_ui_1")
