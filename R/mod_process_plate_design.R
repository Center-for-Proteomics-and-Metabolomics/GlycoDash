#' process_plate_design UI Function
#'
#' @description This Shiny module creates a fileInput with an info icon to the
#'   right of it. This info icon shows a popover on hover.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_process_plate_design_ui <- function(id, 
                                       fileInput_label,
                                       popover_width = "400px", 
                                       popover_title = "", 
                                       popover_content_html = ""){
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
    
#' process_plate_design Server Functions
#'
#' @noRd 
mod_process_plate_design_server <- function(id, allowed, with_info_icon){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observe({
      shinyjs::toggle("info_icon_div",
                      condition = with_info_icon)
    })
    
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
    
    plate_design <- reactive({
      req(input$file)
      
      shinyFeedback::hideFeedback("file")
      
      plate_design <- tryCatch(
        expr = {
          read_and_process_plate_design(input$file$datapath)
        },
        incorrect_formatting = function(c) {
          shinyFeedback::feedbackDanger("file",
                                        show = TRUE,
                                        text = "Incorrect file format.")
          showNotification(
            paste(
              "Please check that your plate design file is formatted correctly.",
              "Click on the information icon to find the required format."
            ),
            type = "error",
            duration = NULL
          )
        },
        plate_numbers = function(c) {
          shinyFeedback::feedbackDanger("file",
                                        show = TRUE,
                                        text = "The plate numbers could not be detected.")
          showNotification(
            paste(
              "Please check that your plate design file is formatted correctly.",
              "Click on the information icon to find the required format."
            ),
            type = "error",
            duration = NULL
          )
        },
        error = function(e) {
          print(e)
        }
      )
    })
    
    return(plate_design)
    
  })
}
    
## To be copied in the UI
# mod_fileInput_with_info_ui("fileInput_with_info_ui_1")
    
## To be copied in the server
# mod_fileInput_with_info_server("fileInput_with_info_ui_1")
