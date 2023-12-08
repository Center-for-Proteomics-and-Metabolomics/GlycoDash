#' process_plate_design UI Function
#'
#' @description This Shiny module lets the user upload and process a plate design
#' file. This module is used within the module mod_add_sample_ids.
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
    
#' process_plate_design Server Functions
#'
#' @noRd 
mod_process_plate_design_server <- function(id, allowed, with_info_icon, reset){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    r <- reactiveValues()
    
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
      shinyFeedback::hideFeedback("file")
      shinyFeedback::feedbackDanger("file",
                                    !(extension() %in% allowed),
                                    text = wrong_extension_warning)
    })
    
    observe({
      if (is_truthy(r$plate_design)) {
        shinyjs::reset("file")
        r$plate_design <- NULL
      }
    }) %>% bindEvent(reset$resetter)
    
    observe({
      req(extension() %in% allowed)
      
      shinyFeedback::hideFeedback("file")
      
      r$plate_design <- tryCatch(
        expr = {
          read_and_process_plate_design(input$file$datapath)
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
          
          NULL
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
          
          NULL
        }
      )
      
    }) %>% bindEvent(input$file$datapath)
    
    
    return(list(
      plate_design = reactive({ r$plate_design }),
      filename = reactive({ input$file$name })
      ))
    
  })
}
