#' fileInput_with_info UI Function
#'
#' @description This Shiny module creates a fileInput with an info icon to the
#'   right of it. This info icon shows a popover on hover.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_fileInput_with_info_ui <- function(id, 
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
    
#' fileInput_with_info Server Functions
#'
#' @noRd 
mod_fileInput_with_info_server <- function(id, allowed){
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
    
    return(reactive({input$file}))
    
  })
}
    
## To be copied in the UI
# mod_fileInput_with_info_ui("fileInput_with_info_ui_1")
    
## To be copied in the server
# mod_fileInput_with_info_server("fileInput_with_info_ui_1")
