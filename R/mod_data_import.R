#' data_import UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_data_import_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyFeedback::useShinyFeedback(),
    bsplus::use_bs_popover(),
    fluidPage(
      fluidRow(
        h1("Data Import")
      ),
      fluidRow(
        column(
          width = 6,
          mod_read_lacytools_ui(ns("read_lacytools_ui_1")),
          mod_add_sample_ids_ui(ns("add_sample_ids_ui_1")),
          mod_add_sample_types_ui(ns("add_sample_types_ui_1")),
          mod_clusters_ui(ns("clusters_ui_1")),
          mod_add_metadata_ui(ns("add_metadata_ui_1"))
        ),
        column(
          width = 6,
          shinydashboard::box(
            title = "View the converted data",
            width = NULL,
            solidHeader = TRUE,
            status = "primary",
            DT::DTOutput(ns("data_table"))
          )
        )
      )
    )
  )
}
    
#' data_import Server Functions
#'
#' @noRd 
mod_data_import_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Creating a reactiveValues object in which reactiveVals from this module can be saved:
    # (reactiveVals are often easier to work with than reactive expressions for some reason)
    x <- reactiveValues()
    
    
    summary <- mod_read_lacytools_server("read_lacytools_ui_1")
    
    data_incl_sample_ids <- mod_add_sample_ids_server("add_sample_ids_ui_1",
                                                      keyword_specific = summary$keyword_specific,
                                                      keyword_total = summary$keyword_total,
                                                      Ig_data = summary$Ig_data,
                                                      summary = summary$data)
    
    observe({
      print("data_incl_sampled_ids$data()")
      print(is_truthy(data_incl_sample_ids$data()))
      print(req(data_incl_sample_ids$data()))
    })
    
    data_incl_sample_types <- mod_add_sample_types_server("add_sample_types_ui_1",
                                                          summary = data_incl_sample_ids$data)
    
    data_incl_clusters <- mod_clusters_server("clusters_ui_1",
                                              summary = data_incl_sample_types)
    
    data_incl_metadata <- mod_add_metadata_server("add_metadata_ui_1",
                                                  summary = data_incl_clusters)
    
    # When the lacytools summary has been read in, the converted data is shown
    # in the data table
    output$data_table <- DT::renderDT({
      req(summary$data())
      
      if (is_truthy(data_incl_metadata())) {
        show_in_table <- data_incl_metadata()
      } else {
        if (is_truthy(data_incl_clusters())) {
          show_in_table <- data_incl_clusters()
        } else {
          if (is_truthy(data_incl_sample_types$data())) {
            show_in_table <- data_incl_sample_types$data()
            showNotification("The sample types were added to the data",
                             type = "message")
          } else { 
            if (is_truthy(data_incl_sample_ids$data())) {
              show_in_table <- data_incl_sample_ids$data()
              showNotification("The sample ID's were added to the data",
                               type = "message")
            } else {
              show_in_table <- summary$data()
              showNotification("The LacyTools summary has been loaded.",
                               type = "message")
            } 
          }
        }
      }
      
      DT::datatable(show_in_table,
                    options = list(scrollX = TRUE),
                    filter = "top")
    }) %>% bindEvent(summary$button(), 
                     data_incl_sample_ids$button(),
                     data_incl_sample_types$button(),
                     data_incl_sample_types$popup())
    
    to_return <- reactive({
      if (is_truthy(data_incl_metadata())) {
        data_incl_metadata()
      } else {
        if (is_truthy(data_incl_clusters())) {
          data_incl_clusters()
        } else { 
          NULL
        }
      }
    })
    
    return(list(
      summary = to_return,
      Ig_data = summary$Ig_data,
      keyword_specific = summary$keyword_specific,
      keyword_total = summary$keyword_total,
      lacytools_summary = reactive({input$lacytools_summary$name}),
      plate_design = list(reactive({input$plate_design$name}),
                          reactive({input$plate_design_specific$name}),
                          reactive({input$plate_design_total$name})),
      metadata = reactive({input$metadata$name}),
      manual_sample_types = reactive({!x$response}),
      sample_types_file = reactive({input$groups_file$name})
    ))
    
  })
}

## To be copied in the UI
# mod_data_import_ui("data_import_ui_1")

## To be copied in the server
# mod_data_import_server("data_import_ui_1")
