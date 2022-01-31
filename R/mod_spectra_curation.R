#' spectra_curation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_spectra_curation_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        h1("Spectra curation") 
      ),
      fluidRow(
        column(
          width = 6,
          shinydashboard::box(
            title = "Spectra curation",
            width = NULL,
            solidHeader = TRUE,
            status = "primary",
            numericInput(ns("n_clusters"), 
                         "How many clusters does your data contain?",
                         value = 1),
            uiOutput(ns("clusters")),
            shinydashboard::box(
              title = "Quality criteria",
              width = NULL,
              status = "primary",
              solidHeader = TRUE,
              sliderInput(ns("mass_accuracy"), 
                          "Acceptable mass accuracy range:",
                          min = -50,
                          max = 50,
                          value = c(-20, 20)
              ),
              numericInput(ns("ipq"),
                           "Max. IPQ value:",
                           value = 0.2,
                           step = 0.1),
              numericInput(ns("sn"),
                           "Min. S/N ratio:",
                           value = 9)
            ),
            selectInput(ns("cut_off_basis"),
                        "Base the spectra curation cut-off on:",
                        choices = c("Negative controls; specific",
                                    "Blanks")
            ),
            actionButton(ns("curate_spectra"),
                         "Perform spectra curation")
          ),
          shinydashboard::box(
            title = "Export results",
            width = NULL,
            solidHeader = TRUE,
            status = "primary",
            radioButtons(ns("download_format"),
                         "Choose a file format:",
                         choices = c("Excel file", "R object")),
            downloadButton(ns("download"), 
                         "Download curated spectra")
          )
        ),
        column(
          width = 6,
          shinydashboard::box(
            title = "Information on spectra curation",
            width = NULL,
            solidHeader = TRUE,
            status = "primary",
            plotOutput(ns("curated_spectra_plot")),
            tableOutput(ns("fail_table"))
          )
        )
      )
    )
  )
}
    
#' spectra_curation Server Functions
#'
#' @noRd 
mod_spectra_curation_server <- function(id, results_data_import){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Creating a reactiveValues object in which reactiveVals from this module
    # can be saved: (reactiveVals are often easier to work with than reactive
    # expressions for some reason)
    x <- reactiveValues()
    
    # If data_incl_metadata exists it is assigned to x$data, otherwise
    # data_incl_plate_design is assigned to x$data. x$data (not the reactives
    # from the previous module) will be used from this point in the module.
    observe({
      if (isTruthy(results_data_import$data_incl_metadata())){
        x$data <- results_data_import$data_incl_metadata()
      } else { if (isTruthy(results_data_import$data_incl_plate_design())){
        x$data <- results_data_import$data_incl_plate_design()
      } 
      }
      print(x$data)
    })
    
    # Create inputIds for the cluster textInputs based on the 
    # value of input$n_clusters:
    cluster_inputIds <- reactive({
      req(input$n_clusters)
      cluster_inputIds <- purrr::map(seq_len(input$n_clusters),
                                       ~ paste0("cluster", .x))
      return(cluster_inputIds)
    })
    
    # Create textInputs for the clusters. The number of inputs created is the
    # same as the value of input$n_clusters.
    output$clusters <- renderUI({
      req(cluster_inputIds())
        purrr::imap(cluster_inputIds(),
                    function(inputId, i) textInput(
                      ns(inputId),
                      label = paste("By what words/letters in the analyte name can the analytes belonging to cluster",
                                    i,
                                    "be recognized?")
                      ))
    })
    
    # The selection menu for input$cut_off_basis is updated so that the choices
    # are all combinations of sample_types and groups that are present in the
    # data.
    observeEvent(x$data, {
      combinations <- expand.grid(sample_type = unique(x$data$sample_type),
                             group = unique(x$data$group))
      combination_strings <- purrr::pmap_chr(combinations,
                                        function(sample_type, group) {
                                          paste0(sample_type,
                                                 "; ",
                                                 group)
                                        })
      options <- c(combination_strings, 
                   unique(x$data$sample_type),
                   unique(x$data$group))
      updateSelectInput(inputId = "cut_off_basis",
                        choices = options)
    })
    
    observeEvent(input$curate_spectra, {
      # Get the values of the cluster textInputs:
      clusters_regex <- purrr::map(cluster_inputIds(),
                                  ~ input[[.x]])
      
      # Extract the group out of input$cut_off_basis that was selected by the
      # user:
      group_to_filter <- stringr::str_extract(string = input$cut_off_basis,
                                              pattern = paste0(unique(x$data$group),
                                                               collapse = "|"))
      
      # Extract the sample_type out of input$cut_off_basis that was selected by
      # the user as cut_off_basis:
      sample_type_to_filter <- stringr::str_extract(string = input$cut_off_basis,
                                                    pattern = paste0(
                                                      unique(x$data$sample_type),
                                                      collapse = "|"))
      
      # Perform spectra curation:
      x$data_spectra_curated <- curate_spectra(
        data = x$data,
        clusters_regex = clusters_regex,
        min_ppm_deviation = input$mass_accuracy[1],
        max_ppm_deviation = input$mass_accuracy[2],
        max_ipq = input$ipq,
        min_sn = input$sn,
        group_to_filter = group_to_filter,
        sample_type_to_filter = sample_type_to_filter)
      
      # Filter out all spectra that didn't pass curation:
      x$curated_spectra <- x$data_spectra_curated %>% 
        dplyr::filter(passed_curation == TRUE)
      
    })
    
    output$curated_spectra_plot <- renderPlot({
      req(x$data_spectra_curated)
      # Move this code to a function instead?
      x$data_spectra_curated %>%  
        ggplot2::ggplot() +
        ggplot2::geom_bar(ggplot2::aes(x = sample_type, fill = passed_curation), 
                          position = "fill") +
        ggplot2::facet_wrap(cluster ~ group) +
        ggplot2::xlab("Sample type") +
        ggplot2::scale_y_continuous(labels = function(x) paste0(x * 100, "%"), 
                                    name = "Proportion of spectra (%)") +
        ggplot2::scale_fill_discrete(name = "Passed curation?", 
                                     labels = c(`TRUE`= "Yes",
                                                `FALSE` = "No")) +
        ggplot2::theme_classic() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                       strip.background = ggplot2::element_rect(fill = "#F6F6F8")) +
        ggpubr::border(size = 0.5)
      })
    
    output$fail_table <- renderTable({
      # create an actual table here:
      shinipsum::random_table(3, 3)
      })
    
    # data_to_download <- reactive({
    #   req(x$data_spectra_curated)
    #   x$data_spectra_curated
    # })
    
    output$download <- downloadHandler(
      filename = function() {
        todays_date <- paste0(stringr::str_replace_all(Sys.Date(),
                                                       pattern = "-",
                                                       replacement = ""))
        switch(input$download_format,
               "R object" = paste0(todays_date, "_curated_spectra.rds"),
               "Excel file" = paste0(todays_date, "_curated_spectra.xlsx"))
      },
      content = function(file) {
        data_to_download <- x$data_spectra_curated
        switch(input$download_format,
               "R object" = save(data_to_download, 
                                 file = file),
               "Excel file" = writexl::write_xlsx(data_to_download, 
                                                  path = file))
      }
    )
  })
}
    
## To be copied in the UI
# mod_spectra_curation_ui("spectra_curation_ui_1")
    
## To be copied in the server
# mod_spectra_curation_server("spectra_curation_ui_1")
