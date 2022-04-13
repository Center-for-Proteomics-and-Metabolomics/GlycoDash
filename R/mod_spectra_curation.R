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
    bsplus::use_bs_popover(),
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
                         value = 1,
                         min = 1,
                         max = 25,
                         step = 1),
            uiOutput(ns("clusters")),
            div(
              id = ns("qc"),
              shinydashboard::box(
                tags$style(
                  HTML(paste0("#",
                              ns("qc"),
                              " .fa {float: right; margin-top: 3px}",
                              "#",
                              ns("qc"),
                              " .box-title {width: 100%}"))
                ),
                title = span(
                  "Analyte quality criteria",
                  icon("info-circle",
                       class = "ml",
                       tabindex = "0") %>% 
                    bsplus::bs_embed_popover(
                      title = "Explanation",
                      content = HTML(paste0(
                        tags$p(paste(
                          "For each spectrum, analytes are curated based on",
                          "the chosen criteria for the mass accuracy, the",
                          "isotopic pattern quality (IPQ) and the signal-to-noise",
                          "ratio (S/N)."
                        )),
                        tags$p(paste(
                          "Next, the proportion of passing analytes and the sum intensity",
                          "of the passing analytes within each spectrum are calculated."
                        )),
                        tags$p(paste(
                          "This proportion and this sum intensity are then compared",
                          "to the spectra curation cut-off values (see below) to decide",
                          "whether a spectrum passes spectra curation."
                        ))
                      )),
                      trigger = "focus",
                      placement = "right",
                      html = "true",
                      container = "body")
                ),
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
              )
            ),
            tags$style(HTML(paste0("#",
                                   ns("popover_cut_off"),
                                   " .popover{width: 400px !important;}"))),
            div(
              id = ns("popover_cut_off"),
              selectizeInput(ns("cut_off_basis"),
                             "Base the spectra curation cut-off on:",
                             choices = c(""),
                             selected = NULL,
                             multiple = TRUE,
                             options = list(placeholder = "select which samples to use as a basis for cut-off")
              ) %>% 
                bsplus::bs_embed_popover(
                  title = "Explanation",
                  content = HTML(paste0(
                    tags$p(paste(
                      "Choose a group of samples that should not pass curation",
                      "(e.g. Specific Ig negative control samples)."
                    )),
                    tags$p(paste(
                      "The average proportion of passing analytes and average sum intensity",
                      "in this group of samples will be used as cut-off values;"
                    )),
                    tags$p(paste(
                      "All spectra that have a proportion of passing analytes and a sum intensity",
                      "higher than these cut-off values will pass spectra curation."
                    ))
                  )),
                  trigger = "hover",
                  placement = "right",
                  html = "true")
            ),
            actionButton(ns("curate_spectra"),
                         "Perform spectra curation")),
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
            br(),
            plotOutput(ns("cut_off_plot"),
                       dblclick = ns("dblclick"),
                       brush = brushOpts(
                         id = ns("brush"),
                         resetOnNew = TRUE
                       )),
            br(),
            "Select and double click a plot area to zoom in."
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
    })
    
    # This observe call ensures that the curate_spectra actionButton is only
    # enabled under the right circumstances
    observe({
      shinyjs::disable(id = "curate_spectra")
      if (all(isTruthy(x$data),
              isTruthy(input$mass_accuracy),
              isTruthy(input$ipq),
              isTruthy(input$sn),
              isTruthy(input$cut_off_basis))) {
        if (all(purrr::map_lgl(cluster_inputIds(),
                               ~ isTruthy(input[[.x]])),
                x$clusters_OK,
                x$clusters_no_overlap)) {
          shinyjs::enable("curate_spectra")
        }
      }
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
                      label = paste("By what word/letters within the analyte name can the analytes belonging to cluster",
                                    i,
                                    "be recognized?")
                      ))
    })
    
    # Check whether the values given as textInputs for the clusters have matches
    # with the analytes in the data:
    observeEvent({purrr::map(cluster_inputIds(),
                            ~ input[[.x]])}, {
      req(x$data)
      x$clusters_OK <- TRUE
      purrr::map(cluster_inputIds(),
                 function(cluster_inputId) {
                   shinyFeedback::hideFeedback(cluster_inputId)
                   req(input[[cluster_inputId]] != "")
                   tryCatch(define_clusters(data = x$data,
                                            clusters_regex = input[[cluster_inputId]]),
                            unmatched_regex = function(c) {
                              shinyFeedback::feedbackDanger(cluster_inputId,
                                                            show = TRUE,
                                                            text = c$message)
                              x$clusters_OK <- FALSE
                            },
                            unmatched_analytes = function(c){ #ignore this error
                              })
                   })
    
      clusters_regex <- purrr::map(cluster_inputIds(),
                                   ~ input[[.x]])
      x$clusters_no_overlap <- TRUE
      
      if (length(clusters_regex) > 1) {
        regex_overlap <- purrr::imap_lgl(
          clusters_regex,
          function(regex, i) {
            other_regexes <- unlist(clusters_regex)[-i]
            any(purrr::map_lgl(other_regexes,
                               function(other_regex) {
                                 stringr::str_detect(string = other_regex,
                                                     pattern = stringr::fixed(regex))
                               }))
          })
        
        if(any(regex_overlap == TRUE)) {
          purrr::map(cluster_inputIds(),
                     ~ shinyFeedback::feedbackDanger(.x,
                                                     show = any(regex_overlap == TRUE),
                                                     text = paste("Overlap between the cluster keywords is not allowed,",
                                                                  "as each analyte should match only one cluster keyword.")))
          x$clusters_no_overlap <- FALSE
        }
        
      }
    })
    
    # The selection menu for input$cut_off_basis is updated so that the choices
    # are all combinations of sample_types and groups that are present in the
    # data.
    observeEvent(x$data, {
      if ("group" %in% colnames(x$data)) {
        combinations <- expand.grid(sample_type = unique(x$data$sample_type),
                                    group = unique(x$data$group))
        options <- purrr::pmap_chr(combinations,
                                   function(sample_type, group) {
                                     paste(group,
                                           sample_type,
                                           "samples")
                                   })
      } else {
        options <- paste("all", unique(x$data$sample_type), "samples")
      }
      
      updateSelectizeInput(inputId = "cut_off_basis",
                           choices = c("", options))
    })
    
    observeEvent(input$curate_spectra, {
      # Get the values of the cluster textInputs:
      clusters_regex <- purrr::map(cluster_inputIds(),
                                  ~ input[[.x]])
      
      # Perform spectra curation:
      tryCatch(expr = { 
        spectra_curation_results <- curate_spectra(data = x$data,
                                                   clusters_regex = clusters_regex,
                                                   min_ppm_deviation = input$mass_accuracy[1],
                                                   max_ppm_deviation = input$mass_accuracy[2],
                                                   max_ipq = input$ipq,
                                                   min_sn = input$sn,
                                                   cut_off_basis = input$cut_off_basis)
        
        x$data_spectra_curated <- spectra_curation_results$curated_data
        
        x$spectra_check <- spectra_curation_results$spectra_check
        
        showNotification("Spectra curation has been performed.",
                         type = "message")
        
              },
      regex_overlap = function(c) {
        showNotification(ui = paste(c$message), 
                         type = "error")
      },
      unmatched_analytes = function(c) {
        showNotification(ui = paste(c$message), 
                         type = "error")
      })
      
    })
    
    observeEvent(x$data_spectra_curated, {
      # Filter out all spectra that didn't pass curation:
      x$curated_spectra <- x$data_spectra_curated %>% 
        dplyr::filter(passed_spectra_curation == TRUE) %>% 
        dplyr::select(-passed_spectra_curation)
      
    })
    
    curated_spectra_plot <- reactive({
      req(x$curated_spectra)
      # Move this code to a function instead?
      plot <- x$data_spectra_curated %>%  
        ggplot2::ggplot() +
        ggplot2::geom_bar(ggplot2::aes(x = sample_type, 
                                       fill = passed_spectra_curation), 
                          position = "fill") +
        ggplot2::xlab("Sample type") +
        ggplot2::scale_y_continuous(labels = function(x) paste0(x * 100, "%"), 
                                    name = "Proportion of spectra (%)") +
        ggplot2::scale_fill_discrete(name = "Passed curation?", 
                                     labels = c(`TRUE`= "Yes",
                                                `FALSE` = "No"),
                                     type = c(`TRUE` = "#3498DB",
                                              `FALSE` = "#E74C3C")) +
        ggplot2::theme_classic() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                       strip.background = ggplot2::element_rect(fill = "#F6F6F8"),
                       text = ggplot2::element_text(size = 16),
                       panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=0.5))
      
      if ("group" %in% colnames(x$data_spectra_curated)) {
        plot +
          ggplot2::facet_wrap(cluster ~ group)
      } else {
        plot +
          ggplot2::facet_wrap(~ cluster)
      }
    })
    
    # observe({
    #   req(curated_spectra_plot())
    #   print(curated_spectra_plot())
    # })
    
    output$curated_spectra_plot <- renderPlot({
      req(curated_spectra_plot())
      curated_spectra_plot()
    })
    
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
        data_to_download <- x$curated_spectra
        switch(input$download_format,
               "R object" = save(data_to_download, 
                                 file = file),
               "Excel file" = writexl::write_xlsx(data_to_download, 
                                                  path = file))
      }
    )
    
    ranges <- reactiveValues(x = NULL,
                             y = NULL)

    cut_off_plot <- reactive({
      req(x$spectra_check,
          input$cut_off_basis)
      create_cut_off_plot(spectra_check = x$spectra_check,
                          cut_off_basis = input$cut_off_basis)
    })

    output$cut_off_plot <- renderPlot({
      req(cut_off_plot())
      cut_off_plot() +
        ggplot2::coord_cartesian(xlim = ranges$x,
                                 ylim = ranges$y,
                                 expand = FALSE)
    })

    # When a double-click happens, check if there's a brush on the plot.
    # If so, zoom to the brush bounds; if not, reset the zoom.
    observeEvent(input$dblclick, {
      brush <- input$brush
      if (!is.null(brush)) {
        ranges$x <- c(brush$xmin, brush$xmax)
        ranges$y <- c(brush$ymin, brush$ymax)

      } else {
        ranges$x <- NULL
        ranges$y <- NULL
      }
    })
    
    return(list(
      curated_spectra = reactive({x$curated_spectra}),
      mass_acc = reactive({input$mass_accuracy}),
      ipq = reactive({input$ipq}),
      sn = reactive({input$sn}),
      cut_off = reactive({input$cut_off_basis}),
      plot = reactive({curated_spectra_plot()})
    ))
    
  })
}
    
## To be copied in the UI
# mod_spectra_curation_ui("spectra_curation_ui_1")
    
## To be copied in the server
# mod_spectra_curation_server("spectra_curation_ui_1")
