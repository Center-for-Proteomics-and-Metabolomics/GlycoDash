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
          width = 4,
          div(
            id = ns("qc"),
            tags$style(HTML(paste0(
              "#", ns("box_header2"), " .awesome-checkbox {padding-top: 7px}",
              "#", ns("box_header2"), " .popover {max-width: 400px !important; color: #333}",
              "#", ns("qc"), " .box-title {width: 100%}",
              "#", ns("box_header2"), " .fas {float: right; margin-right: 5px; font-size: 18px}",
              "#", ns("box_header2"), " .direct-chat-contacts {right: 0; background: #222d32!important}",
              "#", ns("box_header2"), " .btn {float: right; border-width: 0px; margin-right: 2px}",
              "#", ns("qc"), " .dropdown {display: inline-block; float: right;}",
              "#", ns("box_header2"), " .dropdown-menu {background: #333; right: -33px; left: auto; top: 28px;}"
            ))),
            shinydashboard::box(
              title = div(
                "Choose analyte quality criteria",
                id = ns("box_header2"),
                icon("info-circle",
                     class = "ml",
                     #tabindex = "0" #only needed for trigger = "focus"
                ) %>% 
                  bsplus::bs_embed_popover(
                    title = "Explanation",
                    content = HTML(
                      "
                      Here you can select analyte quality criteria that will be used for 
                      spectra curation. These values will also be used in the \"Analyte Curation\"
                      tab. For an analyte to pass in a spectrum, it has to fulfill all three
                      quality criteria.
                      <br> <br>
                      For each spectrum, the intensities of all passing analytes are summed.
                      The percentage of passing analytes is also calculated per spectrum. 
                      These values are shown in the interactive scatter plots. 
                      Spectra curation is performed based on the sum intensity of passing analytes 
                      and on the percentage of passing analytes in the spectra.
                      <br> <br>
                      It is possible to exclude one or two quality criteria from this assessment,
                      by clicking the gears icon.
                      "
                    ),
                    trigger = "hover",
                    placement = "right",
                    html = "true",
                    container = "body"),
                shinyWidgets::dropdownButton(
                  shinyWidgets::awesomeCheckboxGroup(
                    ns("qc_to_include"),
                    "Which analyte quality criteria should be taken into account during spectra curation?",
                    # Choices determined in server based on data type
                    choices = c(""), selected = c(""), status = "primary"),
                  icon = icon("gears",
                              class = "ml"),
                  tooltip = shinyWidgets::tooltipOptions(placement = "top",
                                                         title = "Advanced settings"),
                  width = "250px",
                  size = "xs"
                )),
              status = "primary",
              solidHeader = TRUE,
              width = NULL,
              sliderInput(ns("mass_accuracy"), 
                          "Acceptable mass accuracy range (ppm):",
                          min = -50,
                          max = 50,
                          value = c(-20, 20)
              ),
              # IPQ and S/N in case of LaCyTools
              numericInput(ns("ipq"),
                           "Max. isotopic pattern quality (IPQ) value:",
                           value = 0.2, step = 0.1, min = 0.0),
              numericInput(ns("sn"),
                           "Min. signal-to-noise (S/N) ratio:",
                           value = 9, min = 0.0),
              # IDP and Total area in case of Skyline
              numericInput(ns("idp"),
                           "Min. isotope dot product (IDP) value:",
                           val = 0.9, max = 1.0, step = 0.1),
              numericInput(ns("total_area"), "Min. total area", val = 0)
            )
          )
        ),
        column(
          width = 8,
          tags$style(HTML(paste0("#",
                                 ns("popover_cut_off"),
                                 " .popover{width: 400px !important;}"))),
          div(
            id = ns("popover_cut_off"),
            tags$style(HTML(paste0(
              "#", ns("box_header"), " .awesome-checkbox {padding-top: 7px}",
              "#", ns("box_header"), " .popover {max-width: 400px !important; color: #333}",
              "#", ns("popover_cut_off"), " .box-title {width: 100%}",
              "#", ns("box_header"), " .fas {float: right; margin-right: 5px; font-size: 18px}",
              "#", ns("box_header"), " .direct-chat-contacts {right: 0; background: #222d32!important}",
              "#", ns("box_header"), " .btn {float: right; border-width: 0px; margin-right: 0px}",
              "#", ns("popover_cut_off"), " .dropdown {display: inline-block; float: right; width: 330px}",
              "#", ns("box_header"), " .dropdown-menu {background: #333; right: -10px; left: auto; top: 28px;}"
            ))),
            shinydashboard::box(
              title = div("Calculate spectra curation cut-offs",
                          id = ns("box_header"),
                          icon("info-circle",
                               class = "ml",
                               #tabindex = "0" #only needed for trigger = "focus"
                          ) %>% 
                            bsplus::bs_embed_popover(
                              title = "Spectra curation methods",
                              content = HTML(
                                "
                                <b> Negative control spectra </b>
                                <br>
                                Choose a group of negative controls that should not pass
                                spectra curation. The curation cut-offs will be set at a chosen
                                percentile of the sum intensities and passing analyte percentages
                                in the negative control spectra.
                                <br> <br> 
                                <b> Percentiles </b>
                                <br>
                                The cut-offs will be set at a chosen percentile of the sum intensities
                                and passing analyte percentages in all spectra, except for those belonging
                                to sample types that you choose to exclude from the assessment. For example,
                                when the chosen percentile is 5, then the lowest 5% of all spectra will fail
                                curation.
                                "
                              ),
                              trigger = "hover",
                              placement = "left",
                              html = "true",
                              container = "body")
                          ),
              width = NULL,
              status = "primary",
              solidHeader = TRUE,
              fluidRow(
                column(
                  width = 12,
                  tags$p(paste(
                    "Each spectrum will be curated based on its sum intensity",
                    "and its percentage of passing analytes. Cut-off values",
                    "are calculated for both of these parameters.",
                    "The way this calculation is performed depends on the chosen",
                    "spectra curation method:"
                  )),
                  shinyWidgets::awesomeRadio(ns("curation_method"),
                                             "Curate spectra based on:",
                                             choices = c("Negative control spectra",
                                                         "Percentiles",
                                                         "Skip spectra curation"),
                                             selected = "Negative control spectra"),
                  # shinyjs::toggle can't be used to directly show/hide a module, 
                  # so I put the modules inside divs:
                  div(
                    id = ns("controls_module"), 
                    mod_curate_based_on_controls_ui(ns("curate_based_on_controls_ui_1"))
                  ),
                  div(
                    id = ns("percentiles_module"),
                    mod_curate_based_on_percentiles_ui(ns("curate_based_on_percentiles_ui_1"))
                  ),
                  shinyWidgets::awesomeCheckbox(
                    ns("uncalibrated_as_na"),
                    label = "Treat uncalibrated spectra as missing values, not zeros.",
                    value = TRUE
                  )
                )
              )
            )
            )
          )
        ),
      fluidRow(
        column(
          width = 12,
          shinydashboard::box(
            title = "Perform spectra curation",
            width = NULL,
            solidHeader = TRUE,
            status = "primary",
            tabsetPanel(id = ns("tabs")),
            br(),
            actionButton(ns("button"),
                         "Perform spectra curation",
                         style = "font-size: 16px; padding: 10px 20px; font-weight: bold; 
                                 border: 1px solid black;")
          )
        ) 
      ),
      fluidRow(
        column(
          width = 12,
          shinydashboardPlus::box(
            id = ns("results_box"),
            title = "View spectra curation results",
            width = NULL,
            solidHeader = TRUE,
            status = "primary",
            tabsetPanel(id = ns("plots_curation_results")),
            br(),
            tabsetPanel(
              id = ns("result_tables"),
              tabPanel(title = "Details of passing spectra per analyte",
                       column(width = 12,
                              br(),
                              DT::dataTableOutput(ns("passing_spectra_details")))),
              tabPanel(title = "Overview of failed spectra",
                       column(width = 12,
                              br(),
                              DT::dataTableOutput(ns("failed_spectra_table")))),
              tabPanel(title = "Details of failed spectra per analyte",
                       column(width = 12,
                              br(),
                              DT::dataTableOutput(ns("failed_spectra_details"))))
            )
          )
        )
      ),
      fluidRow(
        column(
          width = 6,
          shinydashboard::box(
            title = "Export results",
            width = NULL,
            solidHeader = TRUE,
            status = "primary",
            radioButtons(ns("download_format"),
                         "Choose a file format:",
                         choices = c("Excel file", "R object")),
            downloadButton(ns("download1"), 
                           "Details of passing spectra per analyte",
                           style = "width: 330px;"),
            br(),
            br(),
            downloadButton(ns("download2"),
                           "Overview of failed spectra",
                           style = "Width: 330px;"),
            br(),
            br(),
            downloadButton(ns("download3"),
                           "Details of failed spectra per analyte",
                           style = "Width: 330px;")
          )
        )
      )
    )
  )
}



#' spectra_curation Server Functions
#'
#' @noRd 
mod_spectra_curation_server <- function(id, results_data_import) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Update user interface based on data type (LaCyTools or Skyline)
    observeEvent(results_data_import$data_type(), {
      if (results_data_import$data_type() == "Skyline data") {
        # Checkboxes to include QC
        shinyWidgets::updateAwesomeCheckboxGroup(
          inputId = "qc_to_include",
          choices = c("Mass accuracy", "Isotope dot product", "Total area"),
          selected = c("Mass accuracy", "Isotope dot product", "Total area")
        )
        # Show IDP and Total area options
        shinyjs::hide("ipq")
        shinyjs::hide("sn")
        shinyjs::show("idp")
        shinyjs::show("total_area")
      } else if (results_data_import$data_type() == "LaCyTools data") {
        # Checkboxes to include QC
        shinyWidgets::updateAwesomeCheckboxGroup(
          inputId = "qc_to_include",
          choices = c("Mass accuracy", "Isotopic pattern quality", "S/N"),
          selected = c("Mass accuracy", "Isotopic pattern quality", "S/N")
        )
        # Show IPQ and SN
        shinyjs::show("ipq")
        shinyjs::show("sn")
        shinyjs::hide("idp")
        shinyjs::hide("total_area")
      }
    })
    
    # If quantitation is done: exlude quantitation clusters except IgG1 glycopeptides
    data_to_check <- reactive({
      req(results_data_import$LaCyTools_summary())
      if (is_truthy(results_data_import$quantitation_clusters())) {
        clusters <- results_data_import$quantitation_clusters()
        exclude <- clusters[setdiff(names(clusters), "IgG1_cluster_glyco")]
        to_return <- results_data_import$LaCyTools_summary() %>% 
          dplyr::filter(!cluster %in% exclude)
        return(to_return)
      } else {
        results_data_import$LaCyTools_summary()
      }
    })
    
    
    # Check if the data contains total and specific samples
    total_and_specific <- reactive({
      req(data_to_check())
      if (results_data_import$contains_total_and_specific_samples() & 
          "group" %in% colnames(data_to_check())) {
        TRUE
      } else {
        FALSE
      }
    })
    
    
    # Data with criteria checks for each analyte in each sample.
    checked_data <- reactive({
      req(data_to_check(), length(input$qc_to_include) > 0)
      
      r$tab_contents <- NULL # Reset the tab contents so that 
      # cut_offs_to_use_all_clusters() becomes invalid and the button is disabled.
      
      if (results_data_import$data_type() == "LaCyTools data") {
        req(input$sn, input$ipq)
        # Check analyte quality criteria for LaCyTools data
        check_analyte_quality_criteria_lacytools(
          my_data = data_to_check(),
          min_ppm_deviation = input$mass_accuracy[1],
          max_ppm_deviation = input$mass_accuracy[2],
          max_ipq = input$ipq,
          min_sn = input$sn,
          criteria_to_consider = input$qc_to_include
        )
      } else if (results_data_import$data_type() == "Skyline data") {
        req(input$total_area, input$idp)
        # Check analyte quality criteria for Skyline data
        check_analyte_quality_criteria_skyline(
          my_data = data_to_check(),
          min_ppm_deviation = input$mass_accuracy[1],
          max_ppm_deviation = input$mass_accuracy[2],
          min_idp = input$idp,
          min_total_area = input$total_area,
          criteria_to_consider = input$qc_to_include
        )
      }
    })
    
    
    
    # Analyte quality criteria checks summarized per cluster per sample: 
    summarized_checks <- reactive({
      req(checked_data())
      summarize_spectra_checks(checked_data(), results_data_import$data_type())
    })
    
    observe({

      shinyjs::toggle("controls_module",
                      condition = input$curation_method == "Negative control spectra")
      
      shinyjs::toggle("percentiles_module",
                      condition = input$curation_method == "Percentiles")
      
      shinyjs::toggle("button",
                      condition = input$curation_method != "Skip spectra curation")
      
      shinyjs::toggle("results_box",
                      condition = input$curation_method != "Skip spectra curation")
      
      shinyjs::toggle("uncalibrated_as_na",
                      condition = input$curation_method != "Skip spectra curation")
      
    })
    
    cut_offs_based_on_controls <- mod_curate_based_on_controls_server(
      "curate_based_on_controls_ui_1",
      results_data_import = results_data_import,
      total_and_specific = total_and_specific,
      summarized_checks = summarized_checks,
      uncalibrated_as_NA = reactive({ input$uncalibrated_as_na })  
    )
    
    cut_offs_based_on_percentiles <- mod_curate_based_on_percentiles_server(
      "curate_based_on_percentiles_ui_1",
      summarized_checks = summarized_checks,
      uncalibrated_as_NA = reactive({ input$uncalibrated_as_na })
    )
    
    calculated_cut_offs <- reactive({
      if (input$curation_method == "Negative control spectra") {
        req(cut_offs_based_on_controls()) 
      } else if (input$curation_method == "Percentiles") {
        req(cut_offs_based_on_percentiles()) 
      } else if (input$curation_method == "Skip spectra curation") {
        NULL
      }
    })
    
    clusters <- reactive({
      req(data_to_check())
      unique(data_to_check()$cluster)
    })
    
    created_tabs <- reactiveValues(clusters = c(""))
    
    observeEvent(clusters(), {
      # Remove tabs in case they have been created before. 
      purrr::map(created_tabs$clusters,
                 function(cluster) {
                   removeTab("tabs",
                             target = cluster)
                 })
      
      # Update created_cluster_tabs with new clusters
      created_tabs$clusters <- clusters()
      
      # Create one tab for each cluster.
      purrr::map(clusters(),
                 function(cluster) {
                   appendTab("tabs",
                             select = TRUE,
                             tabPanel(
                               title = cluster,
                               mod_tab_cut_offs_ui(ns(cluster))
                             ))
                 })
    })
    
    
    
    r <- reactiveValues()
    
    observe({
      req(clusters(), summarized_checks())
      # Generate color palette
      sample_types <- unique(summarized_checks()$sample_type)
      colors <- color_palette(length(sample_types))
      color_palette <- setNames(colors, sample_types)
      # Generate tabs with plots
      r$tab_contents <- rlang::set_names(clusters()) %>% 
        purrr::map(
          .,
          function(current_cluster) {
            mod_tab_cut_offs_server(
              id = current_cluster,
              selected_cluster = current_cluster,
              summarized_checks = reactive({
                summarized_checks() %>%
                  dplyr::filter(cluster == current_cluster)
              }),
              color_palette = color_palette,
              contains_total_and_specific_samples = total_and_specific,
              keyword_specific = results_data_import$keyword_specific,
              keyword_total = results_data_import$keyword_total,
              calculated_cut_offs = reactive({ 
                if (is.null(calculated_cut_offs())) { # When spectra curation is skipped.
                  NULL
                } else {
                  calculated_cut_offs() %>% 
                    dplyr::filter(cluster == current_cluster)
                }       
              }),
              curation_method = reactive({ input$curation_method })
            )
          })
    })
    
    
    cut_offs_to_use_all_clusters <- reactive({
      purrr::map_dfr(r$tab_contents,
                     function(tab) {
                       # Use try_call not do.call, because if
                       # input$curation_method == "Skip spectra curation", then
                       # tab_contents$cut_offs_to_use doesn't exist
                        try_call(tab[["cut_offs_to_use"]])
                     })
    })
  
    
    # Check if there are clusters for which all negative controls were uncalibrated
    missing_cluster_cut_offs <- reactive({
      if (!rlang::is_empty(cut_offs_to_use_all_clusters())) {
        # Check if data contains total and specific samples
        if ("group" %in% colnames(cut_offs_to_use_all_clusters())) {
          to_compare <- rep(clusters(), 2)
        } else {
          to_compare <- clusters()
        }
        # Check if there are cut-offs missing for clusters
        to_check <- cut_offs_to_use_all_clusters()$cluster
        identical <- identical(
          # Need to order elements in the character vectors to compare
          to_check[stringr::str_order(to_check)],
          to_compare[stringr::str_order(to_compare)]
        )
        if (identical) {
          return(FALSE)
        } else {
          return(TRUE)
        }
      } else {
        return(FALSE)
      }
    })
    

    # Enable or disable button based on missing cut-offs
    observe({
      if (!rlang::is_empty(cut_offs_to_use_all_clusters())) {
        if (missing_cluster_cut_offs() == TRUE) {
          shinyjs::disable("button")
        } else {
          shinyjs::enable("button")
        }
      } else {
        shinyjs::disable("button")
      }
    })
    
    # If all negative controls for one or more clusters are uncalibrated, show a warning.
    # observeEvent() to prevent the message from showing up when choosing manual cut-offs
    observeEvent(calculated_cut_offs(), {
      req(cut_offs_based_on_controls(), input$curation_method == "Negative control spectra")
      # Check if there are clusters for which there is no cut-off value
      if (missing_cluster_cut_offs() == TRUE) {
        # Determine missing clusters
        clusters_available = ifelse(
          !rlang::is_empty(cut_offs_based_on_controls()$cluster),
          cut_offs_based_on_controls()$cluster,
          c("")
        )
        clusters_missing <- setdiff(clusters(), clusters_available)  # The ordering in setdiff(x, y) matters
        # Show a warning message
        showNotification(
          tags$div(
            "For the following clusters, all negative control spectra are either missing or uncalibrated: ",
            paste0(clusters_missing, collapse = ", "),
            br(),
            br(),
            "Please do one of the following:",
            tags$ul(
              tags$li("Use different or additional negative controls."),
              tags$li("Choose manual cut-offs for these clusters."),
              tags$li("Choose to treat uncalibrated spectra as zeros, instead of missing values.")
            )
          ),
          type = "warning",
          duration = NULL
        )
      }
    })
    

    # Perform spectra curation when button is clicked:
    curated_data <- reactive({
      curate_spectra(checked_data = checked_data(),
                     summarized_checks = summarized_checks(),
                     cut_offs = cut_offs_to_use_all_clusters())
    }) %>% bindEvent(input$button)
    
    
    # Tell users to re-perform spectra curation when data is updated
    # after curating the spectra earlier.
    observeEvent(checked_data(), {
      req(curated_data())
      showNotification(
        id = ns("msg_data_changed"),
        'Changes were made to your data.
        Please curate your spectra again by clicking the 
        "Perform spectra curation" button.',
        type = "warning", duration = NULL,
        closeButton = FALSE
      )
    })
    
    # Remove the message automatically after curation button is clicked
    observeEvent(input$button, {
      removeNotification(ns("msg_data_changed"))
    })
    
    
    passing_spectra <- reactive({
      req(curated_data())
      kick_out_spectra(curated_spectra = curated_data())
    })
    
    
    to_return <- reactive({
      if (input$curation_method == "Skip spectra curation") {
        req(checked_data(),
            summarized_checks())
        return_when_spectra_curation_is_skipped(
          checked_data = checked_data(),
          summarized_checks = summarized_checks()
        )
      } else {
        req(passing_spectra())
        remove_unneeded_columns(passing_spectra = passing_spectra())
      }
    })
    
    
    output$passing_spectra_details <- DT::renderDataTable({
      req(to_return())
      DT::datatable(to_return() %>% dplyr::mutate_if(is.numeric, ~ round(., 2)),
                    options = list(
                      scrollX = TRUE,
                      pageLength = 5,
                      columnDefs = list(list(className = "dt-center", targets = "_all"))
                    ), filter = "top")
    })
    
    
    output$failed_spectra_table <- DT::renderDataTable({
      req(curated_data())
      
      for_table <- curated_data()%>% 
        dplyr::select(1:cut_off_passing_analyte_percentage) %>% 
        dplyr::distinct() %>% 
        dplyr::filter(!has_passed_spectra_curation)
      
      DT::datatable(for_table %>% 
                      dplyr::mutate_if(is.numeric, ~format(round(., 2), nsmall = 2)),
                    options = list(
                      scrollX = TRUE,
                      pageLength = 5,
                      columnDefs = list(list(className = "dt-center", targets = "_all"))
                    ), filter = "top")
    })
    
    
    output$failed_spectra_details <- DT::renderDataTable({
      req(curated_data())
      
      DT::datatable(curated_data() %>% 
                      dplyr::select(-(passing_analyte_percentage:replicates)) %>% 
                      dplyr::distinct() %>% 
                      dplyr::filter(has_passed_spectra_curation == FALSE) %>% 
                      dplyr::mutate_if(is.numeric, ~format(round(., 2), nsmall = 2)),
                    options = list(
                      scrollX = TRUE,
                      pageLength = 5,
                      columnDefs = list(list(className = "dt-center", targets = "_all"))
                    ), filter = "top")
    })

    
    # Create a counter to track how many times analyte curation is performed.
    # This is used to generate unique tab ids each curation round.
    counter <- reactiveValues(count = 0)
    
    # Create curation results plots
    observeEvent(curated_data(), {
      # Up the counter by one
      counter$count <- counter$count + 1
      # Remove tabs in case they have been created before. 
      purrr::map(names(r$curated_spectra_plots),
                 function(current_cluster) {
                   removeTab("plots_curation_results",
                             target = current_cluster)
                 })
      # Reset
      r$curated_spectra_plots <- NULL
      # Create new tabs with clusters as titles
      purrr::map(clusters(),
                 function(current_cluster) {
                   appendTab("plots_curation_results",
                             select = TRUE,
                             tabPanel(
                               title = current_cluster,
                               mod_tab_curated_spectra_plot_ui(
                                 # Already using ns(cluster) somewhere else in
                                 # mod_spectra_curation, so need to use a
                                 # different namespace here:
                                 ns(paste0(current_cluster, "_", counter$count)))
                             )
                   )
                 })
      # Generate the plots for the tabs
      r$curated_spectra_plots <- rlang::set_names(clusters()) %>% 
        purrr::map(
          .,
          function(current_cluster) {
            mod_tab_curated_spectra_plot_server(
              id = paste0(current_cluster, "_", counter$count), 
              curated_data = reactive({ 
                curated_data() %>% 
                  dplyr::filter(cluster == current_cluster) 
              }),
              total_and_specific = total_and_specific())
          })

    })
    
    
    
    # TODO: shorten this code
    observe({
      # Toggle visibility of tabs
      tab_names <- c(
        "Details of failed spectra per analyte",
        "Overview of failed spectra",
        "Details of passing spectra per analyte"
      )
      if (is_truthy(to_return())) {
        purrr::map(tab_names, function(tab_name) {
          showTab(inputId = "result_tables", target = tab_name, select = TRUE)
        })
      } else {
        purrr::map(tab_names, function(tab_name) {
          hideTab(inputId = "result_tables", target = tab_name)
        })
      }
      # Download buttons
      shinyjs::toggleState("download1", is_truthy(to_return()))
      shinyjs::toggleState("download2", is_truthy(curated_data()))
      shinyjs::toggleState("download3", is_truthy(curated_data()))
    })
    
    output$download1 <- downloadHandler(
      filename = function() {
        current_datetime <- paste0(format(Sys.Date(), "%Y%m%d"), "_", format(Sys.time(), "%H%M"))
        switch(input$download_format,
               "R object" = paste0(current_datetime, "_passing_spectra_details.rds"),
               "Excel file" = paste0(current_datetime, "_passing_spectra_details.xlsx"))
      },
      content = function(file) {
        data_to_download <- to_return()
        switch(input$download_format,
               "R object" = save(data_to_download, 
                                 file = file),
               "Excel file" = writexl::write_xlsx(data_to_download, 
                                                  path = file))
      }
    )
    
    
    output$download2 <- downloadHandler(
      filename = function() {
        current_datetime <- paste0(format(Sys.Date(), "%Y%m%d"), "_", format(Sys.time(), "%H%M"))
        switch(input$download_format,
               "R object" = paste0(current_datetime, "_failed_spectra_overview.rds"),
               "Excel file" = paste0(current_datetime, "_failed_spectra_overview.xlsx"))
      },
      content = function(file) {
        data_to_download <- curated_data() %>%
          dplyr::select(1:cut_off_passing_analyte_percentage) %>%
          dplyr::distinct() %>%
          dplyr::filter(!has_passed_spectra_curation)
        switch(input$download_format,
               "R object" = save(data_to_download, 
                                 file = file),
               "Excel file" = writexl::write_xlsx(data_to_download, 
                                                  path = file))
      }
    )
    
    
    output$download3 <- downloadHandler(
      filename = function() {
        current_datetime <- paste0(format(Sys.Date(), "%Y%m%d"), "_", format(Sys.time(), "%H%M"))
        switch(input$download_format,
               "R object" = paste0(current_datetime, "_failed_spectra_details.rds"),
               "Excel file" = paste0(current_datetime, "_failed_spectra_details.xlsx"))
      },
      content = function(file) {
        data_to_download <- curated_data() %>%
          dplyr::select(-(passing_analyte_percentage:replicates)) %>%
          dplyr::distinct() %>%
          dplyr::filter(has_passed_spectra_curation == FALSE)
        switch(input$download_format,
               "R object" = save(data_to_download, 
                                 file = file),
               "Excel file" = writexl::write_xlsx(data_to_download, 
                                                  path = file))
      }
    )
    
    
    
    return(list(
      passing_spectra = to_return,
      mass_acc = reactive({ input$mass_accuracy }),
      ipq = reactive({ input$ipq }),
      sn = reactive({ input$sn }),
      idp = reactive(input$idp),
      total_area = reactive(input$total_area),
      included_qc = reactive({ input$qc_to_include }),
      uncalibrated_as_NA = reactive({ input$uncalibrated_as_na }),
      cut_off = reactive({input$cut_off_basis}),
      tab_contents = reactive({ r$tab_contents }),
      curated_spectra_plots = reactive({ r$curated_spectra_plots })
    ))
    
  })
}
