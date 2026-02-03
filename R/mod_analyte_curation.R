#' analyte_curation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_analyte_curation_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML(paste0(
      "#", ns("box_header")," .awesome-checkbox {padding-top: 7px}",
      "#", ns("box_header"), " .popover {max-width: 400px !important; color: #333}",
      "#", ns("box")," .box-title {width: 100%}",
      "#", ns("box_header"), " .fas {float: right; margin-right: 5px; font-size: 18px}",
      "#", ns("box_header"), " .direct-chat-contacts {right: 0; background: #222d32!important}",
      "#", ns("box_header"), " .btn {float: right; border-width: 0px; margin-right: 10px}",
      "#", ns("box"), " .dropdown {display: inline-block; float: right; width: 330px}",
      "#", ns("box_header"), " .dropdown-menu {background: #333; right: -30px; left: auto; top: 28px;}"
    ))),
    fluidPage(
      fluidRow(h1("Analyte curation")),
      fluidRow(
        column(
          width = 6,
          # -- BOX WITH ANALYTE CURATION SETTINGS --
          shinydashboardPlus::box(
            id = ns("box"),
            # Title bar
            title = div(
              id = ns("box_header"), "Method for analyte curation",
              icon("info-circle", class = "ml") %>% 
                bsplus::bs_embed_popover(
                  title = "Explanation",
                  content = HTML(""), # TODO: Fill in
                  trigger = "hover", placement = "right", html = "true", container = "body"
                ),
              shinyWidgets::dropdownButton(
                shinyWidgets::awesomeCheckboxGroup(
                  ns("qc_to_include"), label = ("Which analyte quality criteria 
                  should be taken into account during analyte curation?"),
                  # Choices are determined in server based on type of data
                  choices = c(""), selected = c(""), status = "primary"
                ),
                icon = icon("gears", class = "ml"),
                tooltip = shinyWidgets::tooltipOptions(
                  placement = "top", title = "Advanced settings"
                ), 
                width = "250px", size = "xs"
              )
            ),
            width = NULL, solidHeader = TRUE, status = "primary",
            # Dropdown menu for choosing curation method
            selectInput(
              ns("curation_method"), "Analyte curation method:",
              choices = c(
                "Based on percentages of passing spectra",
                "Based on average QC parameters",
                "Per sample",
                "Supply an analyte list"
              )
            ) %>% 
              bsplus::bs_embed_popover(
                title = "Explanation",
                content = HTML(""),  # TODO: Fill in 
                trigger = "hover", placement = "right", html = "true"
              ),
            # <div> for analytes list
            shinyjs::hidden(div(
              id = ns("div_curation_list"),
              fileInput(
                ns("analyte_list"),
                "Upload an Excel file or R object with an analyte list"
              ) %>% 
                bsplus::bs_embed_popover(
                  title = "Explanation",
                  content = HTML(""),  # TODO fill in
                  trigger = "hover", placement = "right", html = "true"
                )
            )),
            # <div> for percentages or averages
            shinyjs::hidden(div(
              id = ns("div_curation_pct_avg"),
              # Option to specify biological groups
              shinyWidgets::materialSwitch(
                ns("curate_per_group"), 
                HTML("<i style='font-size:15px;'> Specify biological groups </i>"),
                status = "success", right = TRUE
              ),  # TODO: Add popup box
              # Nested <div> for biological groups
              shinyjs::hidden(div(
                id = ns("div_biological_groups"),
                selectInput(
                  ns("biogroup_column"), 
                  "Variable in data specifying biological group:",
                  choices = c("")  # Updated in server
                ),
                selectizeInput(
                  ns("groups_to_ignore"),
                  "Biological groups to ignore during analyte curation:",
                  choices = c(""), multiple = TRUE
                ) %>% 
                  bsplus::bs_embed_popover(
                    title = "Explanation",
                    content = HTML(""),  # TODO fill in
                    trigger = "hover", placement = "right", html = "true"
                  )
              )),
              # Option to ignore sample types
              selectizeInput(
                ns("sample_types_to_ignore"), 
                "Sample types to ignore during analyte curation:",
                choices = c(""), multiple = TRUE
              ) %>% 
                bsplus::bs_embed_popover(
                  title = "Explanation",
                  content = HTML(""),  # TODO fill in
                  trigger = "hover", placement = "right", html = "true"
                ),
              # Nested <div> for cut-off percentages
              div(
                id = ns("div_cutoff_percentages"),
                # Option to set cut-offs per cluster (percentages or averages)
                shinyWidgets::materialSwitch(
                  ns("cut_offs_per_cluster"),
                  HTML("<i style='font-size:15px;'> Choose cut-offs per glycosylation site </i>"),
                  right = TRUE, status = "success"
                ),
                uiOutput(ns("cluster_cut_offs_percentages")),
                numericInput(
                  ns("cut_off_percentages"), "Cut-off (%)", 
                  value = 80, min = 0, max = 100
                ) %>% 
                  bsplus::bs_embed_popover(
                    title = "Explanation",
                    content = HTML(""), # TODO fill in
                    placement = "right", trigger = "hover", html = "true")
              ),
              # Nested <div> for cut-off averages
              div(
                id = ns("div_cutoff_averages"),
                # Choose between median and mean
                selectInput(
                  ns("average_method"), "Method for calculating averages:",
                  choices = c("Mean", "Median")
                ),
                sliderInput(
                  ns("avg_mass_accuracy"),
                  "Acceptable mass accuracy range (ppm)",
                  min = -50, max = 50, value = c(-20, 20)
                ),
                numericInput(
                  ns("avg_ipq"),
                  "Maximum isotopic pattern quality:",
                  value = 0.2, min = 0, max = NA
                ),
                numericInput(
                  ns("avg_sn"), 
                  "Minimum S/N:",
                  value = 9, min = 0, max = NA
                ),
                numericInput(
                  ns("avg_idp"),
                  "Minimum isotope dot product",
                  value = 0.9, min = 0, max = 1
                ),
                numericInput(
                  ns("avg_total_area"),
                  "Minimum total area:",
                  value = 0, min = 0, max = NA
                )
              )
            )),
            actionButton(ns("curate_analytes"), "Perform analyte curation")
          ),
          shinydashboard::box(
            title = "Export results", width = NULL, solidHeader = TRUE,
            status = "primary",
            radioButtons(
              ns("download_format"), "Choose a file format:",
              choices = c("Excel file", "R object")
            ),
            downloadButton(ns("download"), "Download analyte-curated data")
          )
        )
      ),
      fluidRow(
        div(
          id = ns("tabbed_box"),
          shinydashboard::box(
            width = 12, solidHeader = TRUE, status = "primary",
            title = "Results per glycosylation site",
            tabsetPanel(id = ns("tabs"))
          )
        )
      )
    )
  )
}



#' analyte_curation Server Functions
#'
#' @noRd
mod_analyte_curation_server <- function(id,
                                        results_spectra_curation,
                                        biogroup_cols,
                                        data_type) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Toggle visibility of UI elements based on user input
    observe({
      if (input$curation_method %in% c(
        "Based on percentages of passing spectra", "Based on average QC parameters"
      )) {
        shinyjs::hide("div_curation_list")
        shinyjs::show("div_curation_pct_avg")
        if (input$curate_per_group) {
          shinyjs::show("div_biological_groups")
        } 
        else {
          shinyjs::hide("div_biological_groups")
        }
        if (input$curation_method == "Based on percentages of passing spectra") {
          shinyjs::show("div_cutoff_percentages")
          shinyjs::hide("div_cutoff_averages")
          if (input$cut_offs_per_cluster) {
            shinyjs::hide("cut_off_percentages")
            shinyjs::show("cluster_cut_offs_percentages")
          }
          else {
            shinyjs::show("cut_off_percentages")
            shinyjs::hide("cluster_cut_offs_percentages")
          }
        }
        else if (input$curation_method == "Based on average QC parameters") {
          shinyjs::hide("div_cutoff_percentages")
          shinyjs::show("div_cutoff_averages")
        }
      }
      else if (input$curation_method == "Per sample") {
        shinyjs::hide("div_curation_list")
        shinyjs::hide("div_curation_pct_avg")
      }
      else if (input$curation_method == "Supply an analyte list") {
        shinyjs::show("div_curation_list")
        shinyjs::hide("div_curation_pct_avg")
      }
    })
    
    
    # Determine QC parameters based on data type.
    qc <- reactiveValues(parameters = NULL)
    observeEvent(data_type(), {
      if (data_type() %in% c("LaCyTools data", "SweetSuite data")) {
        qc$parameters <- c("Mass accuracy", "Isotopic pattern quality", "S/N")
      }
      else if (data_type() == "Skyline data") {
        qc$parameters <- c("Mass accuracy", "Isotope dot product", "Total area")
      }
    })
    
    # Update UI based on data type
    observeEvent(qc$parameters, {
      # Checkboxes under gear icon based.
      shinyWidgets::updateAwesomeCheckboxGroup(
        inputId = "qc_to_include", 
        choices = qc$parameters, selected = qc$parameters
      )
      # Average QC parameters.
      if (data_type() %in% c("LaCyTools data", "SweetSuite data")) {
        shinyjs::show("avg_ipq")
        shinyjs::show("avg_sn")
        shinyjs::hide("avg_idp")
        shinyjs::hide("avg_total_area")
      }
      else if (data_type() == "Skyline data") {
        shinyjs::hide("avg_ipq")
        shinyjs::hide("avg_sn")
        shinyjs::show("avg_idp")
        shinyjs::show("avg_total_area")
      }
    })
    
    
    # Add choices for biological groups.
    observe({
      req(biogroup_cols())
      updateSelectInput(
        inputId = "biogroup_column",
        choices = c(biogroup_cols())
      )
    })
    
    
    # Output for all spectra that passed the curation
    passing_spectra <- reactive({
      req(results_spectra_curation$passing_spectra())
      results_spectra_curation$passing_spectra()
    })
  
    
    # Generate inputs for percentages cluster.
    cut_off_ids <- reactive({
      req(passing_spectra(), input$cut_offs_per_cluster)
      unique(passing_spectra()$cluster)
    })
    
    output$cluster_cut_offs_percentages <- renderUI(
      purrr::map(
        cut_off_ids(), function(id) {
          numericInput(
            ns(id), label = paste0(id, " cut-off (%)"),
            min = 0, max = 100, value = 80
          )
        }
      )
    )
    
    
    # Options for sample types to ignore.
    observe({
      req(passing_spectra())
      choices <- paste(unique(passing_spectra()$sample_type), "samples")
      if ("group" %in% colnames(passing_spectra())) {
        # Add total/specific samples as options.
        choices <- c(choices, paste(unique(passing_spectra()$group), "samples"))
      }
      updateSelectizeInput(
        inputId = "sample_types_to_ignore", choices = c(choices),
        options = list(maxItems = length(choices) - 1)
      )
    })
    
    # Options for biological groups to ignore
    observe({
      req(is_truthy(input$biogroup_column))
      choices <- as.character(dplyr::pull(
        unique(passing_spectra()[input$biogroup_column]) %>% 
          tidyr::drop_na()
      ))
      updateSelectizeInput(
        inputId = "groups_to_ignore", choices = c(choices),
        options = list(maxItems = length(choices) - 1)
      )
    })
    
    
    # Read in the analyte list when it is uploaded. Show a warning if
    # it's the wrong file extension or not formatted correctly.
    analyte_list <- reactive({
      req(input$curation_method == "Supply an analyte list", input$analyte_list)
      shinyFeedback::hideFeedback("analyte_list")
      analytes <- tryCatch(
        expr = {
          read_analyte_list_file(
            input$analyte_list$datapath, input$analyte_list$name
          )
        },
        wrong_extension = function(c) {
          shinyFeedback::feedbackDanger(
            "analyte_list", show = TRUE, text = c$message
          )
          NULL
        },
        missing_columns = function(c) {
          error_message_first_sentence <- stringr::str_replace(
            c$message, "(.+\\.).+", "\\1"
          )
          shinyFeedback::feedbackDanger(
            "analyte_list", show = TRUE, text = error_message_first_sentence
          )
          NULL
        },
        too_many_columns = function(c) {
          showNotification(c$message, type = "error")
          shinyFeedback::feedbackDanger(
            "analyte_list", show = TRUE, text = c$message
          )
          NULL
        })
      
      return(analytes)
    })
    
    
    # Create a reactiveValues vector to store results from the tabs.
    r <- reactiveValues(
      mod_results = list(), created_cluster_tabs = vector()
    )
    # Create a counter to track how many times analyte curation is performed.
    # This is used to generate unique tab ids each curation round.
    counter <- reactiveValues(count = 0)
    
    # When user pushes the button:
    observeEvent(input$curate_analytes, {
      # Remove message (if it exists)
      removeNotification(ns("msg_data_changed"))
      # Update the counter
      counter$count <- counter$count + 1
      # Show spinner, it is removed after data normalization
      shinybusy::show_modal_spinner(
        spin = "cube-grid", color = "#0275D8",
        text = HTML("<br/><strong>Curating analytes...")
      )
      # Remove tabs that may have been created before
      for (cluster in r$created_cluster_tabs) {
        removeTab(inputId = "tabs", target = cluster)
      }
      # Reset reactiveValues vector
      r$mod_results <- list()
      r$created_cluster_tabs <- vector()
      # If curation is done per sample: hide plots
      shinyjs::toggle(
        "tabbed_box",
        condition = input$curation_method != "Per sample"
      )
    },
    # Priority to make sure this code is executed first when button is pushed.
    priority = 10)
    
    
    # Get data of passing spectra but without samples types to ignore.
    # Non-glycosylated peptides are also excluded.
    # In case of curation per sample: just the data without non-glycosylated peptides.
    without_samples_to_ignore <- reactive({
      req(passing_spectra(), input$curation_method != "Supply an analyte list")
      without_nonglycosylated <- passing_spectra() %>%
        dplyr::filter(analyte != paste0(cluster, "1"))
      
      if (input$curation_method == "Per sample") {
        without_nonglycosylated
      }
      else if (is_truthy(input$sample_types_to_ignore)) {
        throw_out_samples(
          passing_spectra = without_nonglycosylated,
          samples_to_ignore = input$sample_types_to_ignore
        )
      }
      else {
        without_nonglycosylated
      }
    })
    

    # checked_analytes is the same as without_samples_to_ignore, but with new
    # columns describing whether an analyte fulfills all three quality criteria.
    checked_analytes <- reactive({
      req(input$curation_method %in% c(
        "Based on percentages of passing spectra", 
        "Based on average QC parameters",
        "Per sample"
      ))
      req(without_samples_to_ignore())
      
      if (data_type() %in% c("LaCyTools data", "SweetSuite data")) {
        check_analyte_quality_criteria_lacytools(
          without_samples_to_ignore(),
          min_ppm_deviation = results_spectra_curation$mass_acc()[1],
          max_ppm_deviation = results_spectra_curation$mass_acc()[2],
          max_ipq = results_spectra_curation$ipq(),
          min_sn = results_spectra_curation$sn(),
          criteria_to_consider = input$qc_to_include
        )
      } 
      else if (data_type() == "Skyline data") {
        check_analyte_quality_criteria_skyline(
          without_samples_to_ignore(),
          min_ppm_deviation = results_spectra_curation$mass_acc()[1],
          max_ppm_deviation = results_spectra_curation$mass_acc()[2],
          min_idp = results_spectra_curation$idp(),
          min_total_area = results_spectra_curation$total_area(),
          criteria_to_consider = input$qc_to_include
        )
      }
    })
    

    # Create a named list with a cut-off percentage for each cluster.
    cut_offs_percentages <- reactive({
      req(passing_spectra())
      req(input$curation_method == "Based on percentages of passing spectra")
      
      clusters <- unique(passing_spectra()$cluster)
      
      if (input$cut_offs_per_cluster) {
        purrr::map(clusters, function(cluster) {
          dplyr::case_when(
            input[[cluster]] < 0 ~ 0,
            input[[cluster]] > 100 ~ 100,
            .default = input[[cluster]]
          )
        }) %>% purrr::set_names(clusters)
      }
      else {
        purrr::map(clusters, function(cluster) {
          dplyr::case_when(
            input$cut_off_percentages < 0 ~ 0,
            input$cut_off_percentages > 100 ~ 100,
            .default = input$cut_off_percentages
          )
        }) %>% purrr::set_names(clusters)
      }
    })
    
    
    # Create a list with cut-off averages
    cut_offs_averages <- reactive({
      req(passing_spectra())
      req(input$curation_method == "Based on average QC parameters")
      list(
        "mass_accuracy" = input$avg_mass_accuracy,
        "max_ipq" = dplyr::case_when(
          input$avg_ipq < 0 ~ 0,
          .default = input$avg_ipq
        ),
        "min_sn" = dplyr::case_when(
          input$avg_sn < 0 ~ 0,
          .default = input$avg_sn
        ),
        "min_idp" = dplyr::case_when(
          input$avg_idp < 0 ~ 0,
          .default = input$avg_idp
        ),
        "min_total_area" = dplyr::case_when(
          input$avg_total_area < 0 ~ 0, 
          .default = input$avg_total_area
        )
      )
    })
    

    # Curate the analytes when user pushed the button.
    # This creates a dataframe with the passing percentage for each analyte,
    # and whether that analyte passes curation (TRUE or FALSE).
    # When curation is done per biological group, each analyte is present multiple
    # times in the dataframe (once for each biological group).
    curated_analytes <- reactive({
      if (input$curation_method == "Based on percentages of passing spectra") {
        req(checked_analytes(), cut_offs_percentages())
        if (input$curate_per_group) {
          checked_analytes() %>% 
            # Drop samples not belonging to a biological group (e.g. pools, blanks)
            tidyr::drop_na(., input$biogroup_column) %>%
            # Drop samples in biological groups that should be ignored
            dplyr::filter(., !.data[[input$biogroup_column]] %in% input$groups_to_ignore) %>% 
            # Perform the curation
            curate_analytes(., cut_offs_percentages(), input$biogroup_column)
        }
        else {
          curate_analytes(
            checked_analytes(), 
            cut_offs_percentages = cut_offs_percentages()
          )
        }
      }
      else if (input$curation_method == "Based on average QC parameters") {
        req(checked_analytes(), cut_offs_averages())
        if (input$curate_per_group) {
          checked_analytes() %>% 
            tidyr::drop_na(., input$biogroup_column) %>% 
            dplyr::filter(., !.data[[input$biogroup_column]] %in% input$groups_to_ignore) %>% 
            curate_analytes(
              .,
              cut_offs_averages = cut_offs_averages(),
              average_method = input$average_method,
              data_type = data_type(),
              bio_groups_colname = input$biogroup_column,
              qc_to_include = input$qc_to_include
            )
        }
        else {
          curate_analytes(
            checked_analytes(),
            cut_offs_averages = cut_offs_averages(),
            average_method = input$average_method,
            data_type = data_type(),
            qc_to_include = input$qc_to_include
          )
        }
      }
      else if (input$curation_method == "Per sample") {
        req(checked_analytes())
        checked_analytes() %>% 
          dplyr::rename(has_passed_analyte_curation = analyte_meets_criteria) %>% 
          dplyr::select(-failed_criteria)
      }
      else if (input$curation_method == "Supply an analyte list") {
        req(analyte_list(), passing_spectra())
        tryCatch(
          expr = {
            curate_analytes_with_list(passing_spectra(), analyte_list())
          },
          missing_analytes = function(c) {
            showNotification(c$message, type = "warning")
            suppressWarnings(
              curate_analytes_with_list(passing_spectra(), analyte_list())
            )
          }
        )
      }
    }) %>% bindEvent(input$curate_analytes)


    # Check that curated_analytes is not empty.
    check_curated_analytes <- reactive({
      req(curated_analytes())
      if (nrow(curated_analytes()) > 0) {
        TRUE
      }
      else FALSE
    })
    
    observeEvent(check_curated_analytes(), {
      if (!check_curated_analytes()) {
        showNotification(
          "No data to perform analyte curation. Did you ignore all samples?",
          type = "error", duration = NULL
        )
        shinybusy::remove_modal_spinner()
      }
    })
    
    
    # analyte_curated_data is a dataframe with the output of the
    # passing spectra, but only for analytes that fulfill the analyte criteria.
    analyte_curated_data <- reactive({
      req(check_curated_analytes() == TRUE)
      if (input$curation_method == "Per sample") {
        # Left join not necessary when curation is done per sample.
        curated_analytes()
      }
      else {
        # This combines the info from curated_analytes (whether analytes pass or not)
        # with the output of the passing spectra.
        # The order here is important in the case of curation per biological group,
        # when one or more groups should be ignored. This order ensures that
        # only groups that should be taken into account are part of the resulting
        # dataframe (reversing the order would include all biological groups).
        dplyr::left_join(curated_analytes(), passing_spectra())
      }
    }) %>% bindEvent(curated_analytes())

    # Tell users to re-perform analyte curation when data is updated
    # after curating the analytes earlier.
    # (It is removed in code above.)
    observeEvent(passing_spectra(), {
      req(analyte_curated_data())
      showNotification(
        id = ns("msg_data_changed"),
        'Changes were made to your data.
        Please curate your analytes again by clicking the
        "Perform analyte curation" button.',
        type = "warning", duration = NULL,
        closeButton = FALSE
      )
    })
    

    # Create a vector with names of the clusters
    clusters <- reactive({
      req(analyte_curated_data())
      sort(unique(analyte_curated_data()$cluster))
    })
    

    # Create tabs for each cluster, when clusters() changes.
    # input$tabs always takes on the value (cluster name) of the selected tab
    # Note that clusters() updates whenever analyte_curated_data() updates.
    observeEvent(clusters(), {
      # Create one tab for each cluster, store the name of the cluster in r.
      for (i in seq(length(clusters()))) {
        r$created_cluster_tabs[i] <- clusters()[i]
        appendTab(
          inputId = "tabs",
          select = TRUE,
          session = session,
          tab = tabPanel(
            title = clusters()[[i]],
            mod_tab_curated_analytes_ui(ns(
              paste0(clusters()[[i]], "_", counter$count)
            ))
          )
        )
      }
    })

    
    # Collect settings.
    info <- reactive({
      req(analyte_curated_data())
      if (input$curation_method == "Based on average QC parameters") {
        list(
          curated_analytes = curated_analytes(),
          cut_offs_averages = cut_offs_averages(),
          analyte_curated_data = analyte_curated_data(),
          curation_method = input$curation_method
        )
      }
      else {
        list(
          curated_analytes = curated_analytes(),
          cut_offs_percentages = cut_offs_percentages(),
          analyte_curated_data = analyte_curated_data(),
          curation_method = input$curation_method
        )
      }
    }) %>% bindEvent(analyte_curated_data())

    
    observeEvent(info()$analyte_curated_data, {
      req(clusters(), input$curation_method != "Per sample")
      r$mod_results <- purrr::set_names(clusters()) %>%
        purrr::map(., function(cluster) {
          mod_tab_curated_analytes_server(
            id = paste0(cluster, "_", counter$count),
            info = info(),
            cluster = cluster,
            biogroup_column = ifelse(
              test = input$curate_per_group,
              yes = input$biogroup_column,
              no = ""
            )
          )
        })
    })
  
    
    with_analytes_to_include <- reactive({
      if (input$curation_method == "Per sample") {
        # req() below is required to prevent the code from running too soon
        # when switching to "Per sample" from a different method.
        # analyte_curated_data() needs to update first
        req("uncalibrated" %in% colnames(analyte_curated_data()))
        to_return <- analyte_curated_data() %>% 
          dplyr::filter(has_passed_analyte_curation) %>% 
          dplyr::select(-has_passed_analyte_curation, -uncalibrated)
      }
      else {
        req(
          passing_spectra(), !rlang::is_empty(r$mod_results),
          all(purrr::map_lgl(r$mod_results, ~is_truthy(.x$analytes_to_include())))
        )
        to_return <- purrr::imap(
          r$mod_results, function(results, current_cluster) {
            data_current_cluster <- passing_spectra() %>% 
              dplyr::filter(cluster == current_cluster)
            dplyr::left_join(results$analytes_to_include(), data_current_cluster)
          }
        ) %>% purrr::reduce(dplyr::full_join)
      }
      # Get data with non-glycosylated peptides
      non_glycosylated <- passing_spectra() %>%
        dplyr::filter(analyte == paste0(cluster, "1")) %>%
        # Rearrange columns to combine with to_return
        dplyr::select(colnames(to_return))
      
      # Return
      if (nrow(non_glycosylated) > 0) {
        dplyr::bind_rows(to_return, non_glycosylated)
      } 
      else {
        to_return
      }
    })
      

    # Make downloading analyte_curated_data possible:
    output$download <- downloadHandler(
      filename = function() {
        current_datetime <- paste0(
          format(Sys.Date(), "%Y%m%d"), "_", format(Sys.time(), "%H%M")
        )
        switch(
          input$download_format,
          "R object" = paste0(current_datetime, "_curated_analytes.rds"),
          "Excel file" = paste0(current_datetime, "_curated_analytes.xlsx")
        )
      },
      content = function(file) {
        data_to_download <- with_analytes_to_include()
        switch(
          input$download_format,
          "R object" = save(data_to_download, file = file),
          "Excel file" = writexl::write_xlsx(data_to_download, path = file)
        )
      }
    )
    
    
    # Set status of buttons
    observe({
      shinyjs::toggleState("download", is_truthy(with_analytes_to_include()))
      shinyjs::toggleState(
        "curate_analytes", condition = all(
          is_truthy(passing_spectra()),
          length(input$qc_to_include) > 0,
          any(
            # Depending on analyte curation method
            all(
              input$curation_method == "Supply an analyte list",
              is_truthy(analyte_list())
            ),
            input$curation_method != "Supply an analyte list"
          )
        )
      )
    })
    

    return(list(
      analyte_curated_data = with_analytes_to_include,
      biogroups_colname = reactive(input$biogroup_column),  # TODO fix when not used
      included_qc = reactive(input$qc_to_include),
      method = reactive({input$curation_method}), 
      curation_method = reactive(input$curation_method), # TODO Remove duplicate
      ignore_samples = reactive({input$sample_types_to_ignore}),
      groups_to_ignore = reactive(input$groups_to_ignore),
      cut_offs = cut_offs_percentages,  # TODO Averages
      analyte_list = reactive({ input$analyte_list$name }),
      objects = reactive({ r$mod_results })
    ))

  })
}
