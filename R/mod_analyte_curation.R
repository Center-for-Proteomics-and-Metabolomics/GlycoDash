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
                  ns("group_column"), 
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
                uiOutput(ns("cut_offs_averages"))
              )
            )),
            actionButton(ns("curate_analytes"), "Perform analyte curation")
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
        else {
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
    r <- reactiveValues(qc_parameters = NULL)
    observeEvent(data_type(), {
      if (data_type() %in% c("LaCyTools data", "SweetSuite data")) {
        r$qc_parameters <- c("Mass accuracy", "Isotopic pattern quality", "S/N")
      }
      else if (data_type() == "Skyline data") {
        r$qc_parameters <- c("Mass accuracy", "Isotope dot product", "Total area")
      }
    })
    
    # QC checkboxes based on data type.
    observeEvent(r$qc_parameters, {
      # Checkboxes based on data type.
      shinyWidgets::updateAwesomeCheckboxGroup(
        inputId = "qc_to_include", 
        choices = r$qc_parameters, selected = r$qc_parameters
      )
    })
    
    # Generate cut-off options for average QC parameters.
    output$cut_offs_averages <- renderUI({
      init_map <- list(
        # (initial, min, max, description)
        "Mass accuracy" = c(20, 0, 100, "Maximum absolute mass accuracy:"),
        "Isotopic pattern quality" = c(0.2, 0, 1, "Maximum isotopic pattern quality:"),
        "S/N" = c(9, 0, NA, "Minimum S/N:"),
        "Isotope dot product" = c(0.9, 0, 1, "Minimum isotope dot product:"),
        "Total area" = c(0, 0, NA, "Minimum total area:")
      )
      purrr::map(r$qc_parameters, function(param) {
        numericInput(
          ns(param), 
          value = init_map[[param]][1],
          min = init_map[[param]][2],
          max = init_map[[param]][3],
          label = init_map[[param]][4]
        )
      })
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
      unique(passing_spectra$cluster)
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
    
    
    
    # TODO RESTRUCTURE CODE BELOW...

    # Create reactiveValues.
    # Below, rv_resp$response is created when analyte curation is performed per biological group.
    # Also store colum name of biological groups, to pass on to normalized data
    rv_resp <- reactiveValues(response = NULL, biogroups_colname = "")

    # Show pop-up with detected biological groups and ask for confirmation.
    observe({
      req(input$curation_method == "Per biological group")
      shinyalert::shinyalert(
        html = TRUE,
        text = paste(
          "The following biological groups were detected:",
          shinycssloaders::withSpinner(DT::dataTableOutput(ns("popup_table")))
        ),
        size = "m",
        confirmButtonText = "Accept these as biological groups",
        showCancelButton = TRUE,
        cancelButtonText = "Cancel",
        confirmButtonCol = "#3c8dbc",
        callbackR = function(response) {
          rv_resp$response <- response  # TRUE or FALSE, depending on whether user accepts biol. groups.
          if (response == TRUE) {
            rv_resp$biogroups_colname <- input$biogroup_column
          } else {
            rv_resp$biogroups_colname <- ""
          }
        }
      )
    }) %>% bindEvent(input$determine_groups_button) # Show pop-up window when button is clicked.


    # When user decides to change curation method, set rv_resp$response to NULL
    observeEvent(input$curation_method, {
      rv_resp$response <- NULL
      rv_resp$biogroups_colnames <- ""
    })

    output$popup_table <- DT::renderDataTable({
      DT::datatable(
        data.frame(unique(passing_spectra()[input$biogroup_column])) %>% tidyr::drop_na(),
        options = list(
          scrollY = "150px",
          paging = FALSE,
          searching = FALSE,
          columnDefs = list(
            list(
              className = 'dt-center',
              targets = "_all"))),
        colnames = "",
        rownames = FALSE
      )
    })


    # Generate input boxes for cut-offs per cluster
    cut_off_ids <- reactive({
      req(passing_spectra(), input$cut_offs_per_cluster == TRUE)
      unique(passing_spectra()$cluster)
    })

    output$cluster_cut_offs <- renderUI(
      purrr::map(cut_off_ids(), function(id) {
        numericInput(
          ns(id), label = paste0(id, " cut-off (%)"),
          min = 0, max = 100, value = 80
        )
      })
    )


    # # Show and hide UI based on the chosen method:
    # observe({
    # 
    #   if (input$curation_method == "Per sample") {
    #     shinyjs::hide("cut_offs_per_cluster")
    #   }
    #   else {
    #     shinyjs::show("cut_offs_per_cluster")
    #   }
    # 
    #   shinyjs::toggle("cut-off", input$cut_offs_per_cluster == FALSE)
    #   shinyjs::toggle("curation_method",
    #                   condition = input$method == "Curate analytes based on data")
    #   shinyjs::toggle("analyte_list_div",
    #                   condition = input$method == "Supply an analyte list")
    #   shinyjs::toggle("curation_based_on_data_div",
    #                   condition = input$method == "Curate analytes based on data")
    #   shinyjs::toggle("ignore_samples",
    #                   condition = input$method == "Curate analytes based on data" &
    #                   input$curation_method != "Per sample")
    #   shinyjs::toggle("groups_to_ignore",
    #                  condition = input$method == "Curate analytes based on data" &
    #                  input$curation_method == "Per biological group")
    #   # Only enable button under right circumstances:
    #   shinyjs::toggleState(
    #     "curate_analytes", condition = all(
    #       is_truthy(passing_spectra()),
    #       length(input$qc_to_include) > 0,
    #       any(
    #         all(input$method == "Supply an analyte list", is_truthy(analyte_list())),
    #         all(
    #           input$method == "Curate analytes based on data",
    #           any(input$curation_method != "Per biological group", isTRUE(rv_resp$response))
    #         )
    #       )
    #     )
    #   )
    #   # Only ask for analyte curation per biological group when "Curate analytes based on data"
    #   shinyjs::toggle("curate_per_group",
    #                   condition = input$method == "Curate analytes based on data")
    #   # Only show drop-down menu to choose biological groups column when the
    #   # user selects "Yes" when asked if curation should be done per group.
    #   shinyjs::toggle(
    #     "biogroup_column",
    #     condition = input$curation_method == "Per biological group"
    #   )
    #   shinyjs::toggle(
    #     "determine_groups_button",
    #     condition = input$curation_method == "Per biological group" & input$method == "Curate analytes based on data"
    #   )
    #   shinyjs::toggleState(
    #     "determine_groups_button",
    #     condition = input$biogroup_column != "" & input$curation_method == "Per biological group"
    #   )
    #   # Don't show cut-off option when doing curation per sample.
    #   shinyjs::toggle(
    #     "cut_off",
    #     condition = all(input$curation_method != "Per sample", input$cut_offs_per_cluster == FALSE)
    #   )
    #   # Toggle download button
    #   shinyjs::toggleState(
    #     "download", condition = is_truthy(with_analytes_to_include())
    #   )
    # }, priority = 5)


    # The selection menu for input$ignore_samples is updated so that the choices
    # are sample_types and groups that are present in the data.
    observe({
      if ("group" %in% colnames(passing_spectra())) {
        choices <- c(paste(unique(passing_spectra()$sample_type), "samples"),
                     paste(unique(passing_spectra()$group), "samples"))
      }
      else {
        choices <- c(paste(unique(passing_spectra()$sample_type), "samples"))
      }

      updateSelectizeInput(inputId = "ignore_samples",
                           choices = c(choices),
                           options = list(maxItems = length(choices) - 1))
    })


    # Update the selection menu for "Biological groups to ignore", based on the chosen column.
    observe({
      req(is_truthy(input$biogroup_column), rv_resp$response == TRUE)
      choices <- as.character(dplyr::pull(
        unique(passing_spectra()[input$biogroup_column]) %>%
        tidyr::drop_na()
      )) # as.character() for when the column contains factors, as is the case with sample_types
      updateSelectizeInput(inputId = "groups_to_ignore", choices = c(choices),
                           options = list(maxItems = length(choices) - 1))
    })

    observeEvent(input$biogroup_column, {
      rv_resp$response <- NULL
      updateSelectizeInput(inputId = "groups_to_ignore", choices = c(""))
    })


    # Read in the analyte list when it is uploaded. Show a Warning if
    # it's the wrong file extension or not formatted correctly:
    analyte_list <- reactive({
      req(input$method == "Supply an analyte list")
      req(input$analyte_list)
      shinyFeedback::hideFeedback("analyte_list")

      analytes <- tryCatch(
        expr = {
          read_analyte_list_file(input$analyte_list$datapath,
                                 input$analyte_list$name)

      },
      wrong_extension = function(c) {
        shinyFeedback::feedbackDanger("analyte_list",
                                      show = TRUE,
                                      text = c$message)
        NULL
      },
      missing_columns = function(c) {
        error_message_first_sentence <- stringr::str_replace(c$message,
                                                             "(.+\\.).+",
                                                             "\\1")

        shinyFeedback::feedbackDanger("analyte_list",
                                      show = TRUE,
                                      text = error_message_first_sentence)
        NULL
      },
      too_many_columns = function(c) {
        showNotification(c$message,
                         type = "error")

        shinyFeedback::feedbackDanger("analyte_list",
                                      show = TRUE,
                                      text = c$message)
        NULL
      })

      return(analytes)
    })



    # Create a reactiveValues vector to store results from the tabs.
    r <- reactiveValues(mod_results = list(),
                        created_cluster_tabs = vector())

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



    # without_samples_to_ignore contains the LaCyTools data of the passed spectra,
    # but without the sample types that the user wants to ignore in analyte curation.
    # Non-glycosylated peptides are also excluded.
    without_samples_to_ignore <- reactive({
      req(input$method == "Curate analytes based on data")
      req(passing_spectra())
      without_nonglycosylated <- passing_spectra() %>%
        dplyr::filter(analyte != paste0(cluster, "1"))
      if (input$curation_method == "Per sample") {
        without_nonglycosylated
      }
      else if (is_truthy(input$ignore_samples)) {
        throw_out_samples(passing_spectra = without_nonglycosylated,
                          samples_to_ignore = input$ignore_samples)
      }
      else {
        without_nonglycosylated
      }
    })


    # checked_analytes is the same as without_samples_to_ignore, but with new
    # columns describing whether an analyte fulfills all three quality criteria.
    checked_analytes <- reactive({
      req(input$method == "Curate analytes based on data")
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
      } else if (data_type() == "Skyline data") {
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


    # Create a named list with a cut-off percentage for each cluster
    # If cut-off was accidentally set below 0 or above 100, adjust
    cut_offs <- reactive({
      clusters <- unique(passing_spectra()$cluster)
      values <- vector("list", length(clusters))
      names(values) <- clusters
      if (input$cut_offs_per_cluster == FALSE) {
        # Same cut-off for each cluster
        for (cluster in clusters) {
          cut_off <- dplyr::case_when(
            input$cut_off < 0 ~ 0,
            input$cut_off > 100 ~ 100,
            .default = input$cut_off
          )
          values[[cluster]] <- cut_off
        }
      } else {
        # Separate cut-offs
        for (cluster in clusters) {
          cut_off <- dplyr::case_when(
            input[[cluster]] < 0 ~ 0,
            input[[cluster]] > 100 ~ 100,
            .default = input[[cluster]]
          )
          values[[cluster]] <- cut_off
        }
      }
      return(values)
    })


    # Curate the analytes when user pushed the button.
    # This creates a dataframe with the passing percentage for each analyte,
    # and whether that analyte passes curation (TRUE or FALSE).
    # When curation is done per biological group, each analyte is present multiple
    # times in the dataframe (once for each biological group).
    # TODO: turn (part of) this into a function
    curated_analytes <- reactive({
      if (input$method == "Curate analytes based on data") {
        if (isTRUE(rv_resp$response)) {
          # Curate per biological group
          curated_analytes <- checked_analytes() %>%
            # Drop samples that don't belong to a biological group (e.g. pools, blanks)
            tidyr::drop_na(., input$biogroup_column) %>%
            # Drop samples in biological groups that should be ignored
            dplyr::filter(., !.data[[input$biogroup_column]] %in% input$groups_to_ignore) %>%
            curate_analytes(., cut_offs(), input$biogroup_column)
        }
        else if (input$curation_method == "On all data") {
            curated_analytes <- curate_analytes(checked_analytes(), cut_offs())
        }
        else if (input$curation_method == "Per sample") {
            curated_analytes <- checked_analytes() %>%
              dplyr::rename(has_passed_analyte_curation = analyte_meets_criteria) %>%
              dplyr::select(-failed_criteria)
        }

      }
      else if (input$method == "Supply an analyte list") {
        req(analyte_list())
        req(passing_spectra())

        curated_analytes <- tryCatch(
          expr = {
            curate_analytes_with_list(
              passing_spectra = passing_spectra(),
              analyte_list = analyte_list()
            )
          },
          missing_analytes = function(c){
            showNotification(c$message,
                             type = "warning")
            suppressWarnings(
              curate_analytes_with_list(
                data = passing_spectra(),
                analyte_list = analyte_list()
              )
            )
          }
        )
      }
      return(curated_analytes)
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

    # analyte_curated_data is a dataframe with the LaCyTools output of the
    # passing spectra, but with only the analytes that passed curation.
    analyte_curated_data <- reactive({
      req(check_curated_analytes() == TRUE)
      if (input$curation_method == "Per sample") {
        # Left join not necessary when curation is done per sample.
        curated_analytes()
      }
      else {
        dplyr::left_join(
          # This combines the info from curated_analytes (whether analytes pass or not)
          # with the LaCyTools output of the passing spectra.
          # The order here is important in the case of curation per biological group,
          # when one or more groups should be ignored. This order ensures that
          # only groups that should be taken into account are part of the resulting
          # dataframe (reversing the order would include all biological groups).
          curated_analytes(), passing_spectra()
        )
      }
    }) %>% bindEvent(curated_analytes())



    # Tell users to re-perform analyte curation when data is updated
    # after curating the analytes earlier.
    # It is removed in code above.
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


    info <- reactive({
      req(analyte_curated_data())
      list(
        curated_analytes = curated_analytes(),
        cut_offs = cut_offs(),
        analyte_curated_data = analyte_curated_data(),
        method = input$method
      )
    }) %>% bindEvent(analyte_curated_data())


    observeEvent(info()$analyte_curated_data, {
      req(clusters())
      req(input$curation_method != "Per sample")
      r$mod_results <- purrr::set_names(clusters()) %>%
        purrr::map(., function(cluster) {
          mod_tab_curated_analytes_server(
            id = paste0(cluster, "_", counter$count),
            info = info(),
            cluster = cluster,
            biogroup_column = ifelse(
              isTRUE(rv_resp$response),
              input$biogroup_column,
              ""
            )
          )
        })
    })


    with_analytes_to_include <- reactive({
      if (input$curation_method == "Per sample") {
        # Analyte curation per sample
        # req() below is required to prevent the code from running too soon
        # when switching to "Per sample" from a different method.
        # analyte_curated_data() needs to update first.
        req("uncalibrated" %in% colnames(analyte_curated_data()))
        to_return <- analyte_curated_data() %>%
          dplyr::filter(has_passed_analyte_curation == TRUE) %>%
          dplyr::select(-has_passed_analyte_curation, -uncalibrated)
      } else {
        # Analyte curation on all data or per biological group
        req(
          passing_spectra(),
          !rlang::is_empty(r$mod_results),
          all(purrr::map_lgl(r$mod_results, ~is_truthy(.x$analytes_to_include())))
        )
        to_return <- purrr::imap(r$mod_results, function(results, current_cluster) {
          data_current_cluster <- passing_spectra() %>%
            dplyr::filter(cluster == current_cluster)
          dplyr::left_join(results$analytes_to_include(), data_current_cluster)
        }) %>%
          purrr::reduce(dplyr::full_join)
      }

      # Get data with non-glycosylated peptides
      non_glycosylated <- passing_spectra() %>%
        dplyr::filter(analyte == paste0(cluster, "1")) %>%
        # Rearrange columns to combine with to_return
        dplyr::select(colnames(to_return))

      # Return
      if (nrow(non_glycosylated) > 0) {
        return(dplyr::bind_rows(to_return, non_glycosylated))
      } else {
        return(to_return)
      }
    })


    # Make downloading analyte_curated_data possible:
    output$download <- downloadHandler(
      filename = function() {
        current_datetime <- paste0(format(Sys.Date(), "%Y%m%d"), "_", format(Sys.time(), "%H%M"))
        switch(input$download_format,
               "R object" = paste0(current_datetime, "_curated_analytes.rds"),
               "Excel file" = paste0(current_datetime, "_curated_analytes.xlsx"))
      },
      content = function(file) {
        data_to_download <- with_analytes_to_include()
        switch(input$download_format,
               "R object" = save(data_to_download,
                                 file = file),
               "Excel file" = writexl::write_xlsx(data_to_download,
                                                  path = file))
      }
    )


    return(list(
      analyte_curated_data = with_analytes_to_include,
      biogroups_colname = reactive(rv_resp$biogroups_colname),
      included_qc = reactive(input$qc_to_include),
      method = reactive({ input$method }), # based on data or analyte list,
      curation_method = reactive(input$curation_method),
      ignore_samples = reactive({ input$ignore_samples }),
      groups_to_ignore = reactive(input$groups_to_ignore),
      cut_offs = cut_offs,
      analyte_list = reactive({ input$analyte_list$name }),
      objects = reactive({ r$mod_results })
    ))

  })
}
