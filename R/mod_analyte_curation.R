#' analyte_curation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_analyte_curation_ui <- function(id){
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
      fluidRow(
        h1("Analyte curation")
      ),
      fluidRow(
        column(
          width = 6,
          # Box with settings for the analyte curation
          shinydashboardPlus::box(
            id = ns("box"),
            title = div(
              id = ns("box_header"),
              "Method for analyte curation",
              icon("info-circle", class = "ml") %>% 
                bsplus::bs_embed_popover(
                  title = "Explanation",
                  content = HTML(
                    "
                    Analyte curation will be performed based on the settings that were chosen
                    for the three quality criteria in the \"Spectra Curation\" tab.
                    <br> <br>
                    You can exclude one or two of the quality criteria from the assessment
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
                  "Which analyte quality criteria should be taken into account during analyte curation?",
                  # Choices determined in server based on data type
                  choices = c(""), selected = c(""), status = "primary"
                ),
                icon = icon("gears", class = "ml"),
                tooltip = shinyWidgets::tooltipOptions(placement = "top", title = "Advanced settings"),
                width = "250px",
                size = "xs"
              )
            ),
            width = NULL,
            solidHeader = TRUE,
            status = "primary",
            # Ask user to choose a method for analyte curation
            selectInput(ns("method"), 
                        "Choose method for analyte curation:",
                        choices = c("Curate analytes based on data",
                                    "Supply an analyte list")) %>% 
              bsplus::bs_embed_popover(
                title = "Explanation",
                content = "
                Analyte curation can be performed based on the data.
                You can also upload a list with analytes that should pass
                in all samples.
                ",
                trigger = "hover",
                placement = "right"
              ),
            tags$style(
              HTML(paste0("#",
                          ns("analyte_list_div"),
                          " .popover {width: 400px;}"))
            ),
            div(id = ns("analyte_list_div"),
                fileInput(ns("analyte_list"), 
                          "Upload an Excel file or R object with an analyte list") %>% 
                  bsplus::bs_embed_popover(
                    title = "Explanation",
                    content = HTML(
                      "
                      <b> Excel file </b> 
                      <br>
                      The file should consist of one column called \"analyte\",
                      which contains the names of the analytes that you want to keep.
                      <br> <br>
                      <b> R object </b>
                      <br>
                      The R object should be a character vector or a list of 
                      character strings (not a dataframe), with the names of the
                      analytes that you want to keep.
                      "
                    ),
                    html = "true",
                    trigger = "hover",
                    placement = "right")
            ),
            tags$style(HTML(paste0("#", ns("curation_based_on_data"), " .popover {width: 400px;}"))),
            selectInput(ns("curation_method"), 
                        "How do you want to perform analyte curation based on the data?",
                        choices = c("On all data",
                                    "Per biological group",
                                    "Per sample")) %>% 
              bsplus::bs_embed_popover(
                title = "Explanation",
                content = HTML("
                    <p> <b> On all data </b> <br>
                    When an analyte fulfills the quality criteria in a percentage
                    of spectra that is higher than the chosen cut-off (e.g. >25%),
                    then that analyte passes curation and is used for further analysis
                    in all samples.
                    
                    <p> <b> Per biological group </b> <br>
                    When an analyte fulfills the quality criteria in a percentage of
                    spectra above the cut-off in one or more of the biological groups,
                    then that analyte passes curation. Spectra without an assigned
                    biological group (e.g. blanks and standards) are not used in this
                    assessment.
                    
                    <p> <b> Per sample </b> <br>
                    For each sample only the analytes that fulfill all quality criteria
                    in that sample will be used for further analysis. </li>
                "),
                trigger = "hover",
                placement = "right",
                html = "true"
              ),
            
            div(
              id = ns("choose_biogroup_cols"),
              selectInput(
                ns("biogroup_column"),
                label = "Which variable (column) in your data contains the biological groups?",
                choices = ""  # Update in server to show column names
              )
            ),
            # Button to determine the biological groups
            actionButton(ns("determine_groups_button"),
                         "Determine the biological groups"),
            # Option to ignore certain biological groups
            div(
              id = ns("curation_based_on_groups_div"),
              selectizeInput(
                ns("groups_to_ignore"),
                HTML("<br/>Biological groups to ignore regarding analyte curation:"),
                choices = c(""),
                multiple = TRUE
              ) %>% 
              bsplus::bs_embed_popover(
                title = "Explanation",
                content = HTML("
                  <p>
                  You may want to exclude certain biological groups from the assessment,
                  for instance when the size of the group is very small.
                
                  <p> <i>
                  Note: if you chose \"sample_type\" as the column that contains 
                  the biological groups, you can either exclude a sample type/group here,
                  in the box below, or both. </i>
                "),
                trigger = "hover",
                placement = "right",
                html = "true"
                )
            ),
            
            div(
              id = ns("curation_based_on_data_div"),
              selectizeInput(ns("ignore_samples"),
                             HTML("Sample types to ignore regarding analyte curation:"),
                             choices = c(""),
                             multiple = TRUE) %>% 
                bsplus::bs_embed_popover(
                  title = "Explanation",
                    content = HTML(
                      "
                      Analytes are curated based on the percentage of spectra
                      in which they pass the analyte criteria (go back to the 
                      \"Spectra Curation\" tab to choose these criteria).
                      <br> <br>
                      You may want to exclude some sample types from this assessment
                      (e.g. blanks and standards). If your data contains total and specific
                      Ig samples, you can also exclude one of these.
                      <br> <br>
                      Select here which samples should be ignored with regards to analyte curation.
                      "
                    ),
                  placement = "right",
                  trigger = "hover",
                  html = "true"),
              shinyWidgets::materialSwitch(
                ns("cut_offs_per_cluster"),
                HTML("<i style='font-size:15px;'> Choose cut-offs per cluster </i>"),
                right = TRUE,
                status = "success"
              ),
              uiOutput(ns("cluster_cut_offs")),
              numericInput(ns("cut_off"), "Cut-off (%)", value = 50, min = 0, max = 100) %>% 
                bsplus::bs_embed_popover(
                  title = "Explanation",
                  content = HTML(
                    "
                    Choose the percentage of spectra in which an analyte should
                    fulfill the quality criteria in order to pass analyte curation.
                    "
                  ),
                  placement = "right",
                  trigger = "hover",
                  html = "true")
            ),
            actionButton(ns("curate_analytes"), 
                         "Perform analyte curation")
           
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
                           "Download analyte-curated data")
          )
        )
      ),
      fluidRow(
        div(
          id = ns("tabbed_box"),
          shinydashboard::box(
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            title = "Analyte curation results per cluster",
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
mod_analyte_curation_server <- function(id, results_spectra_curation, biogroup_cols, data_type) {
  moduleServer( id, function(input, output, session) {
    ns <- session$ns
    
    # Checkboxes based on data type (LaCyTools or Skyline)
    observeEvent(data_type(), {
      if (data_type() == "LaCyTools data") {
        shinyWidgets::updateAwesomeCheckboxGroup(
          inputId = "qc_to_include",
          choices = c("Mass accuracy", "Isotopic pattern quality", "S/N"),
          selected = c("Mass accuracy", "Isotopic pattern quality", "S/N")
        )
      } else if (data_type() == "Skyline data") {
        shinyWidgets::updateAwesomeCheckboxGroup(
          inputId = "qc_to_include",
          choices = c("Mass accuracy", "Isotope dot product", "Total area"),
          selected = c("Mass accuracy", "Isotope dot product", "Total area")
        )
      }
    })
    
    # passing_spectra contains the LaCyTools output for all the spectra
    # that passed curation.
    passing_spectra <- reactive({
      req(results_spectra_curation$passing_spectra())
      results_spectra_curation$passing_spectra()
    })
    
    
    # Create reactiveValues. 
    # Below, rv_resp$response is created when analyte curation is performed per biological group.
    # Also store colum name of biological groups, to pass on to normalized data
    rv_resp <- reactiveValues(response = NULL, biogroups_colname = "")
    
    # Show potential column names with biological groups
    observe({
      req(biogroup_cols())
      updateSelectInput(
        inputId = "biogroup_column",
        choices = c(biogroup_cols())
      )
    })
    
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
          min = 0, max = 100, value = 50
        )
      })
    )
    
    
    # Show and hide UI based on the chosen method:
    observe({
      shinyjs::toggle("cut-off", input$cut_offs_per_cluster == FALSE)
      shinyjs::toggle("curation_method",
                      condition = input$method == "Curate analytes based on data")
      shinyjs::toggle("analyte_list_div", 
                      condition = input$method == "Supply an analyte list")
      shinyjs::toggle("curation_based_on_data_div", 
                      condition = input$method == "Curate analytes based on data")
      shinyjs::toggle("ignore_samples",
                      condition = input$method == "Curate analytes based on data" & 
                      input$curation_method != "Per sample")
      shinyjs::toggle("groups_to_ignore",
                     condition = input$method == "Curate analytes based on data" & 
                     input$curation_method == "Per biological group")
      # Only enable button under right circumstances:
      shinyjs::toggleState(
        "curate_analytes", condition = all(
          is_truthy(passing_spectra()),
          length(input$qc_to_include) > 0,
          any(
            all(input$method == "Supply an analyte list", is_truthy(analyte_list())),
            all(
              input$method == "Curate analytes based on data",
              any(input$curation_method != "Per biological group", isTRUE(rv_resp$response))
            )
          )
        )
      )
      # Only ask for analyte curation per biological group when "Curate analytes based on data"
      shinyjs::toggle("curate_per_group",
                      condition = input$method == "Curate analytes based on data")
      # Only show drop-down menu to choose biological groups column when the 
      # user selects "Yes" when asked if curation should be done per group.
      shinyjs::toggle(
        "biogroup_column",
        condition = input$curation_method == "Per biological group"
      )
      shinyjs::toggle(
        "determine_groups_button",
        condition = input$curation_method == "Per biological group" & input$method == "Curate analytes based on data"
      )
      shinyjs::toggleState(
        "determine_groups_button",
        condition = input$biogroup_column != "" & input$curation_method == "Per biological group"
      )
      # Don't show cut-off option when doing curation per sample.
      shinyjs::toggle(
        "cut_off",
        condition = all(input$curation_method != "Per sample", input$cut_offs_per_cluster == FALSE)
      )
      # Toggle download button
      shinyjs::toggleState(
        "download", condition = is_truthy(with_analytes_to_include())
      )
    }, priority = 5)
    
    
    # The selection menu for input$ignore_samples is updated so that the choices
    # are sample_types and groups that are present in the data.
    observe({
      if ("group" %in% colnames(passing_spectra())) {
        choices <- c(paste(unique(passing_spectra()$sample_type), "samples"), 
                     paste(unique(passing_spectra()$group), "samples"))
      } else {
        choices <- c(paste(unique(passing_spectra()$sample_type), "samples"))
      }
      
      updateSelectizeInput(inputId = "ignore_samples",
                           choices = c(choices),
                           options = list(maxItems = length(choices) - 1))
    })
    
    
    # Update the selection menu for "Biological groups to ignore", based on the chosen columm.
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
      # Update the counter
      counter$count <- counter$count + 1
      # Show spinner
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
    without_samples_to_ignore <- reactive({
      req(input$method == "Curate analytes based on data")
      req(passing_spectra())
      if (input$curation_method == "Per sample") {
        passing_spectra()
      } else if (is_truthy(input$ignore_samples)) {
        throw_out_samples(passing_spectra = passing_spectra(), 
                          samples_to_ignore = input$ignore_samples)
      } else {
        passing_spectra()
      }
    })
    
    
    # checked_analytes is the same as without_samples_to_ignore, but with new 
    # columns describing whether an analyte fulfills all three quality criteria.
    checked_analytes <- reactive({
      req(input$method == "Curate analytes based on data")
      req(without_samples_to_ignore())
      
      if (data_type() == "LaCyTools data") {
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
    cut_offs <- reactive({
      clusters <- unique(passing_spectra()$cluster)
      values <- vector("list", length(clusters))
      names(values) <- clusters
      if (input$cut_offs_per_cluster == FALSE) {
        # Same cut-off for each cluster
        for (cluster in clusters) {
          values[[cluster]] <- input$cut_off
        }
      } else {
        # Separate cut-offs
        for (cluster in clusters) {
          values[[cluster]] <- input[[cluster]]
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
        
        #  Check if curation should be done per biological group
        if (isTRUE(rv_resp$response)) {
          # Curate per biological group
          curated_analytes <- checked_analytes() %>% 
            # Drop samples that don't belong to a biological group (e.g. pools, blanks)
            tidyr::drop_na(., input$biogroup_column) %>% 
            # Drop samples in biological groups that should be ignored
            dplyr::filter(., !.data[[input$biogroup_column]] %in% input$groups_to_ignore) %>%
            curate_analytes(., cut_offs(), input$biogroup_column)
        } else if (input$curation_method == "On all data") {
            curated_analytes <- curate_analytes(checked_analytes(), cut_offs())
        } else if (input$curation_method == "Per sample") {
            # Curation per sample
            curated_analytes <- checked_analytes() %>% 
              dplyr::rename(has_passed_analyte_curation = analyte_meets_criteria) %>% 
              dplyr::select(-failed_criteria)
        }
        
      } else if (input$method == "Supply an analyte list") {
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
    }) %>% bindEvent(input$curate_analytes)  # Execute code when button is pushed.
    
    
    
    # analyte_curated_data is a dataframe with the LaCyTools output of the
    # passing spectra, but with only the analytes that passed curation.
    analyte_curated_data <- reactive({
      req(curated_analytes())
      if (input$curation_method == "Per sample") {
        # Left join not necessary when curation is done per sample.
        curated_analytes()
      } else {
        dplyr::left_join(
          # This combines the info from curated_analytes (whether analytes pass or not)
          # with the LaCyTools output of the passing spectra.
          # The order here is important in the case of curation per biological group,
          # when one or more groups should be ignored. This order ensures that
          # only groups that should be taken into account are part of the resulting
          # dataframe (reversing the order would include all biological groups).
          curated_analytes(), passing_spectra()
        )
      } # bindEvent() below makes sure analyte_curated_data() is updated only
        # when curated_analytes() changes. Not when input$curation_method changes.
        # curated_analytes() changes when the "Perform analyte curation" button is pushed.
    }) %>% bindEvent(curated_analytes())
   
    
    
    
    # Create a vector with names of the clusters
    clusters <- reactive({
      req(analyte_curated_data())
      unique(analyte_curated_data()$cluster)
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
      # Remove spinner  
      shinybusy::remove_modal_spinner()
      # Return
      return(to_return)
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
