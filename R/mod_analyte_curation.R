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
    fluidPage(
      fluidRow(
        h1("Analyte curation")
      ),
      fluidRow(
        column(
          width = 6,
          # Box with settings for the analyte curation
          shinydashboard::box(
            title = "Method for analyte curation",
            width = NULL,
            solidHeader = TRUE,
            status = "primary",
            # Ask user to choose a method for analyte curation
            selectInput(ns("method"), 
                        "Choose method for analyte curation:",
                        choices = c("Curate analytes based on data",
                                    "Supply an analyte list")),
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
                    content = HTML(paste(
                      tags$b("Excel file:"),
                      tags$p(paste(
                        "The file should consist of one column with the names of the",
                        "analytes that you want to keep. A column name is not required."
                      )),
                      tags$b("R object:"),
                      tags$p(paste(
                        "The R object should be a character vector or a list of",
                        "character strings (not a dataframe!) with the names of",
                        "the analytes that you want to keep."
                      ))
                    )),
                    html = "true",
                    trigger = "hover",
                    placement = "right")
            ),
            tags$style(
              HTML(paste0("#",
                          ns("curation_based_on_data"),
                          " .popover {width: 400px;}"))
            ),
            # Ask user whether analyte curation should be done per biological group
            shinyWidgets::awesomeRadio(
              ns("curate_per_group"),
              "Should analyte curation be performed per biological group?",
              choices = c("Yes", "No"),
              selected = "No"
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
              )
            ),
            
            div(
              id = ns("curation_based_on_data_div"),
              selectizeInput(ns("ignore_samples"),
                             HTML("<br/>Sample types to ignore regarding analyte curation:"),
                             choices = c("Total", "Blanks", "Negative controls"),
                             multiple = TRUE) %>% 
                bsplus::bs_embed_popover(
                  title = "Explanation",
                  content = HTML(paste0(
                    tags$p(paste(
                      "Analytes are curated based on the percentage of spectra",
                      "in which they pass the analyte quality criteria (go to \"",
                      "Spectra curation\" to choose these criteria)."
                    )),
                    tags$p(paste(
                      "However, some spectra (e.g. blanks, standards or total", 
                      "Ig samples) should not be included in this assesment."
                    )),
                    tags$p(paste(
                      "Select here which samples should be ignored with regards",
                      "to analyte curation."
                    ))
                  )),
                  placement = "right",
                  trigger = "hover",
                  html = "true"),
              numericInput(ns("cut_off"), "Cut-off (%)", value = 25) %>% 
                bsplus::bs_embed_popover(
                  title = "Explanation",
                  content = HTML(paste0(
                    tags$p(paste(
                      "Choose the percentage of spectra in which an analyte",
                      "needs to fulfill the quality criteria in order to pass", 
                      "analyte curation."
                    ))
                  )),
                  placement = "right",
                  trigger = "hover",
                  html = "true")
            ),
            actionButton(ns("curate_analytes"), 
                         "Perform analyte curation"),
           
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
            div(
              strong("Attention:"),
              paste(
                "in the case of multiple clusters, you must click on each tab",
                "below in order for the data to be correctly processed."),
              style = "color:#0021B8"
            ),
            br(),
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
mod_analyte_curation_server <- function(id, results_spectra_curation, biogroup_cols){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    passing_spectra <- reactive({
      req(results_spectra_curation$passing_spectra())
      results_spectra_curation$passing_spectra()
    })
    
    # Create reactiveValues. 
    # Below, rv_resp$response is created when analyte curation is performed per biological group.
    rv_resp <- reactiveValues(response = NULL)
    
    # Show potential column names with biological groups
    observe({
      req(biogroup_cols())
      updateSelectInput(
        inputId = "biogroup_column",
        choices = c("", biogroup_cols())
      )
    })
    
    # Show pop-up with detected biological groups and ask for confirmation.
    observe({
      req(input$curate_per_group == "Yes")
      shinyalert::shinyalert(
        inputId = "popup",
        html = TRUE,
        text = tagList(
          "The following biological groups were detected:",
          DT::dataTableOutput(ns("popup_table"))
        ),
        size = "m",
        confirmButtonText = "Accept these as biological groups",
        showCancelButton = TRUE,
        cancelButtonText = "Cancel",
        confirmButtonCol = "#3c8dbc",
        callbackR = function(response){
          rv_resp$response <- response  # TRUE or FALSE, depending on whether user accepts biol. groups.
        }
      )
    }) %>% bindEvent(input$determine_groups_button) # Show pop-up window when button is clicked.
    
 
    # The data table that is shown in the pop-up
    output$popup_table <- DT::renderDataTable({
      biological_groups <- data.frame(unique(passing_spectra()[input$biogroup_column])) %>% tidyr::drop_na()
      DT::datatable(biological_groups,
                    options = list(
                      scrollY = "150px",
                      paging = FALSE,
                      searching = FALSE,
                      columnDefs = list(
                        list(
                          className = 'dt-center',
                          targets = "_all"))),
                    rownames = FALSE
      )
    })

    
    # Show and hide UI based on the chosen method:
    observe({
      shinyjs::toggle("analyte_list_div", 
                      condition = input$method == "Supply an analyte list")
      shinyjs::toggle("curation_based_on_data_div", 
                      condition = input$method == "Curate analytes based on data")
      shinyjs::toggle("ignore_samples",
                      condition = input$method == "Curate analytes based on data" & input$curate_per_group == "No")
      shinyjs::toggle("groups_to_ignore",
                     condition = input$method == "Curate analytes based on data" & input$curate_per_group == "Yes")
      # Only enable button under right circumstances:
      shinyjs::toggleState("curate_analytes", 
                           condition = 
                             (input$method == "Supply an analyte list" & 
                             is_truthy(analyte_list())) | 
                             ((input$method == "Curate analytes based on data") &
                             (input$curate_per_group == "No" | isTRUE(rv_resp$response))))
      # Only ask for analyte curation per biological group when "Curate analytes based on data"
      shinyjs::toggle("curate_per_group",
                      condition = input$method == "Curate analytes based on data")
      # Only show drop-down menu to choose biological groups column when the 
      # user selects "Yes" when asked if curation should be done per group.
      shinyjs::toggle(
        "biogroup_column",
        condition = input$curate_per_group == "Yes"
      )
      shinyjs::toggle(
        "determine_groups_button",
        condition = input$curate_per_group == "Yes" & input$method == "Curate analytes based on data"
      )
      shinyjs::toggleState(
        "determine_groups_button",
        condition = input$biogroup_column != ""
      )
    }, priority = 10)
    
    
    # The selection menu for input$ignore_samples is updated so that the choices
    # are sample_types and groups that are present in the data.
    observe({
      if ("group" %in% colnames(passing_spectra())) {
        options <- c(paste(unique(passing_spectra()$sample_type), "samples"), 
                     paste(unique(passing_spectra()$group), "samples"))
      } else {
        options <- c(paste(unique(passing_spectra()$sample_type), "samples"))
      }
      
      updateSelectizeInput(inputId = "ignore_samples",
                           choices = c("", options))
    })
    
    
    # Update the selection menu for "Biological groups to ignore", based on the chosen columm.
    observe({
      req(is_truthy(input$biogroup_column))
      options <- dplyr::pull(
        unique(passing_spectra()[input$biogroup_column]) %>% 
        tidyr::drop_na()
      )
      updateSelectizeInput(inputId = "groups_to_ignore", choices = c("", options))
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
    
    without_samples_to_ignore <- reactive({
      req(input$method == "Curate analytes based on data")
      req(passing_spectra())
      
      if (is_truthy(input$ignore_samples)) {
        throw_out_samples(passing_spectra = passing_spectra(),
                          samples_to_ignore = input$ignore_samples)
      } else {
        passing_spectra()
      }
    })
    
    checked_analytes <- reactive({
      req(input$method == "Curate analytes based on data")
      req(without_samples_to_ignore())
      
      check_analyte_quality_criteria(
        without_samples_to_ignore(),
        min_ppm_deviation = results_spectra_curation$mass_acc()[1],
        max_ppm_deviation = results_spectra_curation$mass_acc()[2],
        max_ipq = results_spectra_curation$ipq(),
        min_sn = results_spectra_curation$sn(),
        criteria_to_consider = c("IPQ", "S/N", "Mass accuracy")
      )
    })
    
    
    # Curate the analytes
    curated_analytes <- reactive({
      
      if (input$method == "Curate analytes based on data") {
        req(checked_analytes())
        #  Check if curation should be done per biological group
        if (isTRUE(rv_resp$response)) {
          # Curate per biological group
          curated_analytes <- checked_analytes() %>% 
            # Drop samples that don't belong to a biological group (e.g. pools, blanks)
            tidyr::drop_na(., input$biogroup_column) %>% 
            # Drop samples in biological groups that should be ignored
            dplyr::filter(., !.data[[input$biogroup_column]] %in% input$groups_to_ignore) %>%  # Thanks ChatGPT
            curate_analytes(., input$cut_off, input$biogroup_column)
        } else {
          curated_analytes <- curate_analytes(checked_analytes(), input$cut_off)
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
    })

    analyte_curated_data <- reactive({
      req(curated_analytes())
      
      dplyr::left_join(curated_analytes(), 
                       passing_spectra())
    }) %>% bindEvent(input$curate_analytes)
    
    observe({
      
      showNotification(ui = paste("Analyte curation has been performed",
                                  ifelse(
                                    input$method == "Curate analytes based on data",
                                    "based on the data.",
                                    "based on the analyte list."
                                  )),
                       type = "message")
    }) %>% bindEvent(analyte_curated_data())
   
    clusters <- reactive({
      req(analyte_curated_data())
      unique(analyte_curated_data()$cluster)
    })
    
    r <- reactiveValues(mod_results = list(),
                        created_cluster_tabs = vector())
    
    # Save which tabs have been created in r$created_cluster_tabs so that they
    # can still be removed after clusters() changes:
    observe({
      req(clusters())
      r$created_cluster_tabs <- union(isolate({ r$created_cluster_tabs }),
                                      clusters())
    })
    
    observe({
      req(clusters(),
          r$created_cluster_tabs)
      
      # Remove tabs that have been created before:
      purrr::map(r$created_cluster_tabs,
                function(cluster) {
                  removeTab("tabs",
                            target = cluster)
                  # Trying to remove a tab that doesn't exist does not result in an error
                })
      
      # Create one tab for each cluster in without_samples_to_ignore:
      purrr::map(clusters(),
                 function(cluster) {
                   shinyjs::delay(
                     ms = 2000,
                     expr = {
                       appendTab("tabs",
                                 select = TRUE,
                                 session = session,
                                 tabPanel(
                                   title = cluster,
                                   mod_tab_curated_analytes_ui(ns(cluster))
                                 ))
                     })
                   
                 })
    })
    
    
    info <- list(
      curated_analytes = curated_analytes,
      cut_off = reactive({ input$cut_off }),
      analyte_curated_data = analyte_curated_data,
      method = reactive({ input$method })
    )
    
    
    observe({
      req(clusters())
      req(info$analyte_curated_data())
      
      r$mod_results <- purrr::set_names(clusters()) %>% 
        purrr::map(
          .,
          function(cluster) {
            mod_tab_curated_analytes_server(id = cluster,
                                            info = info,
                                            cluster = cluster,
                                            biogroup_column = input$biogroup_column
                                            )
          })
    },
    priority = 10)
    
    with_analytes_to_include <- reactive({
      req(passing_spectra(),
          !rlang::is_empty(r$mod_results),
          all(purrr::map_lgl(r$mod_results,
                         ~ is_truthy(.x$analytes_to_include()))))
      
      purrr::imap(r$mod_results,
                  function(results, current_cluster) {
                    data_current_cluster <- passing_spectra() %>% 
                      dplyr::filter(cluster == current_cluster)
                    
                    dplyr::left_join(results$analytes_to_include(),
                                     data_current_cluster)
                      
                  }) %>% 
        purrr::reduce(dplyr::full_join)
      
    })
    
    # Make downloading analyte_curated_data possible:
    output$download <- downloadHandler(
      filename = function() {
        todays_date <- paste0(stringr::str_replace_all(Sys.Date(),
                                                       pattern = "-",
                                                       replacement = ""))
        switch(input$download_format,
               "R object" = paste0(todays_date, "_curated_analytes.rds"),
               "Excel file" = paste0(todays_date, "_curated_analytes.xlsx"))
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
    
    
    # Show a spinner while analyte curation is being performed.
    # It is removed in "mod_tab_curated_analytes.R"
    observeEvent(input$curate_analytes, {
      shinybusy::show_modal_spinner(
        spin = "cube-grid", color = "#0275D8", 
        text = HTML("<br/><strong>Curating analytes...")
      )
    }, priority = 20)
    
    
    
    return(list(
      analyte_curated_data = with_analytes_to_include,
      method = reactive({ input$method }),
      ignore_samples = reactive({ input$ignore_samples }),
      cut_off_percentage = reactive({ input$cut_off }),
      analyte_list = reactive({ input$analyte_list$name }),
      objects = reactive({ r$mod_results })
    ))
 
  })
}
