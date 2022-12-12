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
          width = 4,
          div(
            id = ns("qc"),
            tags$style(HTML(paste0(
              "#",
              ns("box_header2"),
              " .awesome-checkbox {padding-top: 7px}",
              "#",
              ns("box_header2"),
              " .popover {max-width: 400px !important; color: #333}",
              "#",
              ns("qc"),
              " .box-title {width: 100%}",
              "#",
              ns("box_header2"),
              " .fas {float: right; margin-right: 5px; font-size: 18px}",
              "#",
              ns("box_header2"),
              " .direct-chat-contacts {right: 0; background: #222d32!important}",
              "#",
              ns("box_header2"),
              " .btn {float: right; border-width: 0px; margin-right: 2px}",
              "#",
              ns("qc"),
              " .dropdown {display: inline-block; float: right;}",
              "#",
              ns("box_header2"),
              " .dropdown-menu {background: #333; right: -33px; left: auto; top: 28px;}"
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
                    content = HTML(paste0(
                      tags$p(paste(
                        "The analyte quality criteria are used to curate the analytes",
                        "within each spectrum. To pass, an analyte needs to meet all",
                        "three criteria (for the mass accuracy, for the isotopic pattern", 
                        "quality (IPQ) and for the signal-to-noise ratio (S/N))."
                      )),
                      tags$p(paste(
                        "For each spectrum, the sum intensity of all passing",
                        "analytes as well as the percentage of passing analytes",
                        "are calculated. These values are shown in the", 
                        "interactive plot below."
                      ))
                    )),
                    trigger = "hover",
                    placement = "right",
                    html = "true",
                    container = "body"),
                shinyWidgets::dropdownButton(
                  shinyWidgets::awesomeCheckboxGroup(
                    ns("qc_to_include"),
                    "Which analyte quality criteria should be taken into account during spectra curation?",
                    choices = c("Mass accuracy",
                                "IPQ",
                                "S/N"),
                    selected = c("Mass accuracy",
                                 "IPQ",
                                 "S/N"),
                    status = "primary"),
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
              "#",
              ns("box_header"),
              " .awesome-checkbox {padding-top: 7px}",
              "#",
              ns("box_header"),
              " .popover {max-width: 400px !important; color: #333}",
              "#",
              ns("popover_cut_off"),
              " .box-title {width: 100%}",
              "#",
              ns("box_header"),
              " .fas {float: right; margin-right: 5px; font-size: 18px}",
              "#",
              ns("box_header"),
              " .direct-chat-contacts {right: 0; background: #222d32!important}",
              "#",
              ns("box_header"),
              " .btn {float: right; border-width: 0px; margin-right: 0px}",
              "#",
              ns("popover_cut_off"),
              " .dropdown {display: inline-block; float: right; width: 330px}",
              "#",
              ns("box_header"),
              " .dropdown-menu {background: #333; right: -10px; left: auto; top: 28px;}"
            ))
            ),
            shinydashboard::box(
              title = div("Calculate spectra curation cut-offs",
                          id = ns("box_header"),
                          icon("info-circle",
                               class = "ml",
                               #tabindex = "0" #only needed for trigger = "focus"
                          ) %>% 
                            bsplus::bs_embed_popover(
                              title = "Spectra curation methods",
                              content = HTML(paste0(
                                tags$p(
                                  tags$b("Negative control spectra:"),
                                  br(),
                                  paste(
                                    "Choose a group of samples that should not pass spectra",
                                    "curation (negative controls). The spectra curation", 
                                    "cut-offs will be set at a chosen percentile", 
                                    "of the sum intensities and percentages of passing", 
                                    "analytes in those negative control samples."
                                  )
                                ),
                                tags$p(
                                  tags$b("Percentiles"),
                                  br(),
                                  paste(
                                    "The cut-offs will be set at a chosen percentile", 
                                    "of the sum intensities and percentages of passing",
                                    "analytes in all spectra (except for in the sample",
                                    "types to exclude). For example, if the chosen",
                                    "percentile is 5, then the lowest 5% of all spectra will fail",
                                    "curation."
                                  )
                                )
                              )),
                              trigger = "hover",
                              placement = "left",
                              html = "true",
                              container = "body")
                          # shinyWidgets::dropdownButton(
                          #   tags$style(HTML(paste0(
                          #     "#",
                          #     ns("dropdown_content"),
                          #     " .fas {float: left}",
                          #     "#",
                          #     ns("dropdown_content"),
                          #     " .btn {float: none; border-width: 1px; width: 280px; margin: 10px}"
                          #   ))),
                          #   div(id = ns("dropdown_content"),
                          #       tags$style(HTML(paste0("#",
                          #                              ns("div_central_tendency_measure"),
                          #                              " .selectize-control{max-width: 200px;}"))),
                          #       p("The cut-off values are calculated using the following formula:"),
                          #       p("cut-off = mean (or median) + factor * standard-deviation"),
                          #       div(id = ns("div_central_tendency_measure"),
                          #           selectInput(ns("central_tendency_measure"),
                          #                       "Choose whether the cut-off values should be calculated with the mean or with the median:",
                          #                       choices = c("Mean", "Median"))
                          #           ),
                          #       numericInput(ns("sd_factor"),
                          #                    "Choose what factor the standard deviation should be multiplied with:",
                          #                    value = 3,
                          #                    step = 1,
                          #                    min = 1,
                          #                    max = 3)
                          #       ),
                          #   icon = icon("gears",
                          #               class = "ml"),
                          #   tooltip = shinyWidgets::tooltipOptions(placement = "top",
                          #                                          title = "Settings"),
                          #   width = "330px",
                          #   size = "xs"
                          # )
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
                    label = "Treat uncalibrated spectra as missing values, not zeroes.",
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
                         "Perform spectra curation")
          )
        ) 
      ),
      fluidRow(
        column(
          width = 12,
          shinydashboard::box(
            title = "View spectra curation results",
            width = NULL,
            solidHeader = TRUE,
            status = "primary",
            plotly::plotlyOutput(ns("curated_spectra_plot")),
            tabsetPanel(id = ns("more_than_4_clusters")),
            br(),
            tabsetPanel(
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
          width = 12,
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
    
    summary <- reactive({
      req(results_data_import$summary())
      results_data_import$summary()
    })
    
    # Data with criteria checks for each analyte in each sample:
    checked_data <- reactive({
      req(summary(),
          input$sn,
          input$ipq
      )
      check_analyte_quality_criteria(my_data = summary(),
                        min_ppm_deviation = input$mass_accuracy[1],
                        max_ppm_deviation = input$mass_accuracy[2],
                        max_ipq = input$ipq,
                        min_sn = input$sn,
                        criteria_to_consider = input$qc_to_include#,
                        #uncalibrated_as_NA = input$uncalibrated_as_na
                        )
    })
    
    checked_data_uncalibrated <- reactive({
      req(checked_data())
      
      if (!input$uncalibrated_as_na) {
        dplyr::mutate(
          checked_data(),
          analyte_meets_criteria = tidyr::replace_na(analyte_meets_criteria,
                                                     FALSE)
        )
      } else {
        checked_data()
      }
    })
    
    # Analyte quality criteria checks summarized per cluster per sample: 
    summarized_checks <- reactive({
      req(checked_data_uncalibrated())
      summarize_spectra_checks(checked_data_uncalibrated())
    })
    
    observe({
      shinyjs::toggle("controls_module",
                      condition = input$curation_method == "Negative control spectra")
      
      shinyjs::toggle("percentiles_module",
                      condition = input$curation_method == "Percentiles")
    })
    
    #TODO: hide results box (and button?) if Skip spectra curation is chosen
    
    cut_offs_based_on_controls <- mod_curate_based_on_controls_server(
      "curate_based_on_controls_ui_1",
      results_data_import = results_data_import,
      summarized_checks = summarized_checks,
      uncalibrated_as_NA = reactive({ input$uncalibrated_as_na })  
    )
    
    cut_offs_based_on_percentiles <- mod_curate_based_on_percentiles_server(
      "curate_based_on_percentiles_ui_1",
      is_Ig_data = results_data_import$Ig_data,
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
      req(summary())
      unique(summary()$cluster)
    })
    
    observeEvent(clusters(), {
      # Remove tabs in case they have been created before. Still not ideal cause
      # if cluster names are changed then the old tabs won't be removed
      purrr::map(clusters(),
                 function(cluster) {
                   removeTab("tabs",
                             target = cluster)
                 })

      # Create one tab for each cluster:
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
      req(clusters(),
          summarized_checks())
      
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
              Ig_data = results_data_import$Ig_data,
              keyword_specific = results_data_import$keyword_specific,
              keyword_total = results_data_import$keyword_total,
              calculated_cut_offs = reactive({
                calculated_cut_offs() %>% 
                  dplyr::filter(cluster == current_cluster)
              })
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
    
    observe({
      shinyjs::toggleState(
        id = "button",
        condition = all(is_truthy(checked_data()),
                        is_truthy(summarized_checks()),
                        is_truthy(cut_offs_to_use_all_clusters()))
      )
    })
    
    # Perform spectra curation:
    curated_data <- reactive({
      curate_spectra(checked_data = checked_data(),
                     summarized_checks = summarized_checks(),
                     cut_offs = cut_offs_to_use_all_clusters(),
                     uncalibrated_as_NA = input$uncalibrated_as_na)
    }) %>% bindEvent(input$button)
    
    observe({
      showNotification("Spectra curation has been performed.",
                       type = "message")
    }) %>% bindEvent(curated_data())
    
    passing_spectra <- reactive({
      req(curated_data())
      
      curated_data() %>% 
        dplyr::filter(has_passed_spectra_curation == TRUE) %>% 
        dplyr::select(-c(reason_for_failure,
                         passing_analyte_percentage,
                         cut_off_sum_intensity,
                         cut_off_passing_analyte_percentage,
                         curation_method,
                         analyte_meets_criteria,
                         failed_criteria,
                         sample_type_list
                         # Leave 'sum_intensity' for the relative abundance
                         # calculation and leave 'criteria_check' for the
                         # analyte curation.
                         ))
    })
    
    # TODO: fix which columns are shown in the tables
    
    output$failed_spectra_table <- DT::renderDataTable({
      req(curated_data())
      
      for_table <- curated_data()%>% 
        dplyr::select(1:cut_off_passing_analyte_percentage) %>% 
        dplyr::distinct() %>% 
        dplyr::filter(has_passed_spectra_curation == FALSE)
      
      DT::datatable(for_table,
                    options = list(scrollX = TRUE,
                                   filter = "top"))
    })
    
    output$failed_spectra_details <- DT::renderDataTable({
      req(curated_data())
      
      DT::datatable(curated_data() %>% 
                      dplyr::select(-(passing_analyte_percentage:replicates)) %>% 
                      dplyr::distinct() %>% 
                      dplyr::filter(has_passed_spectra_curation == FALSE),
                    options = list(scrollX = TRUE,
                                   searching = TRUE))
      
    })
    
    
    curated_spectra_plot <- reactive({
      req(curated_data(),
          length(unique(clusters())) <= 4)
      
      plot_spectra_curation_results(curated_data(),
                                    results_data_import$Ig_data())
      
    })
    
    observe({
      req(clusters())
      shinyjs::toggle(id = "more_than_4_clusters",
                      condition = length(clusters()) > 4)
      shinyjs::toggle(id = "curated_spectra_plot",
                      condition = length(clusters()) <= 4)
    })
    
    observe({
      req(length(clusters()) > 4,
          curated_data())
      
      # Remove tabs in case they have been created before. Still not ideal cause
      # if cluster names are changed then the old tabs won't be removed
      purrr::map(clusters(),
                 function(current_cluster) {
                   removeTab("more_than_4_clusters",
                             target = current_cluster)
                 })
      
      purrr::map(clusters(),
                 function(current_cluster) {
                   appendTab("more_than_4_clusters",
                             #select = TRUE, #leads to some plotlys being too narrow
                             tabPanel(
                               title = current_cluster,
                               mod_tab_curated_spectra_plot_ui(
                                 # I already use ns(cluster) somewhere else in
                                 # mod_spectra_curation, so I need to use a
                                 # different namespace here:
                                 ns(paste0(current_cluster, "_results")))
                             )
                   )
                 })
    })
    
    observe({
      req(length(clusters()) > 4,
          curated_data())
      
      r$curated_spectra_plots <- rlang::set_names(clusters()) %>% 
        purrr::map(
        .,
        function(current_cluster) {
          mod_tab_curated_spectra_plot_server(
            id = paste0(current_cluster, "_results"), 
            curated_data = reactive({ 
              curated_data() %>% 
                dplyr::filter(cluster == current_cluster) 
            }),
            is_Ig_data = results_data_import$Ig_data)
        })
    })
    
    
    output$curated_spectra_plot <- plotly::renderPlotly({
      req(curated_spectra_plot())
      
      plotly_object <- plotly::ggplotly(curated_spectra_plot(), tooltip = "text")
      plotly_object <- facet_strip_bigger(plotly_object)
      plotly_object <- change_axis_title_distance(plotly_object)
      
      return(plotly_object)
    })
    
    to_return <- reactive({
      if (input$curation_method == "Skip spectra curation") {
        dplyr::full_join(req(checked_data()),
                         req(summarized_checks())) %>% 
          dplyr::select(-c(failed_criteria,
                           passing_analyte_percentage,
                           analyte_meets_criteria
                           # Leave 'sum_intensity' for the relative abundance
                           # calculation and leave 'criteria_check' for the
                           # analyte curation.
          ))
      } else {
        req(passing_spectra())
      }
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
        data_to_download <- to_return()
        switch(input$download_format,
               "R object" = save(data_to_download, 
                                 file = file),
               "Excel file" = writexl::write_xlsx(data_to_download, 
                                                  path = file))
      }
    )
    
    return(list(
      curated_spectra = to_return,
      mass_acc = reactive({ input$mass_accuracy }),
      ipq = reactive({ input$ipq }),
      sn = reactive({ input$sn }),
      included_qc = reactive({ input$qc_to_include }),
      uncalibrated_as_NA = reactive({ input$uncalibrated_as_na }),
      cut_off = reactive({input$cut_off_basis}),
      tab_contents = reactive({ r$tab_contents }),
      curated_spectra_plots = reactive({ r$curated_spectra_plots }),
      plot = curated_spectra_plot
    ))
    
  })
}
    
## To be copied in the UI
# mod_spectra_curation_ui("spectra_curation_ui_1")
    
## To be copied in the server
# mod_spectra_curation_server("spectra_curation_ui_1")
