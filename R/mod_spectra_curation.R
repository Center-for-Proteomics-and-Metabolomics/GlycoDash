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
              " .fa {float: right; margin-right: 5px; font-size: 18px}",
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
              title = div("Spectra curation cut-offs",
                          id = ns("box_header"),
                          shinyWidgets::dropdownButton(
                            tags$style(HTML(paste0(
                              "#",
                              ns("dropdown_content"),
                              " .fa {float: left}",
                              "#",
                              ns("dropdown_content"),
                              " .btn {float: none; border-width: 1px; width: 280px; margin: 10px}"
                            ))),
                            div(id = ns("dropdown_content"),
                                tags$style(HTML(paste0("#",
                                                       ns("div_central_tendency_measure"),
                                                       " .selectize-control{max-width: 200px;}"))),
                                p("The cut-off values are calculated using the following formula:"),
                                p("cut-off = mean (or median) + factor * standard-deviation"),
                                div(id = ns("div_central_tendency_measure"),
                                    selectInput(ns("central_tendency_measure"),
                                                "Choose whether the cut-off values should be calculated with the mean or with the median:",
                                                choices = c("Mean", "Median"))#,
                                    # selectInput(ns("variation_measure"),
                                    #             "Do you want to use percentiles instead of the standard deviation?",
                                    #             choices= c("Yes, use percentiles",
                                    #                        "No, use standard deviations"))
                                    ),
                                numericInput(ns("sd_factor"),
                                             "Choose what factor the standard deviation should be multiplied with:",
                                             value = 3,
                                             step = 1,
                                             min = 1,
                                             max = 3)
                                ),
                            icon = icon("gears",
                                        class = "ml"),
                            tooltip = shinyWidgets::tooltipOptions(placement = "top",
                                                                   title = "Settings"),
                            width = "330px",
                            size = "xs"
                          )),
              width = NULL,
              status = "primary",
              solidHeader = TRUE,
              column(
                width = 12,
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
              div(id = ns("cut_off_basis_Ig_data"),
                  shinydashboardPlus::box(
                    title = "Specific Ig samples",
                    width = 6,
                    status = "primary",
                    solidHeader = TRUE,
                    selectizeInput(ns("cut_off_basis_specific"),
                                   "Base the spectra curation cut-off for the specific Ig samples on:",
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
                        html = "true"),
                    numericInput(ns("cut_off_sum_intensity_specific"),
                                 "Enter a cut-off value for the sum intensity in the specific Ig samples:",
                                 value = ""),
                    numericInput(ns("cut_off_passing_proportion_specific"),
                                 "Enter a cut-off value for the percentage of passing analytes in the specific Ig samples:",
                                 value = "")
                  ),
                  shinydashboardPlus::box(
                    title = "Total Ig samples",
                    width = 6,
                    status = "primary",
                    solidHeader = TRUE,
                    selectizeInput(ns("cut_off_basis_total"),
                                   "Base the spectra curation cut-off for the total Ig samples on:",
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
                        html = "true"),
                    numericInput(ns("cut_off_sum_intensity_total"),
                                 "Enter a cut-off value for the sum intensity in the total Ig samples:",
                                 value = ""),
                    numericInput(ns("cut_off_passing_proportion_total"),
                                 "Enter a cut-off value for the percentage of passing analytes in the total Ig samples:",
                                 value = "")
                  )
              ),
              column(
                width = 12,
                shinyWidgets::materialSwitch(ns("switch_to_manual"),
                                             "Choose cut-off values manually instead",
                                             right = TRUE,
                                             status = "primary"),
                numericInput(ns("cut_off_sum_intensity"),
                             "Enter a cut-off value for the sum intensity:",
                             value = ""),
                numericInput(ns("cut_off_passing_proportion"),
                             "Enter a cut-off value for the percentage of passing analytes:",
                             value = ""),
                tabsetPanel(id = ns("tabs")),
                br(),
                actionButton(ns("button"),
                             "Perform spectra curation")
              )
            )
          )
        ),
        column(
          width = 4,
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
                     #tabindex = "0" #only needed for trigger = "focus"
                ) %>% 
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
                    trigger = "hover",
                    placement = "left",
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
          )
        )
      ),
      fluidRow(
        column(
          width = 12,
          shinydashboard::box(
            title = "Information on spectra curation",
            width = NULL,
            solidHeader = TRUE,
            status = "primary",
            plotly::plotlyOutput(ns("curated_spectra_plot")),
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
        )#,
        # column(
        #   width = 4,
        #   
        # )
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
    
    r <- reactiveValues()
    
    summary <- reactive({
      req(results_data_import$summary)
      results_data_import$summary()
    })
    
    # Hide the cut_off_basis selectInput when manual_cut_off is chosen:
    observe({
      shinyjs::toggle("cut_off_sum_intensity",
                      condition = all(is_truthy(input$switch_to_manual),
                                      results_data_import$Ig_data() == "No"))
      shinyjs::toggle("cut_off_passing_proportion",
                      condition = all(is_truthy(input$switch_to_manual),
                                      results_data_import$Ig_data() == "No"))
      
      shinyjs::toggle("cut_off_sum_intensity_specific",
                      condition = all(is_truthy(input$switch_to_manual),
                                      results_data_import$Ig_data() == "Yes"))
      
      shinyjs::toggle("cut_off_passing_proportion_specific",
                      condition = all(is_truthy(input$switch_to_manual),
                                      results_data_import$Ig_data() == "Yes"))
      
      shinyjs::toggle("cut_off_sum_intensity_total",
                      condition = all(is_truthy(input$switch_to_manual),
                                      results_data_import$Ig_data() == "Yes"))
      
      shinyjs::toggle("cut_off_passing_proportion_total",
                      condition = all(is_truthy(input$switch_to_manual),
                                      results_data_import$Ig_data() == "Yes"))
    })
    
    observe({
      shinyjs::toggle("cut_off_basis_Ig_data",
                      condition = results_data_import$Ig_data() == "Yes")
      shinyjs::toggle("cut_off_basis",
                      condition = results_data_import$Ig_data() == "No")
    })
    
    clusters <- reactive({
      req(summary())
      unique(summary()$cluster)
    })
    
    # Data with criteria checks for each analyte in each sample:
    checked_data <- reactive({
      req(summary(),
          input$sn,
          input$ipq)
      
     do_criteria_check(data = summary(),
                        min_ppm_deviation = input$mass_accuracy[1],
                        max_ppm_deviation = input$mass_accuracy[2],
                        max_ipq = input$ipq,
                        min_sn = input$sn)
      
    })
    
    # Analyte quality criteria checks summarized per cluster per sample: 
    summarized_checks <- reactive({
      req(checked_data())
      summarize_spectra_checks(checked_data())
    })
    
    cut_offs_based_on_samples <- reactive({
      req(summarized_checks(),
          input$sd_factor,
          any(all(results_data_import$Ig_data() == "No",
                  is_truthy(input$cut_off_basis)),
              all(results_data_import$Ig_data() == "Yes",
                  is_truthy(input$cut_off_basis_specific),
                  is_truthy(input$cut_off_basis_total))))
      
      if (results_data_import$Ig_data() == "Yes") {
        
        cut_offs_specific <- calculate_cut_offs(
          summarized_checks(),
          input$cut_off_basis_specific,
          sd_factor = input$sd_factor,
          central_tendency_measure = input$central_tendency_measure
        )
        
        cut_offs_total <- calculate_cut_offs(
          summarized_checks(),
          input$cut_off_basis_total,
          sd_factor = input$sd_factor,
          central_tendency_measure = input$central_tendency_measure
        )
        
        dplyr::full_join(cut_offs_specific,
                         cut_offs_total)
        
      } else {
        calculate_cut_offs(summarized_checks(),
                           input$cut_off_basis,
                           sd_factor = input$sd_factor,
                           central_tendency_measure = input$central_tendency_measure)
      }
    })
    
    manual_cut_offs <- reactive({
      
      if (results_data_import$Ig_data() == "Yes") {
        req(input$cut_off_sum_intensity_specific,
            input$cut_off_sum_intensity_total,
            input$cut_off_passing_proportion_specific,
            input$cut_off_passing_proportion_total)
        
        specific <- tibble::tibble(
          cut_off_sum_int = input$cut_off_sum_intensity_specific,
          cut_off_prop = input$cut_off_passing_proportion_specific,
          group = results_data_import$keyword_specific(),
          type = "manual"
        )
        
        total <- tibble::tibble(
          cut_off_sum_int = input$cut_off_sum_intensity_total,
          cut_off_prop = input$cut_off_passing_proportion_total,
          group = results_data_import$keyword_total(),
          type = "manual"
        )
        
        combined <- dplyr::full_join(specific, total)
        
        # Multiply rows of combined to get one row for each cluster
        cut_offs <- purrr::map_dfr(clusters(),
                                   function(this_cluster) {
                                     combined %>% 
                                       dplyr::mutate(cluster = this_cluster)
                                   })
        
      } else {
        req(input$cut_off_sum_intensity,
            input$cut_off_passing_proportion)
        
        cut_offs_wo_cluster <- data.frame(cut_off_sum_int = input$cut_off_sum_intensity,
                               cut_off_prop = input$cut_off_passing_proportion,
                               type = "manual")
        
        cut_offs <- purrr::map_dfr(clusters(),
                                   function(this_cluster) {
                                     cut_offs_wo_cluster %>% 
                                       dplyr::mutate(cluster = this_cluster)
                                   })
        
      }
      
      return(cut_offs)
    })
    
    cut_offs_to_use <- reactive({
      if (all(is_truthy(input$switch_to_manual),
              is_truthy(manual_cut_offs()))) {
        manual_cut_offs()
      } else {
        req(cut_offs_based_on_samples())
        cut_offs_based_on_samples()
      }
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
    
    observe({
      req(clusters())
      req(is_truthy(summarized_checks()))
      
      r$tab_contents <- rlang::set_names(clusters()) %>% 
        purrr::map(
          .,
          function(current_cluster) {
            mod_tab_cut_offs_server(
                id = current_cluster,
                selected_cluster = current_cluster,
                summarized_checks = reactive({
                  summarized_checks() %>%
                    dplyr::filter(cluster == current_cluster
                    )}),
                switch_to_manual = reactive({ input$switch_to_manual }),
                Ig_data = results_data_import$Ig_data,
                cut_offs_to_use = reactive({
                  cut_offs_to_use() %>% 
                    dplyr::filter(cluster == current_cluster)
                }),
                cut_offs_based_on_samples = reactive({
                  cut_offs_based_on_samples() %>% 
                    dplyr::filter(cluster == current_cluster)
                }),
                manual_cut_offs = reactive({
                  manual_cut_offs() %>% 
                    dplyr::filter(cluster == current_cluster)
                })
              )
            
          })
    })
    
    # The selection menu for input$cut_off_basis is updated so that the choices
    # are all combinations of sample_types and groups that are present in the
    # data.
    observe({
      req(summary())
      if (results_data_import$Ig_data() == "No") {
        options <- paste(unique(summary()$sample_type), "samples")
        
        updateSelectizeInput(inputId = "cut_off_basis",
                             choices = c("", options))
        
      } else {
        options_specific <- paste(results_data_import$keyword_specific(),
                                  unique(summary()$sample_type),
                                  "samples")
        updateSelectizeInput(inputId = "cut_off_basis_specific",
                             choices = c("", options_specific))
        
        options_total <- paste(results_data_import$keyword_total(),
                               unique(summary()$sample_type),
                               "samples")
        updateSelectizeInput(inputId = "cut_off_basis_total",
                             choices = c("", options_total))
      }
    })
    
    # Perform spectra curation:
    curated_data <- reactive({
      req(summary(),
          summarized_checks(),
          cut_offs_to_use())
      
      spectra_curated_data <- tryCatch(
        expr = { 
          curate_spectra(checked_data = checked_data(),
                         summarized_checks = summarized_checks(),
                         cut_offs = cut_offs_to_use())
        })
      
      return(spectra_curated_data)
    }) %>% bindEvent(input$button)
    
    observe({
      showNotification("Spectra curation has been performed.",
                       type = "message")
    }) %>% bindEvent(curated_data())
    
    passing_spectra <- reactive({
      req(curated_data())
      
      curated_data() %>% 
        dplyr::filter(passed_spectra_curation == TRUE) %>% 
        dplyr::select(-c(passed_spectra_curation, 
                         reason_for_failure,
                         failed_criteria,
                         passing_proportion, 
                         cut_off_prop,
                         cut_off_sum_int
                         # Leave 'sum_intensity' for the relative abundance
                         # calculation and leave 'criteria_check' for the
                         # analyte curation.
                         ))
    })
    
    
    output$failed_spectra_table <- DT::renderDataTable({
      
      req(curated_data())
      
      # for_table <- curated_data()%>% 
      #   dplyr::select(group:cut_off_sum_int) %>% 
      #   dplyr::distinct() %>% 
      #   dplyr::filter(passed_spectra_curation == FALSE)
      
      for_table <- curated_data()%>% 
        dplyr::select(1:cut_off_sum_int) %>% 
        dplyr::distinct() %>% 
        dplyr::filter(passed_spectra_curation == FALSE)
      
      DT::datatable(for_table,
                    options = list(scrollX = TRUE,
                                   filter = "top"))
      
    })
    
    output$failed_spectra_details <- DT::renderDataTable({
      
      req(curated_data())
      
      DT::datatable(curated_data()%>% 
                      dplyr::select(-(passing_proportion:cut_off_sum_int)) %>% 
                      dplyr::distinct() %>% 
                      dplyr::filter(passed_spectra_curation == FALSE),
                    options = list(scrollX = TRUE,
                                   searching = TRUE))
      
    })
    
    
    curated_spectra_plot <- reactive({
      req(curated_data())
      # Move this code to a function instead?
      
      # We need to change the values of passed_spectra_curation to what we want
      # to be shown in the legend, because plotly ignores legend labels that are
      # set with scale_ functions:
      my_data <- curated_data() %>% 
        dplyr::distinct(dplyr::across(tidyselect::any_of(c("group", 
                                                           "sample_type", 
                                                           "cluster", 
                                                           "sample_name", 
                                                           "passed_spectra_curation")))) %>% 
        dplyr::mutate(
          `Passed curation?` = dplyr::case_when(
            passed_spectra_curation == "TRUE" ~ "Yes",
            passed_spectra_curation == "FALSE" ~ "No"
          )) %>% 
        dplyr::group_by(dplyr::across(tidyselect::any_of(c("group",
                                                           "cluster",
                                                           "sample_type")))) %>% 
        dplyr::mutate(
          number_true = length(passed_spectra_curation[passed_spectra_curation == "TRUE"]),
          number_false = length(passed_spectra_curation[passed_spectra_curation == "FALSE"]),
          number = dplyr::case_when(
            passed_spectra_curation == "TRUE" ~ number_true,
            passed_spectra_curation == "FALSE" ~ number_false,
            TRUE ~ as.integer(NA)
          ),
          percentage = scales::label_percent(accuracy = 0.01)(number / dplyr::n())
        )
      
      plot <- my_data %>% 
        ggplot2::ggplot() +
        ggplot2::geom_bar(ggplot2::aes(x = sample_type, 
                                       fill = `Passed curation?`,
                                       text = paste(
                                         "Number of spectra:",
                                         number,
                                         "\nPercentage of spectra:",
                                         percentage
                                       )), 
                          position = "fill") +
        ggplot2::xlab("Sample type") +
        ggplot2::scale_y_continuous(labels = function(x) paste0(x * 100, "%"), 
                                    name = "Proportion of spectra (%)") +
        ggplot2::scale_fill_discrete(type = c("Yes" = "#3498DB",
                                              "No" = "#E74C3C")) +
        ggplot2::theme_classic() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                       strip.background = ggplot2::element_rect(fill = "#F6F6F8"),
                       panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=0.5))
      
      if (results_data_import$Ig_data() == "Yes") {
        plot +
          ggplot2::facet_wrap(cluster ~ group)
      } else {
        plot +
          ggplot2::facet_wrap(~ cluster)
      }
      
    })
    
    output$curated_spectra_plot <- plotly::renderPlotly({
      req(curated_spectra_plot())
      
      plotly_object <- plotly::ggplotly(curated_spectra_plot(), tooltip = "text")
      
      plotly_object <- facet_strip_bigger(plotly_object)
      
      plotly_object[["x"]][["layout"]][["annotations"]][[2]][["xshift"]] <- -50
      
      plotly_object[["x"]][["layout"]][["annotations"]][[1]][["yshift"]] <- -50
      
      return(plotly_object)
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
        data_to_download <- passing_spectra()
        switch(input$download_format,
               "R object" = save(data_to_download, 
                                 file = file),
               "Excel file" = writexl::write_xlsx(data_to_download, 
                                                  path = file))
      }
    )
    
    return(list(
      curated_spectra = passing_spectra,
      mass_acc = reactive({ input$mass_accuracy }),
      ipq = reactive({ input$ipq }),
      sn = reactive({ input$sn }),
      cut_off = reactive({input$cut_off_basis}),
      tab_contents = reactive({ r$tab_contents }),
      plot = curated_spectra_plot
    ))
    
  })
}
    
## To be copied in the UI
# mod_spectra_curation_ui("spectra_curation_ui_1")
    
## To be copied in the server
# mod_spectra_curation_server("spectra_curation_ui_1")
