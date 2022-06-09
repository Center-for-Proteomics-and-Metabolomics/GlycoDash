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
            shinydashboard::box(
              title = "Spectra curation cut-offs",
              width = NULL,
              status = "primary",
              solidHeader = TRUE,
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
                  html = "true"),
              shinyWidgets::materialSwitch(ns("switch_to_manual"),
                                           "Choose cut-off values manually instead",
                                           right = TRUE,
                                           status = "primary"),
              numericInput(ns("cut_off_sum_intensity"),
                           "Choose a cut-off value for the sum intensity:",
                           value = ""),
              numericInput(ns("cut_off_passing_proportion"),
                           "Choose a cut-off value for the percentage of passing analytes:",
                           value = ""),
              tabsetPanel(id = ns("tabs")),
              br(),
              actionButton(ns("button"),
                           "Perform spectra curation")
            )
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
            br(),
            plotly::plotlyOutput(ns("cut_off_plot")),
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
    
    x <- reactiveValues()
    
    summary <- reactive({
      req(results_data_import$summary)
      results_data_import$summary()
    })
    
    # Hide the cut_off_basis selectInput when manual_cut_off is chosen:
    observe({
      shinyjs::toggle("cut_off_sum_intensity",
                      condition = is_truthy(input$switch_to_manual))
      shinyjs::toggle("cut_off_passing_proportion",
                      condition = is_truthy(input$switch_to_manual))
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
    
    observe({
      req(clusters())
      req(checked_spectra())
      purrr::map(
        clusters(),
        function(current_cluster) {
          checked_spectra_filtered <- checked_spectra() %>% 
            dplyr::filter(cluster == current_cluster)
          
          mod_tab_cut_offs_server(id = current_cluster,
                                  selected_cluster = current_cluster,
                                  checked_spectra = reactive({checked_spectra_filtered}),
                                  cut_offs_based_on_samples = cut_offs_based_on_samples,
                                  cut_off_basis = reactive({input$cut_off_basis}),
                                  manual_cut_offs = manual_cut_offs,
                                  switch_to_manual = reactive({input$switch_to_manual}))
        })
    })
    
    
    # The selection menu for input$cut_off_basis is updated so that the choices
    # are all combinations of sample_types and groups that are present in the
    # data.
    observe({
      req(summary())
      if (results_data_import$Ig_data() == "Yes") {
        combinations <- expand.grid(sample_type = unique(summary()$sample_type),
                                    group = unique(summary()$group))
        options <- purrr::pmap_chr(combinations,
                                   function(sample_type, group) {
                                     paste(group,
                                           sample_type,
                                           "samples")
                                   })
      } else {
        options <- paste("all", unique(summary()$sample_type), "samples")
      }
      
      updateSelectizeInput(inputId = "cut_off_basis",
                           choices = c("", options))
    })
    
    checked_spectra <- reactive({
      req(summary(),
          input$sn,
          input$ipq)
      check_spectra(data = summary(),
                    min_ppm_deviation = input$mass_accuracy[1],
                    max_ppm_deviation = input$mass_accuracy[2],
                    max_ipq = input$ipq,
                    min_sn = input$sn)
    })
    
    cut_offs_based_on_samples <- reactive({
      req(checked_spectra(),
          input$cut_off_basis)
      
      calculate_cut_offs(checked_spectra(),
                         input$cut_off_basis) %>% 
        dplyr::select(cluster,
                      cut_off_prop,
                      cut_off_sum_int)
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
    
    manual_cut_offs <- reactive({
      req(input$cut_off_sum_intensity,
          input$cut_off_passing_proportion)
      data.frame(cut_off_sum_int = input$cut_off_sum_intensity,
                 cut_off_prop = input$cut_off_passing_proportion)
    })
    
    cut_offs_table <- reactive({
      req(checked_spectra())
      
      calculate_cut_offs_per_type(checked_spectra())
    })
    
    # Perform spectra curation:
    spectra_curation <- reactive({
      req(summary(),
          checked_spectra(),
          cut_offs_to_use())
      
      spectra_curation <- tryCatch(
        expr = { 
          print("check1")
          curate_spectra(data = summary(),
                         spectra_check = checked_spectra(),
                         cut_offs = cut_offs_to_use())
        })
      return(spectra_curation)
    }) %>% bindEvent(input$button)
    
    observe({
      showNotification("Spectra curation has been performed.",
                       type = "message")
    }) %>% bindEvent(spectra_curation())
    
    passing_spectra <- reactive({
      req(spectra_curation())
      
      spectra_curation()$curated_data %>% 
        dplyr::filter(passed_spectra_curation == TRUE) %>% 
        dplyr::select(-passed_spectra_curation)
    })
    
    curated_spectra_plot <- reactive({
      req(spectra_curation())
      # Move this code to a function instead?
      plot <- spectra_curation()$curated_data %>% 
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
      
      if (results_data_import$Ig_data() == "Yes") {
        plot +
          ggplot2::facet_wrap(cluster ~ group)
      } else {
        plot +
          ggplot2::facet_wrap(~ cluster)
      }
      
    })
    
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
        data_to_download <- passing_spectra()
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
      req(spectra_curation(),
          input$cut_off_basis)
      create_cut_off_plot(spectra_check = spectra_curation()$spectra_check,
                          cut_off_basis = input$cut_off_basis)
    })

    output$cut_off_plot <- plotly::renderPlotly({
      req(cut_off_plot())
      plot <- cut_off_plot() +
        ggplot2::coord_cartesian(xlim = ranges$x,
                                 ylim = ranges$y,
                                 expand = FALSE) +
        ggplot2::theme(axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 20)))
      
      plotly <- plotly::ggplotly(plot, tooltip = "text")
      
      plotly[["x"]][["layout"]][["margin"]][["l"]] <- plotly[["layout"]][["margin"]][["l"]] + 20
      
      plotly <- facet_strip_bigger(plotly)
      
    })
    
    return(list(
      curated_spectra = reactive({passing_spectra()}),
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
