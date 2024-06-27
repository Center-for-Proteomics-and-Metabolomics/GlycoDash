#' quantitation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_quantitation_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML(paste0(
      "#", ns("box_header1"), " .awesome-checkbox {padding-top: 7px}",
      "#", ns("box_header1"), " .popover {max-width: 400px !important; color: #333}",
      "#", ns("box_header1"), " .fas {float: right; margin-right: 5px; font-size: 18px}",
      "#", ns("box_header1"), " .direct-chat-contacts {right: 0; background: #222d32!important}",
      "#", ns("box_header1"), " .btn {float: right; border-width: 0px; margin-right: 10px}",
      "#", ns("box_header1"), " .dropdown-menu {background: #333; right: -30px; left: auto; top: 28px;}",
      "#", ns("box1"), " .box-title {width: 100%}",
      "#", ns("box1"), " .dropdown {display: inline-block; float: right; width: 330px}",
      "#", ns("box_header2"), " .awesome-checkbox {padding-top: 7px}",
      "#", ns("box_header2"), " .popover {max-width: 400px !important; color: #333}",
      "#", ns("box_header2"), " .fas {float: right; margin-right: 5px; font-size: 18px}",
      "#", ns("box_header2"), " .direct-chat-contacts {right: 0; background: #222d32!important}",
      "#", ns("box_header2"), " .btn {float: right; border-width: 0px; margin-right: 10px}",
      "#", ns("box_header2"), " .dropdown-menu {background: #333; right: -30px; left: auto; top: 28px;}",
      "#", ns("box2"), " .box-title {width: 100%}",
      "#", ns("box2"), " .dropdown {display: inline-block; float: right; width: 330px}"
    ))),
    fluidPage(
      fluidRow(h1("IgG1 quantitation")),
      fluidRow(
        column(
          width = 5,
          shinydashboardPlus::box(
            id = ns("box1"),
            title = div(
              id = ns("box_header2"),
              "IgG1 quantitation using SILuMAb",
              icon("info-circle", class = "ml") %>% 
                bsplus::bs_embed_popover(
                  title = "Explanation",
                  content = HTML("
                  <p>
                  IgG1 quantitation is first performed based on two different 
                  peptides: the Fc glycopeptides and a proteotypic peptide
                  GPS[...]. It is also possible to perform the quantitation
                  based on only one of these peptides, using the checkboxes.
                  
                  <ul>
                  <li>For the glycopeptides, the summed intensity of the natural IgG1
                  glycopeptides is divided by the summed intensity of the SIL glycopeptides.
                  This ratio is then multiplied by the amount of SILuMAb in the sample.</li>
                  
                  <li>For GPS[...], the intensity of the natural peptide
                  is divided by the intensity of the SIL peptide,
                  after which the ratio is multiplied by the amount of SILuMAb in the sample.</li>
                  </ul>
                  
                  <p>
                  The reported amount of IgG1 in the plot and table below is the
                  median of the values calculated for the two different peptides.
                  When a sample is missing a value for one of the peptides, this
                  peptide is excluded from calculation of the median. The calculated 
                  amount of IgG1 is rounded to a whole number (ng).
                  "),
                  trigger = "hover",
                  placement = "right",
                  html = "true"
                )
            ),
            width = NULL,
            solidHeader = TRUE,
            status = "primary",
            # Text for when there is no SILuMAb
            div(
              id = ns("no_silumab"),
              strong("Your samples do not contain SILuMAb for IgG1 quantitation.\n\n"),
              style = "color:#0021B8; font-size: 16px"
            ),
            # Input for SILuMAb amounts.
            numericInput(
              ns("silumab_amount"),
              "Amount of SILuMAb per sample (ng):",
              value = 5, min = 0, max = NA
            ),
            # Checkboxes to include/exclude peptides
            shinyWidgets::awesomeCheckboxGroup(
              ns("chosen_peptides"),
              "Peptides to include in the calculation:",
              choices = c("Glycopeptides", "GPSVFPLAPSSK"),
              selected = c("Glycopeptides", "GPSVFPLAPSSK")
            ),
            # Option to exclude sample types from calculating correlations
            selectizeInput(
              ns("exclude_samples"),
              "Sample types to exclude from calculating the peptide correlations:",
              choices = c(""),
              multiple = TRUE
            ),
            # Button to quantify IgG1
            actionButton(
              ns("quantify_IgG1"),
              "Quantify IgG1"
            )
          )
        ),
        column(
          width = 7,
          shinydashboardPlus::box(
            id = ns("box2"),
            title = div(
              id = ns("box_header2"),
              "Peptide correlations",
              icon("info-circle", class = "ml") %>% 
                bsplus::bs_embed_popover(
                  title = "Explanation",
                  content = HTML("
                  <p>
                  When quantifying IgG1 based on different peptides,
                  the amounts of IgG1 calculated based on the glycopeptides
                  should correlate well to those calculated based on GPS[...].
                  When this is not the case, you may want to exclude one of 
                  the peptides from the quantitation.
                  
                  <p>
                  The Spearman correlation is calculated based
                  on IgG1 quantities rounded to a whole number of ng.
                  
                  <p>
                  Samples for which no quantity could be calculated 
                  because of missing values are not shown in the plot, and
                  are not used in calculating the correlation.
                  
                  <p>
                  The diagonal line is the line of equality (y = x), which is
                  useful for comparing two different quantitation methods.
                  "),
                  trigger = "hover",
                  placement = "left",
                  html = "true"
                )
            ),
            width = NULL,
            solidHeader = TRUE,
            status = "primary",
            tabsetPanel(id = ns("tabs"))
          )
        )
      ),
      fluidRow(
        column(
          width = 12,
          shinydashboard::box(
            title = "IgG1 quantitation plot",
            width = NULL,
            solidHeader = TRUE,
            status = "primary",
            shinycssloaders::withSpinner(plotly::plotlyOutput(ns("quantitation_plot"), width = "1350px"))
          )
        )
      ),
      fluidRow(
        shinydashboard::box(
          title = "View data with IgG1 quantities",
          width = 12,
          solidHeader = TRUE,
          status = "primary",
          DT::dataTableOutput(ns("data_table"))
        )
      )
    )
  )
  
}




#' quantitation Server Functions
#'
#' @noRd 
mod_quantitation_server <- function(id, quantitation_clusters,
                                    LaCyTools_summary,
                                    data_type,
                                    analyte_curated_data,
                                    results_normalization) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Toggle UI elements
    observe({
      shinyjs::toggle(
        id = "no_silumab",
        condition = !is_truthy(quantitation_clusters())
      )
      shinyjs::toggle(
        id = "silumab_amount",
        condition = is_truthy(quantitation_clusters())
      )
      shinyjs::toggle(
        id = "quantify_IgG1",
        condition = is_truthy(quantitation_clusters())
      )
      shinyjs::toggle(
        id = "chosen_peptides",
        condition = is_truthy(quantitation_clusters())
      )
      shinyjs::toggle(
        id = "exclude_samples",
        condition = is_truthy(quantitation_clusters()) 
      )
      shinyjs::toggleState(
        id = "quantify_IgG1",
        condition = all(
          is_truthy(quantitation_clusters()),
          is_truthy(results_normalization$normalized_data()),
          length(input$chosen_peptides) >= 1
        )
      )
    })
    
    
    # The selection menu for input$exlude_samples is updated so that the choices
    # are sample_types and groups that are present in the data.
    observe({
      if ("group" %in% colnames(results_normalization$normalized_data())) {
        options <- c(paste(unique(results_normalization$normalized_data()$sample_type), "samples"), 
                     paste(unique(results_normalization$normalized_data()$group), "samples"))
      } else {
        options <- c(paste(unique(results_normalization$normalized_data()$sample_type), "samples"))
      }
      
      updateSelectizeInput(inputId = "exclude_samples",
                           choices = c(options))
    })
    
    
    # Calculate ratios of peptides.
    IgG1_ratios <- reactive({
      req(is_truthy(quantitation_clusters()), results_normalization$normalized_data())
      IgG1_sum_intensities <- calculate_IgG1_sum_intensities(
        LaCyTools_summary(), data_type(), quantitation_clusters(), analyte_curated_data()
      )
      ratios <- calculate_IgG1_ratios(IgG1_sum_intensities, quantitation_clusters())
      return(ratios)
    })
    
    
    # Calculate IgG1 amounts based on chosen peptides.
    IgG1_amounts <- reactive({
      req(IgG1_ratios(), input$silumab_amount)
      calculate_IgG1_amounts(IgG1_ratios(), input$chosen_peptides, input$silumab_amount)
    }) %>% bindEvent(input$quantify_IgG1)
    
    
    # Create peptide correlation plots.
    r <- reactiveValues(created_tab_titles = vector("character"))
    observeEvent(IgG1_amounts(), {

      # Remove previously created tabs
      purrr::map(r$created_tab_titles, function(tab_title) {
        removeTab(inputId = "tabs", target = tab_title, session = session)
      })
      
      # Create vector for correlation plots, to show in the report.
      r$peptide_correlation_plots <- vector("list", length = length(input$chosen_peptides))
      
      if (length(input$chosen_peptides) > 1) {
        # Determine new tab IDs and titles
        tab_ids <- determine_tab_ids(input$chosen_peptides)
        tab_titles <- determine_tab_titles(input$chosen_peptides, tab_ids)
        
        # Store tab titles in reactiveValues vector
        r$created_tab_titles <- tab_titles
        
        # See if sample types should be excluded from the correlation
        samples_to_exclude <- stringr::str_remove(input$exclude_samples, " samples")
        
        if (!is_truthy(input$exclude_samples)) {
          to_plot <- IgG1_amounts()
        } else if ("group" %in% colnames(results_normalization$normalized_data())) {
          to_plot <- IgG1_amounts() %>% 
            dplyr::filter(!sample_type %in% samples_to_exclude) %>% 
            dplyr::filter(!group %in% samples_to_exclude)
        } else {
          to_plot <- IgG1_amounts() %>% 
            dplyr::filter(!sample_type %in% samples_to_exclude)
        }
        
        # Create tabs and plots.
        purrr::imap(tab_ids, function(tab_id, i) {
          plot <- plot_peptide_correlation(to_plot, tab_id, input$silumab_amount)
          
          # Add the plot to reactiveValues vector, to show it in the report later.
          r$peptide_correlation_plots[[i]] <- plot
          # Show the plot in UI
          output[[tab_id]] <- plotly::renderPlotly(plotly::ggplotly(plot, tooltip = "text"))
          appendTab(
            inputId = "tabs",
            select = TRUE,
            tab = tabPanel(
              title = tab_titles[[i]],
              plotly::plotlyOutput(ns(tab_id))
            )
          )
        })
      }
    })
    
    
    
    # Create a plot with quantitation results
    quantitation_plot <- reactive({
      req(IgG1_amounts())
      create_quantitation_plot(IgG1_amounts())
    })
    
    output$quantitation_plot <- plotly::renderPlotly({
      req(quantitation_plot())
      plotly_object <- plotly::ggplotly(quantitation_plot(), tooltip = "text") %>% 
        # plotly ignores "outlier.shape" so use function from utils
        GlycoDash::hide_outliers(.)
      return(plotly_object)
    })
    
    
    
    # Combine the calculated IgG1 quantities with normalized data.
    with_data <- reactive({
      req(IgG1_amounts())
      IgG1_quantities <- IgG1_amounts() %>% 
        dplyr::select(sample_name:sample_type, IgG1_median_amount) %>% 
        dplyr::rename(IgG1_quantity_ng = IgG1_median_amount) 
    
      dplyr::full_join(results_normalization$normalized_data_wide(),
                       IgG1_quantities) %>% 
        dplyr::relocate(IgG1_quantity_ng, .after = replicates)
    })
    
    output$data_table <- DT::renderDT({
      req(with_data())
      DT::datatable(data = with_data(),
                    options = list(scrollX = TRUE))
    })
    
    
    # Return the data and plots
    return(list(
      quantitation_data = with_data,
      silumab_amount = reactive(input$silumab_amount),
      chosen_peptides = reactive(input$chosen_peptides),
      quantitation_plot = quantitation_plot,
      peptide_correlation_plots = reactive(r$peptide_correlation_plots)
    ))
    
  })
}