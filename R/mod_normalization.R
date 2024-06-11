#' normalization UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_normalization_ui <- function(id){
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
        h1("Normalized data")
      ),
      fluidRow(
        shinydashboard::box(
          title = "View normalized data",
          width = 12,
          solidHeader = TRUE,
          status = "primary",
          DT::dataTableOutput(ns("data_table"))
        )
      ),
      fluidRow(
        shinydashboardPlus::box(
          id = ns("box"),
          title = div(
            id = ns("box_header"),
            "Visualize normalized data",
            shinyWidgets::dropdownButton(
              colourpicker::colourInput(
                ns("color_low"),
                "Lowest value color",
                value = "blue",
                returnName = TRUE,
                palette = "limited",
                closeOnClick = TRUE
              ),
              colourpicker::colourInput(
                ns("color_mid"),
                "Middle value color",
                value = "white",
                returnName = TRUE,
                palette = "limited",
                closeOnClick = TRUE
              ),
              colourpicker::colourInput(
                ns("color_high"),
                "Highest value color",
                value = "red",
                returnName = TRUE,
                palette = "limited",
                closeOnClick = TRUE
              ),
              colourpicker::colourInput(
                ns("color_na"),
                "Background/missing values color",
                value = "gray70",
                returnName = TRUE,
                palette = "limited",
                closeOnClick = TRUE
              ),
              icon = icon("paintbrush", class = "ml"),
              tooltip = shinyWidgets::tooltipOptions(placement = "top", title = "Colors"),
              width = "250px",
              size = "xs"
            )
          ),
          width = 12,
          solidHeader = TRUE,
          status = "primary",
          selectInput(
            ns("heatmap_yaxis"),
            "Variable to plot on y-axis:",
            choices = c("Cluster", "Sample")
          ),
          shinyWidgets::materialSwitch(
            ns("facet_per_group"),
            HTML("<i style='font-size:15px;'> Show heatmaps per biological group </i>"),
            status = "success",
            right = TRUE,
            value = TRUE  # TRUE by default
          ),
          selectizeInput(
            ns("exclude_sample_types"),
            "Select sample types to exclude from the heatmaps:",
            choices = "",
            multiple = TRUE
          ),
          tabsetPanel(id = ns("tabs")),
          plotly::plotlyOutput(ns("clusters_plot"), height = "500px", width = "1350px"),
          textOutput(ns("no_data"))
        )
      ),
      fluidRow(
        shinydashboard::box(
          title = "Export results",
          width = 6,
          solidHeader = TRUE,
          status = "primary",
          radioButtons(ns("download_format"),
                       "Choose a file format:",
                       choices = c("Excel file", "R object")),
          downloadButton(ns("download"), "Download normalized data")
        )
      )
    )
  )
}
    
#' normalization Server Functions
#'
#' @noRd 
mod_normalization_server <- function(id, results_analyte_curation, merged_metadata, data_type) {
  moduleServer( id, function(input, output, session) {
    ns <- session$ns
    
    analyte_curated_data <- reactive({
      # req() considers an empty dataframe Truthy, and because of the way that
      # results_analyte_curation$analyte_curated_data() is created in
      # mod_analyte_curation.R there is a moment that it is an empty dataframe.
      # Solution -> wait until results_analyte_curation$analyte_curated_data()
      # is not empty:
      req(!rlang::is_empty(results_analyte_curation$analyte_curated_data()))
      results_analyte_curation$analyte_curated_data()
    })

    
    total_intensities <- reactive({
      req(analyte_curated_data())
      calculate_total_intensity(data = analyte_curated_data(), data_type = data_type())
    })
    
    # Normalized data is in long format
    normalized_data <- reactive({
      req(total_intensities())
      normalize_data(total_intensities = total_intensities()) %>% 
        { # Combine with metadata if it exists
          if (is_truthy(merged_metadata())) {
            dplyr::left_join(., merged_metadata(), by = "sample_id")
          } else .
        }
    })
    
    # Turn normalized data into wide format
    normalized_data_wide <- reactive({
      req(normalized_data())
      normalized_data() %>% 
        tidyr::pivot_wider(names_from = cluster, values_from = sum_intensity, 
                           names_glue = "{cluster}_sum_intensity") %>% 
        tidyr::pivot_wider(names_from = analyte, values_from = relative_abundance) %>% 
        dplyr::group_by(sample_name) %>% 
        tidyr::fill(replicates:last_col(), .direction = "downup") %>% 
        dplyr::ungroup() %>% 
        dplyr::distinct()
    })
    
    output$data_table <- DT::renderDT({
      req(normalized_data_wide())
      
      DT::datatable(data = normalized_data_wide(),
                    options = list(scrollX = TRUE))
    })
    
    
    
    # Create heatmap plots
    r <- reactiveValues(
      created_tab_titles = vector("character"),
      heatmaps = list()
    )
    
    # TODO:
    # - Option to plot cluster on y-axis
    # - Make it work with spike vs total
    # - Make it work with analyte curation per sample.
    # - Prevent making heatmaps when all sample types are excluded
    
    
    observe({
      req(normalized_data())
      
      # Remove previously created tabs
      purrr::map(r$created_tab_titles, function(tab_title) {
        removeTab(inputId = "tabs", target = tab_title, session = session)
      })
      
      ########## SAMPLE ON Y-AXIS, ONE TAB PER CLUSTER #############
      if (input$heatmap_yaxis == "Sample") {
        
        # Generate new tab titles
        cluster_names <- unique(normalized_data()$cluster)
        r$created_tab_titles <- cluster_names
        
        # Temporary list for storing heatmaps
        # This is necessary to prevent infinite looping
        temp_heatmaps <- vector("list", length = length(cluster_names))
        
        # Create tabs and plots
        purrr::imap(cluster_names, function(cluster, i) {
          
          plot <- sample_heatmap(
            normalized_data = normalized_data(),
            cluster_name = cluster,
            exclude_sample_types = input$exclude_sample_types,
            group_facet = dplyr::case_when(
              .default = "", 
              ( # Only when switch is on, and curation was done per biological group.
                # Second check is required, because switch is on by default.
                input$facet_per_group == TRUE &
                results_analyte_curation$curation_method() == "Per biological group"
              ) ~ results_analyte_curation$biogroups_colname()
            ),
            color_low = input$color_low,
            color_mid = input$color_mid,
            color_high = input$color_high,
            color_na = input$color_na
          )
          
          temp_heatmaps[[i]] <- plot  # Store plot in the temporary list
          
          # Show plot in UI, or message if there is no data
          if (is.character(plot)) {
            output[[cluster]] <- renderText(plot)
            appendTab(
              inputId = "tabs",
              select = TRUE,
              tab = tabPanel(
                title = cluster_names[[i]],
                textOutput(ns(cluster))
              )
            )
          } else {
            output[[cluster]] <- plotly::renderPlotly(plotly::ggplotly(plot, tooltip = "text"))
            appendTab(
              inputId = "tabs",
              select = TRUE,
              tab = tabPanel(
                title = cluster_names[[i]],
                plotly::plotlyOutput(ns(cluster), height = "600px", width = "1350px")
              )
            )
          }
          
          # Update reactive heatmaps list
          isolate(r$heatmaps <- temp_heatmaps)
          
        })
    
        #### CLUSTER ON Y-AXIS #### 
      } else if (input$heatmap_yaxis == "Cluster") {

        # Make the plot
        plot <- cluster_heatmap(
          normalized_data = normalized_data(),
          exclude_sample_types = input$exclude_sample_types,
          group_facet = dplyr::case_when(
            .default = "",
            (
              input$facet_per_group == TRUE &
              results_analyte_curation$curation_method() == "Per biological group"
            ) ~ results_analyte_curation$biogroups_colname()
          ),
          color_low = input$color_low,
          color_mid = input$color_mid,
          color_high = input$color_high,
          color_na = input$color_na
        )
        
        # Show plot in UI, or message if there is no data.
        if (is.character(plot)) {
          shinyjs::hide("clusters_plot")
          shinyjs::show("no_data")
          output$no_data <- renderText(plot)
        } else {
          shinyjs::show("clusters_plot")
          shinyjs::hide("no_data")
          output$clusters_plot <- plotly::renderPlotly(plotly::ggplotly(plot, tooltip = "text"))
        }
        
      }

    }) %>% 
      # Bind to events to prevent updating heatmaps when only analyte curation
      # settings are changed. 
      bindEvent(c(
        normalized_data(), input$facet_per_group, input$heatmap_yaxis, input$exclude_sample_types,
        input$color_low, input$color_mid, input$color_high, input$color_na
      ))
    
    
    
    
    # Determine choices for excluding sample types
    observe({
      req(normalized_data())
      updateSelectizeInput(
        inputId = "exclude_sample_types",
        choices = as.character(unique(normalized_data()$sample_type))
      )
    })
    
    # Toggle UI
    observe({
      shinyjs::toggleState("download", is_truthy(normalized_data_wide()))
      
      if (results_analyte_curation$curation_method() == "Per biological group" &
          results_analyte_curation$biogroups_colname() != "") {
        shinyjs::show("facet_per_group")
      } else {
        shinyjs::hide("facet_per_group")
      }
      
      
      if (results_analyte_curation$curation_method() == "Per biological group" &
          input$facet_per_group == TRUE & results_analyte_curation$biogroups_colname() != "") {
        shinyjs::hide("exclude_sample_types")
      } else {
        shinyjs::show("exclude_sample_types")
      }
      
      if (input$heatmap_yaxis == "Sample") {
        shinyjs::show("tabs")
        shinyjs::hide("clusters_plot")
      } else if (input$heatmap_yaxis == "Cluster") {
        shinyjs::hide("tabs")
        shinyjs::show("clusters_plot")
      }
      
    })

    
    # Download data
    output$download <- downloadHandler(
      filename = function() {
        current_datetime <- paste0(format(Sys.Date(), "%Y%m%d"), "_", format(Sys.time(), "%H%M"))
        switch(input$download_format,
               "R object" = paste0(current_datetime, "_normalized_data.rds"),
               "Excel file" = paste0(current_datetime, "_normalized_data.xlsx"))
      },
      content = function(file) {
        data_to_download <- normalized_data_wide()
        switch(input$download_format,
               "R object" = save(data_to_download,
                                 file = file),
               "Excel file" = writexl::write_xlsx(data_to_download,
                                                  path = file))
      }
    )
    
    return(list(
      normalized_data = normalized_data,
      normalized_data_wide = normalized_data_wide
    ))
 
  })
}
