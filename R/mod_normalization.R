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
          shinyWidgets::materialSwitch(
            ns("separate_charges"),
            HTML("<i style='font-size:15px;'> Normalize charge states separately </i>"),
            status = "success",
            right = TRUE,
            value = FALSE
          ),
          DT::dataTableOutput(ns("data_table"))
        )
      ),
      fluidRow(
        shinydashboardPlus::box(
          id = ns("box"),
          title = div(
            id = ns("box_header"),
            "Visualize normalized data",
            icon("info-circle", class = "ml") %>% 
              bsplus::bs_embed_popover(
                title = "Explanation",
                content = HTML(
                  "
                  When plotting \"Cluster\" on the y-axis, the median relative abundance
                  for each analyte is shown in one heatmap. When plotting \"Sample\" 
                  on the y-axis, a heatmap is shown for each cluster, with the relative
                  abundances of all glycans per sample.
                  <br> <br>
                  If analyte curation was performed per biological group, separate
                  heatmaps can be shown per group. In that case, sample types without 
                  a biological group assigned (e.g. blanks and pools) are automatically excluded.
                  "
                ),
                trigger = "hover",
                placement = "left",
                html = "true",
                container = "body"),
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
            choices = c("Glycosylation site", "Sample")
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
          shinyjqui::jqui_resizable(plotly::plotlyOutput(ns("clusters_plot"))),
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
      # Check if charge states should be treated separately
      if (input$separate_charges == TRUE) {
        results_analyte_curation$analyte_curated_data() %>% 
          tidyr::unite("analyte", analyte:charge, sep = "_", remove = FALSE)
      } else {
        results_analyte_curation$analyte_curated_data()
      }
    }) 

    total_intensities <- reactive({
      req(analyte_curated_data())
      calculate_total_intensity(data = analyte_curated_data(), data_type = data_type())
    })
    
    # Normalized data is in long format
    normalized_data <- reactive({
      req(total_intensities())
      data <- normalize_data(total_intensities = total_intensities()) %>% 
        # Sort by glycan composition
        tidyr::separate(analyte, into = c("cluster", "analyte"),
                        sep = "1", extra = "merge") %>% 
        sort_glycans(.) %>% 
        dplyr::group_by(cluster) %>% 
        dplyr::arrange(analyte) %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate(analyte = paste0(cluster, "1", analyte))
        
      # Check if metadata exists
      if (is_truthy(merged_medata())) {
        data <- dplyr::left_join(data, merged_metadata(), by = "sample_id")
      }
      
      return(data)
    })
    

    # Notes from data per analyte (Skyline)
    notes <- reactive({
      req(analyte_curated_data())
      if ("note" %in% colnames(analyte_curated_data())) {
        notes <- analyte_curated_data() %>% 
          dplyr::select(analyte, note) %>% 
          dplyr::filter(note != "") %>% 
          dplyr::distinct(analyte, .keep_all = TRUE)
      } else NULL
    })
    
    # When notes exist: tell user that downloading Excel includes notes
    # R object does not include notes.
    observe({
      if (is_truthy(notes())) {
        updateRadioButtons(
          inputId = "download_format",
          choices = c("Excel file (data and notes)", "R object (data only)")
        )
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
      DT::datatable(data = normalized_data_wide() %>% 
                      dplyr::mutate_if(is.numeric, ~format(round(., 2), nsmall = 2)),
                    options = list(
                      scrollX = TRUE,
                      pageLength = 6,
                      columnDefs = list(list(className = "dt-center", targets = "_all"))
                    ), filter = "top")
    })
    
    
    # Create heatmap plots
    r <- reactiveValues(
      created_tab_titles = vector("character"),  # To store names of tabs'
      heatmaps = list()  # To store the heatmaps for the report
    )
    
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
          
          # Store plot in list, give it name of the cluster.
          # Need isolate() to prevent infinite loop
          isolate({
            r$heatmaps[[i]] <- plot
            names(r$heatmaps)[[i]] <- cluster
          })
          
          # Show plot in UI
          output[[cluster]] <- plotly::renderPlotly(plotly::ggplotly(plot, tooltip = "text"))
          appendTab(
            inputId = "tabs",
            select = TRUE,
            tab = tabPanel(
              title = cluster_names[[i]],
              shinyjqui::jqui_resizable(plotly::plotlyOutput(ns(cluster)))
            )
          )
        })
        
        #### CLUSTER ON Y-AXIS #### 
      } else if (input$heatmap_yaxis == "Glycosylation site") {

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
        
        # Store plot in list
        isolate(r$heatmaps <- list(plot))
        
        # Show plot in UI and store in list
        output$clusters_plot <- plotly::renderPlotly(plotly::ggplotly(plot, tooltip = "text"))
      }
      
    }) %>% 
      # Bind to events to prevent updating heatmaps when only analyte curation
      # settings are changed. 
      bindEvent(c(
        normalized_data(), input$facet_per_group, input$heatmap_yaxis, input$exclude_sample_types,
        input$color_low, input$color_mid, input$color_high, input$color_na
      ))

    
    
    # Excluded sample types, to pass on to report
    heatmaps_excluded_sample_types <- reactive({
      if (results_analyte_curation$curation_method() == "Per biological group") {
        if (input$facet_per_group == TRUE) {
          c("")
        } else if (length(input$exclude_sample_types) == 0) {
          c("None")
        } else {
          input$exclude_sample_types
        }
      } else {
        if (length(input$exclude_sample_types) == 0) {
          c("None")
        } else {
          input$exclude_sample_types
        }
      }
    })

    
    # Determine choices for excluding sample types
    observe({
      req(normalized_data())
      updateSelectizeInput(
        inputId = "exclude_sample_types",
        choices = as.character(unique(normalized_data()$sample_type)),
        options = list(maxItems = length(as.character(unique(normalized_data()$sample_type))) - 1)
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
      } else if (input$heatmap_yaxis == "Glycosylation site") {
        shinyjs::hide("tabs")
        shinyjs::show("clusters_plot")
      }
      
    })

    
    # Download normalized data
    output$download <- downloadHandler(
      filename = function() {
        current_datetime <- paste0(format(Sys.Date(), "%Y%m%d"), "_", format(Sys.time(), "%H%M"))
        if (grepl("R object", input$download_format)) {
          paste0(current_datetime, "_normalized_data.rds")
        } else {
          paste0(current_datetime, "_normalized_data.xlsx")
        }
      },
      content = function(file) {
        if (grepl("R object", input$download_format)) {
          save(normalized_data_wide(), file = file)
        } else if (is_truthy(notes())) {
          data_list <- list("Data" = normalized_data_wide(), "Notes" = notes())
          writexl::write_xlsx(data_list, path = file)
        } else{
          writexl::write_xlsx(normalized_data_wide(), path = file)
        }
      }
    )
    
    
    
    return(list(
      normalized_data = normalized_data,
      normalized_data_wide = normalized_data_wide,
      notes = notes,
      heatmaps = reactive(r$heatmaps),
      heatmaps_excluded_sample_types = heatmaps_excluded_sample_types
    ))
 
  })
}
