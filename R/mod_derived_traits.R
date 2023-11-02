#' derived_traits UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_derived_traits_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyFeedback::useShinyFeedback(),  # Needed for warning message when non-xlsx file is uploaded.
    tags$style(HTML(paste0(
      "#",
      ns("box_header"),
      " .awesome-checkbox {padding-top: 7px}",
      "#",
      ns("box_header"),
      " .popover {max-width: 400px !important; color: #333}",
      "#",
      ns("box"),
      " .box-title {width: 100%}",
      "#",
      ns("box_header"),
      # changed all .fa to .fas  because of fontawesome version update
      " .fas {float: right; margin-right: 5px; font-size: 18px}",
      "#",
      ns("box_header"),
      " .direct-chat-contacts {right: 0; background: #222d32!important}",
      "#",
      ns("box_header"),
      " .btn {float: right; border-width: 0px; margin-right: 5px}",
      "#",
      ns("box"),
      " .dropdown {display: inline-block; float: right; width: 135px}",
      "#",
      ns("box_header"),
      " .dropdown-menu {background: #333; right: -30px; left: auto; top: 28px;}"
    ))),
    fluidPage(
      fluidRow(
        h1("Glycosylation traits"),
              ),
      fluidRow(
        shinydashboard::box(
          id = ns("box"),
          title = "Calculate glycosylation traits automatically",
          width = 5,
          solidHeader = TRUE,
          status = "primary",
          shinyWidgets::awesomeCheckboxGroup(
            ns("antibody_types"),
            "Select the types of antibodies that are present in your data:",
            choices = c("Human IgG", "Mouse IgG")
          ),
          # Tab panel for traits options
          tabsetPanel(id = ns("tabs")),
          br(),
          actionButton(ns("do_calculation"),
                       "Calculate glycosylation traits")
        ),
        shinydashboard::box(
          title = "Formulas used to automatically calculate the glycosylation traits:",
          width = 7,
          solidHeader = TRUE,
          status = "primary",
        DT::dataTableOutput(ns("formulas"))
        )
      ),
      
      fluidRow(
        shinydashboardPlus::box(
          id = ns("box"),
          title = div(
            id = ns("box_header"),
            "Calculate custom glycosylation traits",
      
            # Add info for custom traits
            icon("info-circle", class = "ml") %>% 
              bsplus::bs_embed_popover(
                title = "Explanation",
                content = HTML(
                  "Here you can upload an Excel file with formulas of the 
                  custom traits that you want to calculate.
                  Click the paperclip button to download an example Excel file.
                  <br><br>
                  The Excel file must contain one column that specifies the
                  clusters for which you want to calculate the traits. The 
                  second column contains the formulas that you want to use.
                  A formula consists of the name of the trait, and how
                  to calculate the trait, separated by an \"=\" sign. 
                  <br><br>
                  <strong>Trait names should not contain any spaces.</strong>
                  <br><br>
                  You must always place spaces around the following signs: 
                  addition, subtraction, division, multiplication (+ &nbsp; - &nbsp; \\ &nbsp; *)
                  "
                ),
                trigger = "hover",
                placement = "right",
                html = "true"
              ),
            
            shinyWidgets::dropdownButton(
              tags$style(HTML(paste0(
                "#",
                ns("dropdown_content"),
                " .fas {float: left}",
                "#",
                ns("dropdown_content"),
                " .btn {float: none; border-width: 1px; width: 280px; margin: 10px}"
              ))),
              div(id = ns("dropdown_content"),
                  downloadButton(ns("download_ex_custom_formulas"),
                                 "Download an example Excel file")),
              icon = icon("paperclip", class = "ml"),
              tooltip = shinyWidgets::tooltipOptions(placement = "top",
                                                     title = "Example"),
              width = "330px",
              size = "xs"
            )
          ),
          width = 5,
          solidHeader = TRUE,
          status = "primary",
          fileInput(ns("custom_traits_file"),
                    "Upload Excel file with custom glycosylation traits formulas:"
                    )
        ),
        
        shinydashboard::box(
          title = "Formulas used to calculate the custom glycosylation traits:",
          width = 7,
          solidHeader = TRUE,
          status = "primary",
          DT::dataTableOutput(ns("custom_formulas"))
        )
      ),
      
      
      fluidRow(
        shinydashboard::box(
          title = "View data with glycosylation traits",
          width = 12,
          solidHeader = TRUE,
          status = "primary",
          DT::dataTableOutput(ns("data_table"))
        )
      )
    )
  )
}
    



#' derived_traits Server Functions
#'
#' @noRd 
mod_derived_traits_server <- function(id, results_normalization, results_quantitation) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    normalized_data <- reactive({
      req(results_normalization$normalized_data())
      results_normalization$normalized_data()
    })
    
    
    # Create vector to store created tab names, and to count number of changes in input$antibody_types
    r <- reactiveValues(count = 0, created_tab_ids = vector("character"))
    
    observeEvent(input$antibody_types, {
      # Increase count by 1
      r$count <- r$count + 1
      # Remove previously created tabs
      purrr::map(r$created_tab_ids, function(tab_id) {
        removeTab(inputId = "tabs", target = tab_id, session = session)
      })
      # Reset r$created_tab_ids
      r$created_tab_ids <- vector("character")
      # Create a tab for each selected antibody type
      purrr::imap(input$antibody_types, function(antibody_type, i) {
        # Generate a unique tab id
        tab_id <- paste0(antibody_type, "_", r$count)
        # Store the tab_id in the vector
        r$created_tab_ids[i] <- tab_id
        # Create the new tab
        appendTab(
          inputId = "tabs",
          select = TRUE,
          tab = tabPanel(
            title = antibody_type,
            value = tab_id,
            mod_tab_traits_ui(ns(tab_id))
          )
        )
      })
      # Generate the server parts of the tabs
      purrr::map(r$created_tab_ids, function(tab_id) {
        antibody_type <- stringr::str_split(tab_id, pattern = "_")[[1]][1]
        mod_tab_traits_server(id = tab_id, antibody_type = antibody_type)
      })
    #  ignoreNULL = FALSE is required to trigger the observer when all boxes are unchecked
    }, ignoreNULL = FALSE)
  
    

    ####################  Custom glycosylation traits  ####################
    
    # Reactive expression containing the file extension of the uploaded file
    extension <- reactive({
      req(input$custom_traits_file)
      tools::file_ext(input$custom_traits_file$name)
    })
    
    # Show warning when non-Excel file is uploaded.
    observe({
      req(extension())
      shinyFeedback::feedbackWarning("custom_traits_file",
                                     extension() != "xlsx",
                                     text = "Please upload an Excel (.xlsx) file.")
    })
    
    
    traits_excel <- reactive({
      req(input$custom_traits_file, extension())
      if (extension() == "xlsx"){
        readxl::read_excel(input$custom_traits_file$datapath, col_names = FALSE, col_types = "text")
      }
    })
    
    custom_traits <- reactive({
      req(traits_excel(), normalized_data())
      calculate_custom_traits(normalized_data(), traits_excel())
    })
    
    data_with_custom_traits <- reactive({
      req(custom_traits())
      dplyr::full_join(custom_traits(), results_normalization$normalized_data_wide()) %>% 
      dplyr::select(-tidyselect::ends_with("formula"))
    })
    
    

  
    ####################  Default glycosylation traits  ####################
    
    # observe({
    #   shinyjs::toggleState("do_calculation", all(
    #     is_truthy(normalized_data()),
    #     length(input$antibody_types) > 0,
    #     if ("Human IgG" %in% input$antibody_types) {
    #       length(input$human_IgG_traits) > 0 & length(input$human_IgG_clusters) > 0
    #     }
    #   ))
    # })
    
    derived_traits <- reactive({
      req(normalized_data())
      calculate_derived_traits(data = normalized_data(),
                               selected_derived_traits = input$traits_menu)
    }) %>% bindEvent(input$do_calculation)
    
    observeEvent(input$do_calculation, {
      req(normalized_data())
      browser()
    }, priority = 10)
    
    data_with_derived_traits <- reactive({
      req(derived_traits())
      dplyr::full_join(results_normalization$normalized_data_wide(),
                       derived_traits()) %>% 
        dplyr::select(-tidyselect::ends_with("formula")) %>% 
        tidyr::pivot_wider(names_from = cluster,
                           values_from = dplyr::any_of(input$traits_menu),
                           names_glue = "{cluster}_{.value}") %>% 
        dplyr::relocate(contains(input$traits_menu), .after = replicates)
    })
    
    
    
    ############### Combined default + custom traits ###############
    
    # Get column names of custom traits, required to relocate them after combining with default traits
    custom_traits_colnames <- reactive({
      req(custom_traits())
      custom_traits() %>% 
        dplyr::select(replicates:dplyr::last_col()) %>% 
        dplyr::select(-replicates) %>% 
        dplyr::select(1:floor(ncol(.)/2)) %>% # This removes columns with traits formulas
        names()  # This extracts the column names as a vector
    })
    
    
    # Combine default traits with custom traits
    data_with_all_traits <- reactive({
      req(data_with_derived_traits(), data_with_custom_traits(), custom_traits_colnames())
      dplyr::full_join(
        data_with_derived_traits(), data_with_custom_traits()
      ) %>% 
        dplyr::relocate(all_of(custom_traits_colnames()), .after = replicates)
    })
    
    
    # If only default traits were calculated: use "data_with_derived_traits()" here
    # If only custom traits were calculated: use "data_with_custom_traits()" here
    # If both default and custom traits were calculated: use "data_with_all_traits()" here
    with_data <- reactive({
      if (is_truthy(data_with_all_traits())) {
        return(data_with_all_traits())  
      } else if (is_truthy(data_with_derived_traits())) {
        return(data_with_derived_traits())
      } else if (is_truthy(data_with_custom_traits())) {
        return(data_with_custom_traits())
      }
    })
    
    # Check if there is quantitation data to combine with the traits.
    data_with_traits <- reactive({
      req(with_data())
      if (is_truthy(results_quantitation$quantitation_data())) {
        dplyr::full_join(with_data(), results_quantitation$quantitation_data()) %>% 
          dplyr::relocate(IgG1_quantity_ng, .after = replicates)
      } else {
        with_data()
      }
    })
    
    output$data_table <- DT::renderDT({
      req(data_with_traits())
      DT::datatable(data = data_with_traits(),
                    options = list(scrollX = TRUE))
    })
    
    
    
    ########## Download example Excel of custom traits ##########
    output$download_ex_custom_formulas <- downloadHandler(
      filename = "Custom_traits_formulas_example.xlsx",
      content = function(file) {
        example_file <- system.file("app",
                                    "www",
                                    "Custom_traits_formulas_example.xlsx",
                                    package = "GlycoDash")
        file.copy(example_file, file)
      }
    )
    
    
    
    ############### Formulas of glycosylation traits ###############
    
    #### Custom traits formulas
    custom_formulas <- reactive({
      req(traits_excel())
      tryCatch({
        traits_excel() %>%
          dplyr::rename(cluster = ...1) %>% 
          dplyr::rename(trait_formula = ...2) %>% 
          tidyr::separate(trait_formula, into = c("custom_trait", "formula"),
                          sep = "=", remove = TRUE)
      }, warning = function(w) {
        shinyFeedback::feedbackWarning("custom_traits_file",
                                       show = TRUE,
                                       text = "Excel file not formatted correctly!")
        NULL
      })
      # traits_excel() %>%
      #   dplyr::rename(cluster = ...1) %>% 
      #   dplyr::rename(trait_formula = ...2) %>% 
      #   tidyr::separate(trait_formula, into = c("custom_trait", "formula"),
      #                   sep = "=", remove = TRUE)
    })
    
    output$custom_formulas <- DT::renderDT({
      req(custom_formulas(), extension(), custom_traits())
      DT::datatable(custom_formulas(),
                    colnames = c("Cluster", "Trait", "Formula"),
                    rownames = FALSE, 
                    options = list(paging = FALSE,
                                  ordering = FALSE,
                                  searching = FALSE))
    })
    
    
    #### Default traits formulas
    formulas <- reactive({
      req(derived_traits())
      derived_traits() %>% 
        dplyr::select(tidyselect::ends_with("formula"), cluster) %>% 
        dplyr::distinct() %>% 
        tidyr::pivot_longer(cols = -cluster,
                            names_to = "Trait",
                            values_to = "Formula") %>% 
        dplyr::rename_with(.fn = firstupper, 
                           .cols = cluster) %>% 
        dplyr::mutate(`Trait` = dplyr::recode(`Trait`,
                                                      fuc_formula = "Fucosylation",
                                                      gal_formula = "Galactosylation",
                                                      sial_formula = "Sialylation",
                                                      bis_formula = "Bisection"))
    })
    
    output$formulas <- DT::renderDT({
      req(formulas())
      
      DT::datatable(formulas(),
                    rownames = FALSE, 
                    options = list(paging = FALSE,
                                   ordering = FALSE,
                                   searching = FALSE))
    })
    
    
    
    return(
      list(
        data_with_traits = data_with_traits,
        normalized_data = normalized_data,
        derived_traits = reactive({ input$traits_menu }),
        formulas = formulas,
        custom_traits_colnames = custom_traits_colnames,
        custom_formulas = custom_formulas
      )
    )
 
  })
}
