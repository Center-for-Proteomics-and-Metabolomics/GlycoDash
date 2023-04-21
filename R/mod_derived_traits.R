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
        h1("Derived traits"),
              ),
      fluidRow(
        shinydashboard::box(
          id = ns("box"),
          title = "Calculate derived traits automatically",
          width = 4,
          solidHeader = TRUE,
          status = "primary",
          div(
            strong("Attention:"),
            "derived traits calculations can only be performed on IgG data for now.",
            style = "color:#0021B8"
          ),
          br(),
          shinyWidgets::awesomeCheckboxGroup(ns("traits_menu"),
                                             "Select the derived traits that should be calculated:",
                                             choices = c("Fucosylation",
                                                         "Bisection",
                                                         "Galactosylation",
                                                         "Sialylation")),
          actionButton(ns("do_calculation"),
                       "Calculate derived traits")
        ),
        shinydashboard::box(
          title = "Formulas used to automatically calculate the derived traits:",
          width = 8,
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
            "Calculate custom derived traits",
      
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
          width = 4,
          solidHeader = TRUE,
          status = "primary",
          fileInput(ns("custom_traits_file"),
                    "Upload Excel file with custom derived traits formulas"
                    )
        ),
        
        shinydashboard::box(
          title = "Formulas used to calculate the custom derived traits:",
          width = 8,
          solidHeader = TRUE,
          status = "primary",
          DT::dataTableOutput(ns("custom_formulas"))
        )
      ),
      
      
      
      fluidRow(
        shinydashboard::box(
          title = "View data with derived traits",
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
mod_derived_traits_server <- function(id, results_normalization){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    normalized_data <- reactive({
      req(results_normalization$normalized_data())
      results_normalization$normalized_data()
    })
    
    ####################  Custom derived traits  ####################
    
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
        readxl::read_excel(input$custom_traits_file$datapath, col_names = FALSE)
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
    

  
    ####################  Default derived traits  ####################
    
    observe({
      shinyjs::toggleState("do_calculation", 
                           condition = all(is_truthy(input$traits_menu),
                                           is_truthy(normalized_data())))
    })
    
    derived_traits <- reactive({
      #req(normalized_data())
      
      calculate_derived_traits(data = normalized_data(),
                               selected_derived_traits = input$traits_menu)
    }) %>% bindEvent(input$do_calculation)
    
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
    
    # Combine default traits with custom traits
    data_with_all_traits <- reactive({
      req(data_with_derived_traits(), data_with_custom_traits())
      dplyr::full_join(
        data_with_derived_traits(), data_with_custom_traits()
      )
    })
    
    
    # If only default traits were calculated: use "data_with_derived_traits()" here
    # If only custom traits were calculated: use "data_with_custom_traits()" here
    # If both default and custom traits were calculated: use "data_with_all_traits()" here
    
    traits_data_table <- reactive({
      if (is_truthy(data_with_all_traits())) {
        return(data_with_all_traits())  
      } else if (is_truthy(data_with_derived_traits())) {
        return(data_with_derived_traits())
      } else if (is_truthy(data_with_custom_traits())) {
        return(data_with_custom_traits())
      }
    })

    output$data_table <- DT::renderDT({
      req(traits_data_table())
      # req(data_with_derived_traits())
      DT::datatable(data = traits_data_table(),
                    # data = data_with_derived_traits(),
                    options = list(scrollX = TRUE))
    })
    
    
    
    ########## Download example Excel of custom traits ##########
    output$download_ex_custom_formulas <- downloadHandler(
      filename = "Custom_traits_formulas_example.xlsx",
      content = function(file) {
        example_file <- system.file("app",
                                    "www",
                                    "Custom_traits_formulas_example.xlsx",
                                    package = "glycodash")
        file.copy(example_file, file)
      }
    )
    
    
    
    ############### Formulas of derived traits ###############
    
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
      req(custom_formulas(), extension())
      DT::datatable(custom_formulas(),
                    colnames = c("Cluster", "Custom trait", "Formula"),
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
                            names_to = "Derived trait",
                            values_to = "Formula") %>% 
        dplyr::rename_with(.fn = firstupper, 
                           .cols = cluster) %>% 
        dplyr::mutate(`Derived trait` = dplyr::recode(`Derived trait`,
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
    
    
    
##### Create "data_with_traits" tibble to return  #########
    data_with_traits <- reactive({
      if (is_truthy(data_with_all_traits())) {
        return(data_with_all_traits())
      } else if (is_truthy(data_with_derived_traits())) {
        return(data_with_derived_traits())
      } else if (is_truthy(data_with_custom_traits())) {
        return(data_with_custom_traits())
      }
    })
    
    
    return(
      list(
        # data_with_derived_traits = data_with_derived_traits,
        data_with_traits = data_with_traits,
        normalized_data = normalized_data,
        derived_traits = reactive({ input$traits_menu }),
        formulas = formulas,
        custom_formulas = custom_formulas
      )
    )
 
  })
}
