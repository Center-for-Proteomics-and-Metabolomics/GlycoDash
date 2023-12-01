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
          tabsetPanel(
            id = ns("tabs"),
            # Human IgG tab
            tabPanel("Human IgG", tagList(
              br(),
              shinyWidgets::awesomeCheckboxGroup(
                ns("human_IgG_traits"),
                "Select the traits you want to calculate for human IgG:",
                choices = c(
                  "Fucosylation of complex-type glycans",
                  "Bisection of complex-type glycans",
                  "Galactosylation of complex-type glycans",
                  "Sialylation of complex type-glycans",
                  "Percentage of monoantennary complex-type glycans",
                  "Percentage of hybrid-type glycans",
                  "Oligomannose-type glycans: average number of mannoses"
                )
              ),
              selectizeInput(
                ns("human_IgG_clusters"),
                "For which clusters in your data should the human IgG traits be calculated?",
                choices = c(""),
                multiple = TRUE
              )
            )),
            # Mouse IgG tab
            tabPanel("Mouse IgG", tagList(
              br(),
              shinyWidgets::awesomeCheckboxGroup(
                ns("mouse_IgG_traits"),
                "Select the traits you want to calculate for mouse IgG:",
                choices = c(
                  "Mouse Trait 1",
                  "Mouse Trait 2",
                  "Mouse Trait 3"
                )
              ),
              selectizeInput(
                ns("mouse_IgG_clusters"),
                "For which clusters in your data should the mouse IgG traits be calculated?",
                choices = c(""),
                multiple = TRUE
              )
            ))
          ),
          br(),
          actionButton(ns("do_calculation"),
                       "Calculate glycosylation traits")
        ),
        shinydashboard::box(
          title = "Formulas used to automatically calculate the glycosylation traits:",
          width = 7,
          solidHeader = TRUE,
          status = "primary",
          downloadButton(ns("download_formulas"), "Download as Excel file"),
          br(),
          DT::dataTableOutput(ns("formulas"))
        ),
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
                  The Excel file must contain one column called \"trait\" that
                  specifies the names of the traits. The second column must be called
                  \"formula\" and should contain the formulas for the traits. Analyte names
                  in the formula should include both the cluster and glycan composition, 
                  e.g. \"IgGI1H4N4F1\".
                  <br><br>
                  <strong>Trait names should not contain any spaces.</strong>
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
    
    # Toggle visibility of tabs, depending on input$antibody_types
    observeEvent(input$antibody_types, {
      purrr::map(
        c("Human IgG", "Mouse IgG"),
        function(antibody_type) {
          if (antibody_type %in% input$antibody_types) {
            showTab(inputId = "tabs", target = antibody_type, select = TRUE)
          } else {
            hideTab(inputId = "tabs", target = antibody_type)
          }
        }
      )
    }, ignoreNULL = FALSE)
    
    
    # Add cluster options
    clusters <- reactive({
      req(normalized_data())
      unique(normalized_data()$cluster)
    })
    
    observe({
      req(clusters())
      for (id in c("human_IgG_clusters", "mouse_IgG_clusters")) {
        updateSelectizeInput(id, choices = clusters(), session = session)
      }
    })
    
    
    # Toggle calculation button
    observe({
      shinyjs::toggleState("do_calculation", condition = all(
        !is.null(input$antibody_types),
        if ("Human IgG" %in% input$antibody_types) {
          !is.null(input$human_IgG_traits) & !is.null(input$human_IgG_clusters)
        },
        if ("Mouse IgG" %in% input$antibody_types) {
          !is.null(input$mouse_IgG_traits) & !is.null(input$mouse_IgG_clusters)
        }
      ))
    })

    
    ####################  Custom glycosylation traits  ####################
    
    # Reactive expression containing the file extension of the uploaded file
    extension <- reactive({
      req(input$custom_traits_file)
      tools::file_ext(input$custom_traits_file$name)
    })
    
    # Check if the extension is OK
    observeEvent(extension(), {
      shinyFeedback::hideFeedback("custom_traits_file")  # Hide previous feedback, if any
      shinyFeedback::feedbackDanger(
        "custom_traits_file",
        !extension() %in% c("xlsx", "xls"),
        "Please upload an .xlsx or .xls file."
      )
    })
    
    # Read the custom traits Excel file as a data frame.
    traits_excel <- reactive({
      req(input$custom_traits_file, extension(), extension() == "xlsx")
      readxl::read_excel(input$custom_traits_file$datapath, col_names = TRUE, col_types = "text")
    })

    
    # Check if traits_excel is formatted correctly
    r <- reactiveValues(correct_formatting = FALSE)
    observeEvent(traits_excel(), {
      # First check for correct columns
      ncol <- ncol(traits_excel())
      colnames <- colnames(traits_excel())
      if (!all(ncol == 2, colnames[1] == "trait", colnames[2] == "formula")) {
        shinyalert::shinyalert(
          text = "Your Excel file should contain two columns: \"trait\" and \"formula\". Please adjust your file.",
          confirmButtonCol = "tomato"
        )
        r$correct_formatting <- FALSE
      } else {
        # Then check for spaces in trait names
        if (any(grepl(" ", traits_excel()$trait))) {
          shinyalert::shinyalert(
            text = "Trait names should not contain any spaces. Please adjust your Excel file.",
            confirmButtonCol = "tomato"
          )
        }
        r$correct_formatting <- FALSE
      }
    })

    # Calculate the custom traits
    data_with_custom_traits <- reactive({
      req(traits_excel(), normalized_data(), r$correct_formatting == TRUE)
      calculate_custom_traits(traits_excel(), results_normalization$normalized_data_wide())
    })



    ####################  Default glycosylation traits  ####################
    
    human_IgG_traits <- reactive({
      req(input$human_IgG_traits)
      match_human_IgG_traits(input$human_IgG_traits)
    })
    
    trait_formulas <- reactive({
      req(normalized_data())
      formula_list <- create_formula_list(
        normalized_data = normalized_data(),
        chosen_traits = human_IgG_traits(),
        chosen_clusters = input$human_IgG_clusters,
        reference = human_IgG_ref
      )
      purrr::reduce(formula_list, c)  # c = concatenate
    }) %>% bindEvent(input$do_calculation)
    
    
    data_with_derived_traits <- reactive({
      req(trait_formulas())
      calculate_traits(results_normalization$normalized_data_wide(), trait_formulas())
    })

    
    ############### Combined default + custom traits ###############
    
    # Combine default traits with custom traits
    data_with_all_traits <- reactive({
      req(data_with_derived_traits(), data_with_custom_traits())
      dplyr::full_join(
        data_with_derived_traits(), data_with_custom_traits()
      ) %>%
        dplyr::relocate(all_of(traits_excel()$trait), .after = replicates)
    })
    
    # If only default traits were calculated: use "data_with_derived_traits()" here
    # If only custom traits were calculated: use "data_with_custom_traits()" here
    # If both default and custom traits were calculated: use "data_with_all_traits()" here
    with_data <- reactive({
      if (is_truthy(data_with_all_traits())) {
        data_with_all_traits()
      } else if (is_truthy(data_with_derived_traits())) {
        data_with_derived_traits()
      } else if (is_truthy(data_with_custom_traits())) {
        data_with_custom_traits()
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
    
    # Display for double check
    output$custom_formulas <- DT::renderDT({
      req(traits_excel(), data_with_derived_traits())
      DT::datatable(traits_excel(),
                    rownames = FALSE, 
                    options = list(paging = FALSE,
                                  ordering = FALSE,
                                  searching = FALSE))
    })
    
    
    # Display the formulas of the default traits
    formulas_table <- reactive({
      req(data_with_traits())
      formula_dfs <- vector("list", length = length(trait_formulas()))
      for (i in seq(length(trait_formulas()))) {
        trait_formula <- trait_formulas()[i]
        trait_name <- names(create_expr_ls(trait_formula))
        calculation <- stringr::str_remove(trait_formula, paste0(trait_name, " = "))
        formula_dfs[[i]] <- data.frame(trait = trait_name, formula = calculation)
      }
      purrr::reduce(formula_dfs, dplyr::full_join)
    })
    
    output$formulas <- DT::renderDT({
      req(formulas_table())
      DT::datatable(formulas_table(),
                    rownames = FALSE,
                    options = list(paging = FALSE,
                                   ordering = FALSE,
                                   searching = FALSE))
    })
    
    # Option to download the default traits formulas as an Excel file
    observe({
      shinyjs::toggleState("download_formulas", is_truthy(formulas_table()))
    })
    
    output$download_formulas <- downloadHandler(
      filename = function() {
        current_datetime <- paste0(format(Sys.Date(), "%Y%m%d"), "_", format(Sys.time(), "%H%M"))
        paste0(current_datetime, "_glycosylation_traits_formulas.xlsx")
      },
      content = function(file) {
        writexl::write_xlsx(formulas_table(), path = file)
      }
    )
    
    
    return(
      list(
        data_with_traits = data_with_traits,
        normalized_data = normalized_data,
        derived_traits = reactive({ input$traits_menu }),
        formulas = formulas_table,
        custom_traits_excel = traits_excel
      )
    )
 
  })
}
