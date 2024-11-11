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
    tags$style(HTML(paste0(
      "#", ns("box_header"), " .awesome-checkbox {padding-top: 7px}",
      "#", ns("box_header"), " .popover {max-width: 400px !important; color: #333}",
      "#", ns("box"), " .box-title {width: 100%}",
      "#", ns("box_header"), " .fas {float: right; margin-right: 5px; font-size: 18px}",
      "#", ns("box_header"), " .direct-chat-contacts {right: 0; background: #222d32!important}",
      "#", ns("box_header"), " .btn {float: right; border-width: 0px; margin-right: 5px}",
      "#", ns("box"), " .dropdown {display: inline-block; float: right; width: 135px}",
      "#", ns("box_header"), " .dropdown-menu {background: #333; right: -30px; left: auto; top: 28px;}"
    ))),
    fluidPage(
      fluidRow(
        h1("Glycosylation traits"),
              ),
      fluidRow(
        shinydashboardPlus::box(
          id = ns("box"),
          title = div(
            id = ns("box_header"),
            "Calculate glycosylation traits automatically",
            icon("info-circle", class = "ml") %>%
              bsplus::bs_embed_popover(
                title = "Explanation",
                content = HTML(
                  "
                Glycosylation traits are calculated based on a reference list
                containing known glycan compositions. A warning is shown when
                your data contains analytes with unknown glycan compositions.
                <br> <br>
                The formulas used to calculate the traits will be shown in the 
                table on the right. You can change the calculations by downloading
                the table as an Excel file, modifying the formulas, and then uploading
                the file in the \"custom glycosylation traits\" box below.
                "
                ),
                trigger = "hover",
                placement = "bottom",
                html = "true"
              ),
          ),
          width = 5,
          solidHeader = TRUE,
          status = "primary",
          shinyWidgets::awesomeCheckboxGroup(
            ns("antibody_types"),
            "Select the types of antibody glycans that are present in your data:",
            choices = c(
              "Human IgG: N-glycans",
              "Human IgA: N-glycans",
              "Human IgA: O-glycans",
              "Human IgM: N-glycans",
              "Human Joining Chain: N-glycans",
              "Mouse IgG: N-glycans"
            )
          ),
          
          # Tab panel for traits options
          tabsetPanel(
            id = ns("tabs"),
            
            # Human IgG N-glycans
            tabPanel("Human IgG: N-glycans", tagList(
              br(),
              shinyWidgets::awesomeCheckboxGroup(
                ns("human_IgG_N_traits"),
                "Select traits to calculate:",
                choices = c(
                  "Fucosylation of complex-type glycans",
                  "Bisection of complex-type glycans",
                  "Galactosylation per antenna of complex-type glycans",
                  "Sialylation per antenna of complex-type glycans",
                  "Sialylation per galactose of complex-type glycans",
                  "Percentage of monoantennary complex-type glycans",
                  "Percentage of hybrid-type glycans",
                  "Percentage of oligomannose-type glycans",
                  "Oligomannose-type glycans: average number of mannoses"
                )
              ),
              selectizeInput(
                ns("human_IgG_N_clusters"),
                "Select clusters for which these traits should be calculated:",
                choices = c(""),
                multiple = TRUE
              )
            )),
            
            # Human IgA N-glycans per glycosylation site
            tabPanel("Human IgA: N-glycans", tagList(
              br(),
              shinyWidgets::awesomeCheckboxGroup(
                ns("human_IgA_N47_traits"),
                "Select traits to calculate for IgA2 glycosylation site N47:",
                choices = c(
                  "Core fucosylation of complex-type glycans",
                  "Antennary fucosylation of complex-type glycans",
                  "Bisection of complex-type glycans",
                  "Galactosylation per antenna of complex-type glycans",
                  "Sialylation per antenna of complex-type glycans",
                  "Sialylation per galactose of complex-type glycans",
                  "Percentage of triantennary complex-type glycans"
                )
              ),
              selectizeInput(
                ns("human_IgA_N47_clusters"),
                "Select clusters for which these traits should be calculated:",
                choices = c(""),
                multiple = TRUE
              ),
              
              br(),
              shinyWidgets::awesomeCheckboxGroup(
                ns("human_IgA_N144_traits"),
                "Select traits to calculate for IgA1/2 glycosylation site N144/N131:",
                choices = c(
                  "Fucosylation of complex-type glycans",
                  "Bisection of complex-type glycans",
                  "Galactosylation per antenna of complex-type glycans",
                  "Sialylation per antenna of complex-type glycans",
                  "Sialylation per galactose of complex-type glycans",
                  "Percentage of hybrid-type glycans",
                  "Percentage of oligomannose-type glycans",
                  "Oligomannose-type glycans: average number of mannoses"
                )
              ),
              selectizeInput(
                ns("human_IgA_N144_clusters"),
                "Select clusters for which these traits should be calculated:",
                choices = c(""),
                multiple = TRUE
              ),
              
              br(),
              shinyWidgets::awesomeCheckboxGroup(
                ns("human_IgA_N205_traits"),
                "Select traits want to calculate for IgA2 glycosylation site N205:",
                choices = c(
                  "Fucosylation of complex-type glycans",
                  "Bisection of complex-type glycans",
                  "Galactosylation per antenna of complex-type glycans",
                  "Sialylation per antenna of complex-type glycans",
                  "Sialylation per galactose of complex-type glycans"
                )
              ),
              selectizeInput(
                ns("human_IgA_N205_clusters"),
                "Select clusters for which these traits should be calculated:",
                choices = c(""),
                multiple = TRUE
              ),
              
              br(),
              shinyWidgets::awesomeCheckboxGroup(
                ns("human_IgA_N340_traits"),
                "Select traits to calculate for IgA1/2 glycosylation site N340/N327:",
                choices = c(
                  "Fucosylation of complex-type glycans",
                  "Bisection of complex-type glycans",
                  "Galactosylation per antenna of complex-type glycans",
                  "Sialylation per antenna of complex-type glycans",
                  "Sialylation per galactose of complex-type glycans",
                  "Percentage of triantennary complex-type glycans",
                  "Percentage of oligomannose-type glycans",
                  "Oligomannose-type glycans: average number of mannoses"
                )
              ),
              selectizeInput(
                ns("human_IgA_N340_clusters"),
                "Select clusters for which these traits should be calculated:",
                choices = c(""),
                multiple = TRUE
              )
            )),
            
            # Human IgA O-glycans
            tabPanel("Human IgA: O-glycans", tagList(
              br(),
              shinyWidgets::awesomeCheckboxGroup(
                ns("human_IgA_O_traits"),
                "Select traits to calculate:",
                choices = c(
                  "Sialic acids",
                  "Galactoses",
                  "GalNAcs",
                  "Sialic acids per galactose",
                  "Galactoses per GalNAc",
                  "Tn antigens",
                  "T antigens",
                  "Sialyl-T (sT) antigens",
                  "Disialylated O-antigens"
                )
              ),
              selectizeInput(
                ns("human_IgA_O_clusters"),
                "Select clusters for which these traits should be calculated:",
                choices = c(""),
                multiple = TRUE
              )
            )),
            
            # Human IgM N-glycans per glycosylation site
            tabPanel("Human IgM: N-glycans", tagList(
              br(),
              shinyWidgets::awesomeCheckboxGroup(
                ns("human_IgM_N46_traits"),
                "Select traits to calculate for IgM glycosylation site N46:",
                choices = c(
                  "Fucosylation of complex-type glycans",
                  "Bisection of complex-type glycans",
                  "Galactosylation per antenna of complex-type glycans",
                  "Sialylation per antenna of complex-type glycans",
                  "Sialylation per galactose of complex-type glycans",
                  "Antennarity of complex-type glycans",
                  "Percentage of hybrid-type glycans",
                  "Fucosylation of hybrid-type glycans",
                  "Bisection of hybrid-type glycans",
                  "Percentage of oligomannose-type glycans",
                  "Oligomannose-type glycans: average number of mannoses"
                )
              ),
              selectizeInput(
                ns("human_IgM_N46_clusters"),
                "Select clusters for which these traits should be calculated:",
                choices = c(""),
                multiple = TRUE
              ),
              
              br(),
              shinyWidgets::awesomeCheckboxGroup(
                ns("human_IgM_N209_traits"),
                "Select traits to calculate for IgM glycosylation site N209:",
                choices = c(
                  "Core fucosylation of complex-type glycans",
                  "Antennary fucosylation of complex-type glycans",
                  "Bisection of complex-type glycans",
                  "Galactosylation per antenna of complex-type glycans",
                  "Sialylation per antenna of complex-type glycans",
                  "Sialylation per galactose of complex-type glycans",
                  "Percentage of triantennary complex-type glycans"
                )
              ),
              selectizeInput(
                ns("human_IgM_N209_clusters"),
                "Select clusters for which these traits should be calculated:",
                choices = c(""),
                multiple = TRUE
              ),
              
              br(),
              shinyWidgets::awesomeCheckboxGroup(
                ns("human_IgM_N272_traits"),
                "Select traits to calculate for IgM glycosylation site N272:",
                choices = c(
                  "Core fucosylation of complex-type glycans",
                  "Antennary fucosylation of complex-type glycans",
                  "Bisection of complex-type glycans",
                  "Galactosylation per antenna of complex-type glycans",
                  "Sialylation per antenna of complex-type glycans",
                  "Sialylation per galactose of complex-type glycans",
                  "Percentage of triantennary complex-type glycans"
                )
              ),
              selectizeInput(
                ns("human_IgM_N272_clusters"),
                "Select clusters for which these traits should be calculated:",
                choices = c(""),
                multiple = TRUE
              ),
              
              br(),
              shinyWidgets::awesomeCheckboxGroup(
                ns("human_IgM_N279_traits"),
                "Select traits to calculate for IgM glycosylation site N279:",
                choices = c(
                  "Fucosylation of complex-type glycans",
                  "Antennarity of complex-type glycans",
                  "Percentage of hybrid-type glycans",
                  "Fucosylation of hybrid-type glycans",
                  "Percentage of oligomannose-type glycans",
                  "Oligomannose-type glycans: average number of mannoses"
                )
              ),
              selectizeInput(
                ns("human_IgM_N279_clusters"),
                "Select clusters for which these traits should be calculated:",
                choices = c(""),
                multiple = TRUE
              ),
              
              br(),
              shinyWidgets::awesomeCheckboxGroup(
                ns("human_IgM_N440_traits"),
                "Select traits to calculate for IgM glycosylation site N440:",
                choices = c(
                  "Oligomannose-type glycans: average number of mannoses"
                )
              ),
              selectizeInput(
                ns("human_IgM_N440_clusters"),
                "Select clusters for which these traits should be calculated:",
                choices = c(""),
                multiple = TRUE
              )
            )),
            
            
            # Human JC N-glycans
            tabPanel("Human Joining Chain: N-glycans", tagList(
              br(),
              shinyWidgets::awesomeCheckboxGroup(
                ns("human_JC_N_traits"),
                "Select traits to calculate:",
                choices = c(
                  "Fucosylation of complex-type glycans",
                  "Bisection of complex-type glycans",
                  "Galactosylation per antenna of complex-type glycans",
                  "Sialylation per antenna of complex-type glycans",
                  "Sialylation per galactose of complex-type glycans",
                  "Percentage of monoantennary complex-type glycans",
                  "Percentage of hybrid-type glycans"
                )
              ),
              selectizeInput(
                ns("human_JC_N_clusters"),
                "Select clusters for which these traits should be calculated:",
                choices = c(""),
                multiple = TRUE
              )
            )),
            
            # Mouse IgG tab
            tabPanel("Mouse IgG: N-glycans", tagList(
              br(),
              shinyWidgets::awesomeCheckboxGroup(
                ns("mouse_IgG_N_traits"),
                "Select traits to calculate:",
                choices = c(
                  "Fucosylation of complex-type glycans",
                  "Bisection of complex-type glycans",
                  "Galactosylation per antenna of complex-type glycans",
                  "Sialylation (N-glycolylneuraminic acid) per antenna of complex-type glycans",
                  "\u03B1-1,3-galactosylation of complex-type glycans",
                  "Percentage of monoantennary complex-type glycans",
                  "Percentage of hybrid-type glycans",
                  "Percentage of oligomannose-type glycans",
                  "Oligomannose-type glycans: average number of mannoses"
                )
              ),
              selectizeInput(
                ns("mouse_IgG_N_clusters"),
                "Select clusters for which these traits should be calculated:",
                choices = c(""),
                multiple = TRUE
              )
            ))
          ),
          br(),
          actionButton(ns("button"), "Calculate traits")
        ),
        
        shinydashboard::box(
          title = "Formulas used to automatically calculate the glycosylation traits",
          width = 7,
          solidHeader = TRUE,
          status = "primary",
          downloadButton(ns("download_formulas"), "Download as Excel file"),
          br(),
          br(),
          DT::dataTableOutput(ns("formulas"))
        ),
      ),
      
      fluidRow(
        shinydashboardPlus::box(
          id = ns("box"),
          title = div(
            id = ns("box_header"),
            "Automatically calculated traits plotted against total spectrum intensities",
            icon("info-circle", class = "ml") %>%
              bsplus::bs_embed_popover(
                title = "Explanation",
                content = HTML(
                  "
                As a sanity check, automatically calculated glycosylation traits
                are plotted against total spectrum intensities. Correlations
                between traits and total spectrum intensities for standards
                (e.g. VisuCon or Pool) indicate that differences in traits between
                samples are (at least partly) a technical artefact, rather than 
                a biological effect.
                "
                ),
                trigger = "hover",
                placement = "left",
                html = "true",
                container = "body"
              )
            ),
          width = 12,
          solidHeader = TRUE,
          status = "primary",
          tabsetPanel(id = ns("intensity_plots"))
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
                  The Excel file must contain one column called \"trait\" that
                  specifies the names of the traits. The second column must be called
                  \"formula\" and should contain the formulas for the traits. Analyte names
                  in the formula should include both the peptide name and glycan composition, 
                  e.g. \"IgGI1H4N4F1\".
                  <br> <br>
                  <strong>Trait names should not contain any spaces.</strong>
                  <br> <br>
                  For an example file, click the paperclip icon.
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
          title = "Formulas used to calculate the custom glycosylation traits",
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
    
    normalized_data_wide <- reactive({
      req(results_normalization$normalized_data_wide())
      results_normalization$normalized_data_wide()
    })
    
    # Toggle visibility of tabs, depending on input$antibody_types
    observeEvent(input$antibody_types, {
      purrr::map(
        c("Human IgG: N-glycans", "Human IgA: N-glycans", "Human IgA: O-glycans", 
          "Human IgM: N-glycans", "Human Joining Chain: N-glycans", "Mouse IgG: N-glycans"), 
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
      for (id in c("human_IgG_N_clusters", "mouse_IgG_N_clusters", 
                   "human_IgA_N47_clusters", "human_IgA_N144_clusters", 
                   "human_IgA_N205_clusters", "human_IgA_N340_clusters", 
                   "human_IgA_O_clusters", "human_JC_N_clusters",
                   "human_IgM_N46_clusters", "human_IgM_N209_clusters", 
                   "human_IgM_N272_clusters", "human_IgM_N279_clusters", 
                   "human_IgM_N440_clusters")) {
        updateSelectizeInput(id, choices = clusters(), session = session)
      }
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
        "Please upload a .xlsx or .xls file."
      )
    })
    
    # Read the custom traits Excel file as a data frame.
    traits_excel <- reactive({
      req(input$custom_traits_file, extension(), extension() == "xlsx")
      readxl::read_excel(input$custom_traits_file$datapath, col_names = TRUE, col_types = "text")
    })

    
    # Check if traits_excel is formatted correctly
    r <- reactiveValues()
    observeEvent(traits_excel(), {
      r$correct_formatting <- TRUE
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
          r$correct_formatting <- FALSE
        }
      }
    }, priority = 10)

    # Calculate the custom traits
    data_with_custom_traits <- reactive({
      req(traits_excel(), normalized_data(), r$correct_formatting == TRUE)
      tryCatch({
        calculate_custom_traits(traits_excel(), normalized_data_wide())
      }, error = function(e) {
        shinyalert::shinyalert(
          text = "One or more of your formulas contain non-existing analytes. Please check your file and try again.",
          confirmButtonCol = "tomato"
        )
        NULL
      })
    })
    

    ####################  Default glycosylation traits  ####################
    
    # If user selects sialylation per galactose, then sialylation and galactosylation
    # must both be selected automatically if not yet done so.
    # (Perhaps this is not the most optimal code)
    to_listen <- reactive({
      list(
        input$human_IgG_N_traits,
        input$human_IgA_N47_traits,
        input$human_IgA_N144_traits,
        input$human_IgA_N205_traits,
        input$human_IgA_N340_traits,
        input$human_IgM_N46_traits,
        input$human_IgM_N209_traits,
        input$human_IgM_N272_traits,
        input$human_JC_N_traits
      )
    })
    
    observeEvent(to_listen(), {
      ids <- c(
        "human_IgG_N_traits",
        "human_IgA_N47_traits",
        "human_IgA_N144_traits",
        "human_IgA_N205_traits",
        "human_IgA_N340_traits",
        "human_IgM_N46_traits",
        "human_IgM_N209_traits",
        "human_IgM_N272_traits",
        "human_JC_N_traits"
      )
      for (i in seq(length(ids))) {
        id <- ids[[i]]
        selected_traits <- input[[id]]
        if ("Sialylation per galactose of complex-type glycans" %in% selected_traits) {
          if (!all("Sialylation per antenna of complex-type glycans" %in% selected_traits,
                   "Galactosylation per antenna of complex-type glycans" %in% selected_traits)) {
            shinyWidgets::updateAwesomeCheckboxGroup(
              inputId = id,
              selected = unique(c(
                selected_traits,
                "Sialylation per antenna of complex-type glycans",
                "Galactosylation per antenna of complex-type glycans"
              ))
            )
          }
        }
      }
    })
    
    
    # Also check O-glycans: sialic acids per galactose, and galactoses per GalNAc
    observeEvent(input$human_IgA_O_traits, {
      selected_traits <- input$human_IgA_O_traits
      if (all("Sialic acids per galactose" %in% selected_traits,
              "Galactoses per GalNAc" %in% selected_traits)) {
        if (!all("Sialic acids" %in% selected_traits,
                 "Galactoses" %in% selected_traits,
                 "GalNAcs" %in% selected_traits)) {
          shinyWidgets::updateAwesomeCheckboxGroup(
            inputId = "human_IgA_O_traits",
            selected = unique(c(selected_traits, "Sialic acids", "Galactoses", "GalNAcs"))
          )
        }
      }
      else if ("Sialic acids per galactose" %in% selected_traits) {
        if (!all("Sialic acids" %in% selected_traits, "Galactoses" %in% selected_traits)) {
          shinyWidgets::updateAwesomeCheckboxGroup(
            inputId = "human_IgA_O_traits",
            selected = unique(c(selected_traits, "Sialic acids", "Galactoses"))
          )
        }
      }
      else if ("Galactoses per GalNAc" %in% selected_traits) {
        if (!all("Galactoses" %in% selected_traits, "GalNAcs" %in% selected_traits)) {
          shinyWidgets::updateAwesomeCheckboxGroup(
            inputId = "human_IgA_O_traits",
            selected = unique(c(selected_traits, "Galactoses", "GalNAcs"))
          )
        }
      }
    })
    
    
  
    ################# DETERMINE FORMULAS FOR DEFAULT TRAITS #########################
    
    #TODO: Reduce the code below
    
    # Trait formulas for human IgG
    human_IgG_N_traits <- reactive({
      req(input$human_IgG_N_traits)
      match_traits(input$human_IgG_N_traits)
    })

    human_IgG_N_formulas <- reactive({
      req(length(input$human_IgG_N_clusters) > 0, is_truthy(human_IgG_N_traits()))
      load(system.file("app", "www", "human_IgG_N_ref.rda", package = "GlycoDash"))
      purrr::reduce(
        create_formula_list(
          normalized_data(), human_IgG_N_traits(), input$human_IgG_N_clusters, human_IgG_N_ref
        ), c  # c = concatenate
      )
    })

    # Trait formulas for human IgA N-glycans
    human_IgA_N47_traits <- reactive({
      req(input$human_IgA_N47_traits)
      match_traits(input$human_IgA_N47_traits)
    })

    human_IgA_N47_formulas <- reactive({
      req(length(input$human_IgA_N47_clusters) > 0, is_truthy(human_IgA_N47_traits()))
      load(system.file("app", "www", "human_IgA_N47_ref.rda", package = "GlycoDash"))
      purrr::reduce(
        create_formula_list(
          normalized_data(), human_IgA_N47_traits(), input$human_IgA_N47_clusters, human_IgA_N47_ref
        ), c  # c = concatenate
      )
    })

    human_IgA_N144_traits <- reactive({
      req(input$human_IgA_N144_traits)
      match_traits(input$human_IgA_N144_traits)
    })

    human_IgA_N144_formulas <- reactive({
      req(length(input$human_IgA_N144_clusters) > 0, is_truthy(human_IgA_N144_traits()))
      load(system.file("app", "www", "human_IgA_N144_ref.rda", package = "GlycoDash"))
      purrr::reduce(
        create_formula_list(
          normalized_data(), human_IgA_N144_traits(), input$human_IgA_N144_clusters, human_IgA_N144_ref
        ), c  # c = concatenate
      )
    })

    human_IgA_N205_traits <- reactive({
      req(input$human_IgA_N205_traits)
      match_traits(input$human_IgA_N205_traits)
    })

    human_IgA_N205_formulas <- reactive({
      req(length(input$human_IgA_N205_clusters) > 0, is_truthy(human_IgA_N205_traits()))
      load(system.file("app", "www", "human_IgA_N205_ref.rda", package = "GlycoDash"))
      purrr::reduce(
        create_formula_list(
          normalized_data(), human_IgA_N205_traits(), input$human_IgA_N205_clusters, human_IgA_N205_ref
        ), c  # c = concatenate
      )
    })

    human_IgA_N340_traits <- reactive({
      req(input$human_IgA_N340_traits)
      match_traits(input$human_IgA_N340_traits)
    })

    human_IgA_N340_formulas <- reactive({
      req(length(input$human_IgA_N340_clusters) > 0, is_truthy(human_IgA_N340_traits()))
      load(system.file("app", "www", "human_IgA_N340_ref.rda", package = "GlycoDash"))
      purrr::reduce(
        create_formula_list(
          normalized_data(), human_IgA_N340_traits(), input$human_IgA_N340_clusters, human_IgA_N340_ref
        ), c  # c = concatenate
      )
    })


    # Trait formulas for human IgA O-glycans
    human_IgA_O_traits <- reactive({
      req(input$human_IgA_O_traits)
      match_traits(input$human_IgA_O_traits)
    })

    human_IgA_O_formulas <- reactive({
      req(length(input$human_IgA_O_clusters) > 0, is_truthy(human_IgA_O_traits()))
      load(system.file("app", "www", "human_IgA_O_ref.rda", package = "GlycoDash"))
      purrr::reduce(
        create_formula_list(
          normalized_data(), human_IgA_O_traits(), input$human_IgA_O_clusters, human_IgA_O_ref
        ), c  # c = concatenate
      )
    })
  
    
    # Trait formulas for human IgM N-glycans
    human_IgM_N46_traits <- reactive({
      req(input$human_IgM_N46_traits)
      match_traits(input$human_IgM_N46_traits)
    })
    
    human_IgM_N46_formulas <- reactive({
      req(length(input$human_IgM_N46_clusters) > 0, is_truthy(human_IgM_N46_traits()))
      load(system.file("app", "www", "human_IgM_N46_ref.rda", package = "GlycoDash"))
      purrr::reduce(
        create_formula_list(
          normalized_data(), human_IgM_N46_traits(), input$human_IgM_N46_clusters, human_IgM_N46_ref
        ), c  # c = concatenate
      )
    })
    
    human_IgM_N209_traits <- reactive({
      req(input$human_IgM_N209_traits)
      match_traits(input$human_IgM_N209_traits)
    })
    
    human_IgM_N209_formulas <- reactive({
      req(length(input$human_IgM_N209_clusters) > 0, is_truthy(human_IgM_N209_traits()))
      load(system.file("app", "www", "human_IgM_N209_ref.rda", package = "GlycoDash"))
      purrr::reduce(
        create_formula_list(
          normalized_data(), human_IgM_N209_traits(), input$human_IgM_N209_clusters, human_IgM_N209_ref
        ), c  # c = concatenate
      )
    })
    
    human_IgM_N272_traits <- reactive({
      req(input$human_IgM_N272_traits)
      match_traits(input$human_IgM_N272_traits)
    })
    
    human_IgM_N272_formulas <- reactive({
      req(length(input$human_IgM_N272_clusters) > 0, is_truthy(human_IgM_N272_traits()))
      load(system.file("app", "www", "human_IgM_N272_ref.rda", package = "GlycoDash"))
      purrr::reduce(
        create_formula_list(
          normalized_data(), human_IgM_N272_traits(), input$human_IgM_N272_clusters, human_IgM_N272_ref
        ), c  # c = concatenate
      )
    })
    
    human_IgM_N279_traits <- reactive({
      req(input$human_IgM_N279_traits)
      match_traits(input$human_IgM_N279_traits)
    })
    
    human_IgM_N279_formulas <- reactive({
      req(length(input$human_IgM_N279_clusters) > 0, is_truthy(human_IgM_N279_traits()))
      load(system.file("app", "www", "human_IgM_N279_ref.rda", package = "GlycoDash"))
      purrr::reduce(
        create_formula_list(
          normalized_data(), human_IgM_N279_traits(), input$human_IgM_N279_clusters, human_IgM_N279_ref
        ), c  # c = concatenate
      )
    })
    
    human_IgM_N440_traits <- reactive({
      req(input$human_IgM_N440_traits)
      match_traits(input$human_IgM_N440_traits)
    })
    
    human_IgM_N440_formulas <- reactive({
      req(length(input$human_IgM_N440_clusters) > 0, is_truthy(human_IgM_N440_traits()))
      load(system.file("app", "www", "human_IgM_N440_ref.rda", package = "GlycoDash"))
      purrr::reduce(
        create_formula_list(
          normalized_data(), human_IgM_N440_traits(), input$human_IgM_N440_clusters, human_IgM_N440_ref
        ), c  # c = concatenate
      )
    })
    

    # Trait formulas for human JC
    human_JC_N_traits <- reactive({
      req(input$human_JC_N_traits)
      match_traits(input$human_JC_N_traits)
    })

    human_JC_N_formulas <- reactive({
      req(length(input$human_JC_N_clusters) > 0, is_truthy(human_JC_N_traits()))
      load(system.file("app", "www", "human_JC_N_ref.rda", package = "GlycoDash"))
      purrr::reduce(
        create_formula_list(
          normalized_data(), human_JC_N_traits(), input$human_JC_N_clusters, human_JC_N_ref
        ), c  # c = concatenate
      )
    })


    # Trait formulas for mouse IgG
    mouse_IgG_N_traits <- reactive({
      req(input$mouse_IgG_N_traits)
      match_traits(input$mouse_IgG_N_traits)
    })

    mouse_IgG_N_formulas <- reactive({
      req(length(input$mouse_IgG_N_clusters) > 0, is_truthy(mouse_IgG_N_traits()))
      load(system.file("app", "www", "mouse_IgG_N_ref.rda", package = "GlycoDash"))
      purrr::reduce(
        create_formula_list(
          normalized_data(), mouse_IgG_N_traits(), input$mouse_IgG_N_clusters, mouse_IgG_N_ref
        ), c  # c = concatenate
      )
    })
    
    ##########################################################################
    
    
    # Combine the trait formulas
    trait_formulas <- reactive({
      
      # Initiate empty vector to append formulas to
      formulas <- c() 
      
      # Check each possible "formulas" reactive.
      # N-glycans: check if sialylation per galactose should be calculated.
      # O-glycans: check if sialic acids per galactose or galactose per GalNAc should be calculated.
      if (is_truthy(human_IgG_N_formulas())) {
        formulas <- c(formulas, human_IgG_N_formulas())
        if ("Sialylation per galactose of complex-type glycans" %in% input$human_IgG_N_traits) {
          formulas <- c(
            formulas, purrr::map(input$human_IgG_N_clusters, ~ paste0(
              .x, "_sialylation_per_galactose = ", .x, "_sialylation / ", .x, "_galactosylation * 100"
            ))
          )
        }
      }
      if (is_truthy(human_IgA_N47_formulas())) {
        formulas <- c(formulas, human_IgA_N47_formulas())
        if ("Sialylation per galactose of complex-type glycans" %in% input$human_IgA_N47_traits) {
          formulas <- c(
            formulas, purrr::map(input$human_IgA_N47_clusters, ~ paste0(
              .x, "_sialylation_per_galactose = ", .x, "_sialylation / ", .x, "_galactosylation * 100"
            ))
          )
        }
      }
      if (is_truthy(human_IgA_N144_formulas())) {
        formulas <- c(formulas, human_IgA_N144_formulas())
        if ("Sialylation per galactose of complex-type glycans" %in% input$human_IgA_N144_traits) {
          formulas <- c(
            formulas, purrr::map(input$human_IgA_N144_clusters, ~ paste0(
              .x, "_sialylation_per_galactose = ", .x, "_sialylation / ", .x, "_galactosylation * 100"
            ))
          )
        }
      }
      if (is_truthy(human_IgA_N205_formulas())) {
        formulas <- c(formulas, human_IgA_N205_formulas())
        if ("Sialylation per galactose of complex-type glycans" %in% input$human_IgA_N205_traits) {
          formulas <- c(
            formulas, purrr::map(input$human_IgA_N205_clusters, ~ paste0(
              .x, "_sialylation_per_galactose = ", .x, "_sialylation / ", .x, "_galactosylation * 100"
            ))
          )
        }
      }
      if (is_truthy(human_IgA_N340_formulas())) {
        formulas <- c(formulas, human_IgA_N340_formulas())
        if ("Sialylation per galactose of complex-type glycans" %in% input$human_IgA_N340_traits) {
          formulas <- c(
            formulas, purrr::map(input$human_IgA_N340_clusters, ~ paste0(
              .x, "_sialylation_per_galactose = ", .x, "_sialylation / ", .x, "_galactosylation * 100"
            ))
          )
        }
      }
      if (is_truthy(human_IgA_O_formulas())) {
        formulas <- c(formulas, human_IgA_O_formulas())
        if ("Sialic acids per galactose" %in% input$human_IgA_O_traits) {
          formulas <- c(
            formulas, purrr::map(input$human_IgA_O_clusters, ~ paste0(
              .x, "_sialic_acids_per_galactose = ", .x, "_sialic_acids / ", .x, "_galactoses"
            ))
          )
        }
        if ("Galactoses per GalNAc" %in% input$human_IgA_O_traits) {
          formulas <- c(
            formulas, purrr::map(input$human_IgA_O_clusters, ~ paste0(
              .x, "_galactoses_per_galnac = ", .x, "_galactoses / ", .x, "_galnacs"
            ))
          )
        }
      }
      if (is_truthy(human_JC_N_formulas())) {
        formulas <- c(formulas, human_JC_N_formulas())
        if ("Sialylation per galactose of complex-type glycans" %in% input$human_JC_N_traits) {
          formulas <- c(
            formulas, purrr::map(input$human_JC_N_clusters, ~ paste0(
              .x, "_sialylation_per_galactose = ", .x, "_sialylation / ", .x, "_galactosylation * 100"
            ))
          )
        }
      }
      if (is_truthy(human_IgM_N46_formulas())) {
        formulas <- c(formulas, human_IgM_N46_formulas())
        if ("Sialylation per galactose of complex-type glycans" %in% input$human_IgM_N46_traits) {
          formulas <- c(
            formulas, purrr::map(input$human_IgM_N46_clusters, ~ paste0(
              .x, "_sialylation_per_galactose = ", .x, "_sialylation / ", .x, "_galactosylation * 100"
            ))
          )
        }
      }
      if (is_truthy(human_IgM_N209_formulas())) {
        formulas <- c(formulas, human_IgM_N209_formulas())
        if ("Sialylation per galactose of complex-type glycans" %in% input$human_IgM_N209_traits) {
          formulas <- c(
            formulas, purrr::map(input$human_IgM_N209_clusters, ~ paste0(
              .x, "_sialylation_per_galactose = ", .x, "_sialylation / ", .x, "_galactosylation * 100"
            ))
          )
        }
      }
      if (is_truthy(human_IgM_N272_formulas())) {
        formulas <- c(formulas, human_IgM_N272_formulas())
        if ("Sialylation per galactose of complex-type glycans" %in% input$human_IgM_N272_traits) {
          formulas <- c(
            formulas, purrr::map(input$human_IgM_N272_clusters, ~ paste0(
              .x, "_sialylation_per_galactose = ", .x, "_sialylation / ", .x, "_galactosylation * 100"
            ))
          )
        }
      }
      if (is_truthy(human_IgM_N279_formulas())) {
        formulas <- c(formulas, human_IgM_N279_formulas())
      }
      if (is_truthy(human_IgM_N440_formulas())) {
        formulas <- c(formulas, human_IgM_N440_formulas())
      }
      if (is_truthy(mouse_IgG_N_formulas())) {
        formulas <- c(formulas, mouse_IgG_N_formulas())
      }
      
      return(formulas)
    })
    
    # Set button status
    observe({
      shinyjs::toggleState("button", length(trait_formulas()) > 0)
    })
    
    # Calculate traits when user pushes button
    data_with_derived_traits <- reactive({
      req(trait_formulas())
      trait_formulas_toreport <- trait_formulas()[!grepl(" = Not reported", trait_formulas())]
      calculate_traits(normalized_data_wide(), trait_formulas_toreport)
    }) %>% bindEvent(input$button)  # Calculate after pushing button

    
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
    # Otherwise: just normalized data in wide format
    with_data <- reactive({
      if (is_truthy(data_with_all_traits())) {
        data_with_all_traits()
      } else if (is_truthy(data_with_derived_traits())) {
        data_with_derived_traits()
      } else if (is_truthy(data_with_custom_traits())) {
        data_with_custom_traits()
      } else if (is_truthy(normalized_data_wide())) {
        normalized_data_wide()
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
      # Disabling server-side rendering seems to prevent error messages being
      # thrown by the browser, when quickly selecting and deselecting traits.
      # But it does make rendering somewhat slower.
      server = FALSE
      DT::datatable(data = data_with_traits() %>% 
                      dplyr::mutate_if(is.numeric, ~format(round(., 2), nsmall = 2)),
                    options = list(
                      scrollX = TRUE,
                      pageLength = 6,
                      columnDefs = list(list(className = "dt-center", targets = "_all"))
                    ), filter = "top")
    })
    
    
    ########## Plot the traits vs total spectrum intensities #################
    
    # List with names of traits per cluster
    cluster_traits <- reactive({
      req(data_with_traits())
      columns <- colnames(data_with_traits())
      # Use map to create a list of trait names for each cluster
      cluster_traits <- purrr::map(clusters(), function(cluster) {
        substring <- paste0(cluster, "_")
        columns[grepl(substring, columns) & !grepl("_sum_intensity", columns)
                & !grepl("_quantity_ng", columns)]
      })
      # Turn into a named list
      names(cluster_traits) <- clusters()
      return(cluster_traits)
    })
    
    
    # Create a tab for each name in cluster_traits that is not empty
    # created_tabs <- reactiveValues(tabs = )
    intensity_plots <- reactiveValues(plots = NULL)
    observeEvent(cluster_traits(), {
      # Remove previously generated tabs and plots
      for (cluster in clusters()) {
        removeTab(inputId = "intensity_plots", target = cluster)
      } 
      intensity_plots$plots <- NULL
      # Generate tabs with plots
      non_empty_clusters <- names(purrr::keep(cluster_traits(), ~ length(.x) > 0))
      if (length(non_empty_clusters) > 0) {
        for (cluster in non_empty_clusters) {
          # Create tab
          appendTab(
            inputId = "intensity_plots", 
            select = TRUE,
            session = session,
            tab = tabPanel(
              title = cluster,
              mod_tab_intensities_ui(ns(cluster))
            )
          )
          # Create plot
          intensity_plots$plots[[cluster]] <- mod_tab_intensities_server(
            id = cluster,
            data = data_with_traits(),
            traits = cluster_traits()[[cluster]]
          )$intensity_plot()
        }
      }
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
      req(traits_excel(), data_with_custom_traits())
      DT::datatable(traits_excel(),
                    rownames = FALSE, 
                    options = list(paging = FALSE,
                                  ordering = FALSE,
                                  searching = FALSE))
    })
    
    
    # Display the formulas of the default traits
    formulas_table <- reactive({
      req(trait_formulas())
      formula_dfs <- vector("list", length = length(trait_formulas()))
      for (i in seq(length(trait_formulas()))) {
        trait_formula <- trait_formulas()[i]
        trait_name <- names(create_expr_ls(trait_formula))
        calculation <- stringr::str_remove(trait_formula, paste0(trait_name, " = "))
        formula_dfs[[i]] <- data.frame(trait = trait_name, formula = calculation)
      }
      purrr::reduce(formula_dfs, dplyr::full_join)
    }) %>% bindEvent(data_with_derived_traits()) # Update table after calculation of traits
    
    output$formulas <- DT::renderDT({
      req(formulas_table())
      levels <- c("", unique(formulas_table()$formula[grepl("Not reported", formulas_table()$formula)]))
      DT::datatable(formulas_table(), rownames = FALSE, filter = "top") %>% 
        # Highlight traits that are not reported
        DT::formatStyle(
          "formula",
          target = "row",
          backgroundColor = DT::styleEqual(
            levels = levels, # This contains "" to prevent character(0)
            rep("gold", length(levels))
          )
        )
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
        writexl::write_xlsx(
          formulas_table() %>% 
            dplyr::filter(!grepl("Not reported", formula)), 
          path = file
        )
      }
    )
    
    return(
      list(
        data_with_traits = data_with_traits,
        normalized_data = normalized_data,
        derived_traits = reactive({ input$traits_menu }),
        formulas = formulas_table,
        custom_traits_excel = traits_excel,
        intensity_plots = reactive(intensity_plots$plots)
      )
    )
 
  })
}
