#' analyte_curation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_analyte_curation_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        h1("Analyte curation")
      ),
      fluidRow(
        column(
          width = 6,
          shinydashboard::box(
            title = "Method for analyte curation",
            width = NULL,
            solidHeader = TRUE,
            status = "primary",
            selectInput(ns("method"), 
                        "Choose method for analyte curation:",
                        choices = c("Supply an analyte list", 
                                    "Curate analytes based on data")),
            tags$style(
              HTML(paste0("#",
                          ns("analyte_list_div"),
                          " .popover {width: 400px;}"))
            ),
            div(id = ns("analyte_list_div"),
                fileInput(ns("analyte_list"), 
                          "Upload an Excel file or R object with an analyte list") %>% 
                  bsplus::bs_embed_popover(
                    title = "Explanation",
                    content = HTML(paste(
                      tags$b("Excel file:"),
                      tags$p(paste(
                        "The file should consist of one column with the names of the",
                        "analytes that you want to keep. A column name is not required."
                      )),
                      tags$b("R object:"),
                      tags$p(paste(
                        "The R object should be a character vector or a list of",
                        "character strings (not a dataframe!) with the names of",
                        "the analytes that you want to keep."
                      ))
                    )),
                    html = "true",
                    trigger = "hover",
                    placement = "right")
            ),
            tags$style(
              HTML(paste0("#",
                          ns("curation_based_on_data"),
                          " .popover {width: 400px;}"))
            ),
            div(
              id = ns("curation_based_on_data"),
              selectizeInput(ns("ignore_samples"),
                             "Sample types to ignore regarding analyte curation:",
                             choices = c("Total", "Blanks", "Negative controls"),
                             multiple = TRUE) %>% 
                bsplus::bs_embed_popover(
                  title = "Explanation",
                  content = HTML(paste0(
                    tags$p(paste(
                      "Analytes are curated based on the percentage of spectra",
                      "in which they pass the analyte quality criteria (go to \"",
                      "Spectra curation\" to choose these criteria)."
                    )),
                    tags$p(paste(
                      "However, some spectra (e.g. blanks, standards or total", 
                      "Ig samples) should not be included in this assesment."
                    )),
                    tags$p(paste(
                      "Select here which samples should be ignored with regards",
                      "to analyte curation."
                    ))
                  )),
                  placement = "right",
                  trigger = "hover",
                  html = "true"),
              numericInput(ns("cut_off"), "Cut-off (%)", value = 25) %>% 
                bsplus::bs_embed_popover(
                  title = "Explanation",
                  content = HTML(paste0(
                    tags$p(paste(
                      "Choose the percentage of spectra in which an analyte",
                      "needs to fulfill the quality criteria in order to pass", 
                      "analyte curation."
                    ))
                  )),
                  placement = "right",
                  trigger = "hover",
                  html = "true")#,
              # shinyWidgets::materialSwitch(
              #   ns("per_bio_group"),
              #   "Perform analyte curation separately per biological group.",
              #   status = "primary",
              #   right = TRUE
              # ),
              # selectInput(ns("bio_group"),
              #             label = "Choose which variable corresponds to the biological group:",
              #             choices = "")
              # selectInput to choose the variable to use for biological groups
              # needs to be updated to contain all colnames in the data
              # popup to accept the groups?
            ),
            actionButton(ns("curate_analytes"), 
                         "Perform analyte curation")
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
                           "Download analyte-curated data")
          )
        ),
        column(
          width = 6,
          
        )
      ),
      fluidRow(
        div(
          id = ns("tabbed_box"),
          shinydashboard::box(
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            title = "Information on analyte curation per cluster",
            #uiOutput(ns("information")),
            tabsetPanel(id = ns("tabs"))
          )
        )
      )
    )
  )
}
    
#' analyte_curation Server Functions
#'
#' @noRd 
mod_analyte_curation_server <- function(id, results_spectra_curation){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    x <- reactiveValues()
    
    summary <- reactive({
      req(results_spectra_curation$curated_spectra())
      results_spectra_curation$curated_spectra()
    })
    
    observe({
      shinyjs::toggle("analyte_list", 
                      condition = input$method == "Supply an analyte list")
      shinyjs::toggle("curation_based_on_data", 
                      condition = input$method == "Curate analytes based on data")
      shinyjs::toggleState("curate_analytes", 
                           condition = 
                             (input$method == "Supply an analyte list" & 
                             !is.null(input$analyte_list)) | 
                             (input$method == "Curate analytes based on data"))
    })
    
    # observe({
    #   req(summary())
    #   
    #   updateSelectInput(
    #     session = session,
    #     inputId = "bio_group",
    #     choices = colnames(summary())
    #   )
    # })
    # 
    # observe({
    #   shinyjs::toggle("bio_group",
    #                   condition = input$per_bio_group)
    # })
    
    clusters <- reactive({
      unique(summary()$cluster)
    })
    
    info <- list(
      curated_analytes = reactive(x$curated_analytes),
      cut_off = reactive(input$cut_off),
      analyte_curated_data = reactive(x$analyte_curated_data),
      method = reactive(input$method)
    )
    
    # bio_groups <- reactive({
    #   req(summary(),
    #       input$bio_group)
    #   
    #   unique(summary()[[input$bio_group]])
    # })
    
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
                               mod_information_box_ui(ns(cluster))
                             ))
                 })
      
    })
    
    observe({
      req(clusters())
      purrr::map(info,
                 ~ req(.x))
      
      x$mod_results <- purrr::set_names(clusters()) %>% 
        purrr::map(
          .,
          function(cluster) {
            mod_information_box_server(id = cluster,
                                       info = info,
                                       cluster = cluster)
          })
    })
    
    # The selection menu for input$ignore_samples is updated so that the choices
    # are sample_types and groups that are present in the data.
    observeEvent(summary(), {
      
      if ("group" %in% colnames(summary())) {
        options <- c(paste(unique(summary()$sample_type), "samples"), 
                     paste(unique(summary()$group), "samples"))
      } else {
        options <- c(paste(unique(summary()$sample_type), "samples"))
      }
      
      updateSelectizeInput(inputId = "ignore_samples",
                           choices = c("", options))
    })
    
    ext_analyte_list <- reactive({
      req(input$analyte_list)
      ext <- tools::file_ext(input$analyte_list$name)
      return(ext)
    })
    
    # Read in the analyte list when it is uploaded, or show a feedbackWarning if
    # it's the wrong filetype:
    observeEvent(input$analyte_list, {
      req(ext_analyte_list())
      shinyFeedback::hideFeedback("analyte_list")
      if (ext_analyte_list() == "rds") {
        x$analyte_list <- load_and_assign(input$analyte_list$datapath)
      } else { if (ext_analyte_list() %in% c("xlsx", "xls")) {
        x$analyte_list <- readxl::read_excel(input$analyte_list$datapath,
                                             col_names = FALSE)
      } 
      }
      
      shinyFeedback::feedbackWarning(inputId = "analyte_list", 
                                     show = !(ext_analyte_list() %in% c("rds", "xlsx", "xls")),
                                     text = "Please upload a .xlsx, .xls or .rds file.")
    })
    
    without_samples_to_ignore <- reactive({
      req(summary())
      req(input$ignore_samples)
      req(input$method == "Curate analytes based on data")
      
      if ("group" %in% colnames(summary())) {
        group_to_ignore <- stringr::str_extract(
          string = input$ignore_samples,
          pattern = paste0(unique(summary()$group),
                           collapse = "|")) %>% 
          na.omit(.)
      } else {
        group_to_ignore <- NULL
      }
      
      sample_types_to_ignore <- stringr::str_extract(
        string = input$ignore_samples,
        pattern = paste0(unique(summary()$sample_type),
                         collapse = "|")) %>% 
        na.omit(.)
      
      if (!is.null(group_to_ignore)) { 
        if (!(group_to_ignore %in% summary()$group)) {
          rlang::abort(class =  "wrong_group",
                       message = paste("The group_to_ignore",
                                       group_to_ignore,
                                       "is not present in the group column of the data."))
        } 
      }
      
      if (!is.null(sample_types_to_ignore)) {
        if (any(!(sample_types_to_ignore %in% summary()$sample_type))) {
          rlang::abort(class =  "wrong_sample_type",
                       message = "One or more of sample_types_to_ignore is not present in the \"sample_type\" column of the data.")
        }
        
        if (is.null(group_to_ignore)) {
          filtered <- summary() %>% 
            dplyr::filter(!(sample_type %in% sample_types_to_ignore))
        } else {
          filtered <- summary() %>% 
            dplyr::filter(!(group %in% group_to_ignore) & !(sample_type %in% sample_types_to_ignore))
        }
      }
      return(filtered)
      
    })
    
    observeEvent(input$curate_analytes, {
      # Reset x$curated_analytes and x$analyte_curated data so that the plot is
      # no longer shown if curation based on data had already been performed
      x$curated_analytes <- NULL
      x$analyte_curated_data <- NULL
      if (input$method == "Curate analytes based on data") {
        if (is_truthy(without_samples_to_ignore())) {
          data_to_use <- without_samples_to_ignore()
        } else {
          data_to_use <- summary()
        }
        # Perform analyte curation:
        x$curated_analytes <- curate_analytes(
          data = data_to_use,
          cut_off_percentage = input$cut_off)
        
        x$analyte_curated_data <- dplyr::left_join(x$curated_analytes, 
                                                   summary())
        
        showNotification("Analyte curation has been performed based on the data.", 
                         type = "message")
        
      } 
      
      if (input$method == "Supply an analyte list") {
        
        tryCatch(expr = {
          x$analyte_curated_data <- curate_analytes_with_list(
            data = summary(),
            analyte_list = x$analyte_list)
          
          showNotification("Analyte curation has been performed based on the analyte list.", 
                           type = "message")
          },
          too_many_columns = function(c) {
            showNotification(c$message, 
                             type = "error")
            shinyFeedback::feedbackDanger("analyte_list",
                                          show = TRUE,
                                          text = c$message)
          },
          missing_analytes = function(c){
            showNotification(c$message,
                             type = "warning")
            x$analyte_curated_data <- suppressWarnings(
              curate_analytes_with_list(
                data = summary(),
                analyte_list = x$analyte_list))
          })
      }
      
    })
    
    with_analytes_to_include <- reactive({
      req(summary(),
          purrr::map(x$mod_results,
                     ~ is_truthy(.x$analytes_to_include())))
      
      purrr::imap(x$mod_results,
                  function(results, current_cluster) {
                    data_current_cluster <- summary() %>% 
                      dplyr::filter(cluster == current_cluster)
                    
                    dplyr::left_join(results$analytes_to_include(),
                                     data_current_cluster)
                      
                  }) %>% 
        purrr::reduce(dplyr::full_join)
      
    })
    
    # Make downloading analyte_curated_data possible:
    output$download <- downloadHandler(
      filename = function() {
        todays_date <- paste0(stringr::str_replace_all(Sys.Date(),
                                                       pattern = "-",
                                                       replacement = ""))
        switch(input$download_format,
               "R object" = paste0(todays_date, "_curated_analytes.rds"),
               "Excel file" = paste0(todays_date, "_curated_analytes.xlsx"))
      },
      content = function(file) {
        data_to_download <- with_analytes_to_include()
        switch(input$download_format,
               "R object" = save(data_to_download, 
                                 file = file),
               "Excel file" = writexl::write_xlsx(data_to_download, 
                                                  path = file))
      }
    )
    
    return(list(
      analyte_curated_data = with_analytes_to_include,
      method = reactive({ input$method }),
      ignore_samples = reactive({ input$ignore_samples }),
      cut_off_percentage = reactive({ input$cut_off }),
      analyte_list = reactive({ input$analyte_list$name }),
      objects = reactive({ x$mod_results })
    ))
 
  })
}
    
## To be copied in the UI
# mod_analyte_curation_ui("analyte_curation_ui_1")
    
## To be copied in the server
# mod_analyte_curation_server("analyte_curation_ui_1")
