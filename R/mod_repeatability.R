#' repeatability UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_repeatability_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    fluidPage(
      fluidRow(
        h1("Repeatability")
      ),
      fluidRow(
        tags$style(
          HTML(paste0("#",
                      ns("tabbed_box"),
                      " .btn {float: right;}",
                      "#",
                      ns("tabbed_box"),
                      " .box-title {width: 100%}",
                      "#",
                      ns("tabbed_box"),
                      " .fa {float: right; margin-top: 3px; margin-left: 5px;}"))
        ),
        div(
          id = ns("tabbed_box"),
          shinydashboard::box(
            title = span(
              "Assess repeatability",
              actionButton(ns("add_tab"),
                           "Add a tab",
                           icon = icon("plus-square"))),
            width = NULL,
            solidHeader = TRUE,
            status = "primary",
            tabsetPanel(
              id = ns("tabs"),
              # tabPanel(title = "First tab",
              #          uiOutput(ns("first_tab")))
              tabPanel("Standard 1",
                       column(
                         width = 6,
                         br(),
                         shinydashboard::box(
                           title = "Select a standard",
                           width = NULL,
                           solidHeader = TRUE,
                           status = "primary",
                           uiOutput(ns("standard_menu")),
                           actionButton(ns("assess_repeatability"),
                                        label = "Assess repeatability")
                         ),
                         plotOutput(ns("plot"))
                       ),
                       column(
                         width = 6,
                         br(),
                         DT::dataTableOutput(ns("table"))
                       )
              )
            )
          )
        )
        # column(
        #   width = 6,
        #   shinydashboard::box(
        #     title = "Assess repeatability",
        #     width = NULL,
        #     solidHeader = TRUE,
        #     status = "primary",
        #     uiOutput(ns("standard_menu")),
        #     actionButton(ns("assess_repeatability"),
        #                  label = "Assess repeatability")
        #   )
        # )
      ),
      # fluidRow(
      #   column(
      #     width = 6,
      #     shinydashboard::box(
      #       title = "Repeatability plot",
      #       width = NULL,
      #       solidHeader = TRUE,
      #       status = "primary",
      #       plotOutput(ns("plot"))
      #     )
      #   ),
      #   column(
      #     width = 6,
      #     shinydashboard::box(
      #       title = "Repeatability table",
      #       width = NULL,
      #       solidHeader = TRUE,
      #       status = "primary",
      #       DT::dataTableOutput(ns("table"))
      #     )
      #   )
    )
  )
}
    
#' repeatability Server Functions
#'
#' @noRd 
mod_repeatability_server <- function(id, results_normalization, results_data_import){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    x <- reactiveValues()

    observe({
      req(results_normalization$normalized_data())
      x$data <- results_normalization$normalized_data()
    })
    
    # data <- reactive({
    #   results_normalization$normalized_data()
    # })
    # 
    # Ig_data <- reactive({
    #   results_data_import$Ig_data()
    # })
    # 
    # output$first_tab <- renderUI({
    #   mod_tab_repeatability_ui("tab1")
    # })
    # 
    # observe({
    #   mod_tab_repeatability_server("tab1",
    #                                data = data,
    #                                Ig_data = Ig_data)
    # })
    
    output$standard_menu <- renderUI({
      req(x$data,
          results_data_import$Ig_data())

      sample_type_menu <- selectInput(ns("standard_sample_type"),
                        label = "Choose which standard you want to assess:",
                        choices = unique(x$data$sample_type))

      if (results_data_import$Ig_data() == "Yes") {
        groups_menu <- selectInput(ns("standard_group"),
                                   label = "Choose if you want to look at total or specific Ig samples:",
                                   choices = unique(x$data$group))
      }

      menu <- list(sample_type_menu, groups_menu)
      menu[sapply(menu, is.null)] <- NULL

      return(menu)
    })

    observeEvent(input$assess_repeatability, {
      x$repeatability <- calculate_repeatability_stats(data = x$data,
                                                       standard_sample_type = input$standard_sample_type,
                                                       standard_group = input$standard_group)
      x$variation_table <- x$repeatability %>%
        dplyr::summarise(intra_plate_variation = mean(RSD, na.rm = TRUE))
    })

    output$plot <- renderPlot({
      req(x$repeatability)
      visualize_repeatability(x$repeatability)
    })

    output$table <- DT::renderDataTable({
      req(x$variation_table)

      for_table <- x$variation_table %>%
        dplyr::mutate(intra_plate_variation = signif(intra_plate_variation,
                                                     digits = 3))

      sketch <- htmltools::withTags(table(
        DT::tableHeader(c("Plate", "Intra-plate variation (%)")),
        DT::tableFooter(c("Inter-plate variation (%)",
                          signif(mean(for_table$intra_plate_variation,
                                      na.rm = TRUE),
                                 digits = 3)))
      ))

      DT::datatable(data = for_table,
                    container = sketch,
                    rownames = FALSE)
    })

    observeEvent(input$add_tab, {
      appendTab(inputId = "tabs",
                tabPanel("Standard 2",
                         column(
                           width = 6,
                           br(),
                           shinydashboard::box(
                             title = "Select a standard",
                             width = NULL,
                             solidHeader = TRUE,
                             status = "primary",
                             uiOutput(ns("standard_menu_2")),
                             actionButton(ns("assess_repeatability_2"),
                                          label = "Assess repeatability")
                           ),
                           plotOutput(ns("plot_2"))
                         ),
                         column(
                           width = 6,
                           br(),
                           DT::dataTableOutput(ns("table_2"))
                         )))
      
      shinyjs::disable("add_tab")
      
    })
    
    output$standard_menu_2 <- renderUI({
      req(x$data,
          results_data_import$Ig_data())
      
      sample_type_menu <- selectInput(ns("standard_sample_type_2"),
                                      label = "Choose which standard you want to assess:",
                                      choices = unique(x$data$sample_type))
      
      if (results_data_import$Ig_data() == "Yes") {
        groups_menu <- selectInput(ns("standard_group_2"),
                                   label = "Choose if you want to look at total or specific Ig samples:",
                                   choices = unique(x$data$group))
      }
      
      menu <- list(sample_type_menu, groups_menu)
      menu[sapply(menu, is.null)] <- NULL
      
      return(menu)
    })
    
    observeEvent(input$assess_repeatability_2, {
      x$repeatability_2 <- calculate_repeatability_stats(data = x$data,
                                                       standard_sample_type = input$standard_sample_type_2,
                                                       standard_group = input$standard_group_2)
      x$variation_table_2 <- x$repeatability_2 %>%
        dplyr::summarise(intra_plate_variation = mean(RSD, na.rm = TRUE))
    })
    
    output$plot_2 <- renderPlot({
      req(x$repeatability_2)
      visualize_repeatability(x$repeatability_2)
    })
    
    output$table_2 <- DT::renderDataTable({
      req(x$variation_table_2)
      
      for_table <- x$variation_table_2 %>%
        dplyr::mutate(intra_plate_variation = signif(intra_plate_variation,
                                                     digits = 3))
      
      sketch <- htmltools::withTags(table(
        DT::tableHeader(c("Plate", "Intra-plate variation (%)")),
        DT::tableFooter(c("Inter-plate variation (%)",
                          signif(mean(for_table$intra_plate_variation,
                                      na.rm = TRUE),
                                 digits = 3)))
      ))
      
      DT::datatable(data = for_table,
                    container = sketch,
                    rownames = FALSE)
    })
 
  })
}
