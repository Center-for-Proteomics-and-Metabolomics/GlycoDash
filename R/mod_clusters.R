#' clusters UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_clusters_ui <- function(id){
  ns <- NS(id)
  shinydashboard::box(
    title = "Clusters",
    width = NULL,
    solidHeader = TRUE,
    status = "primary",
    numericInput(ns("n_clusters"), 
                 "How many clusters does your data contain?",
                 value = 1,
                 min = 1,
                 max = 25,
                 step = 1),
    uiOutput(ns("clusters")),
    actionButton(ns("button"),
                 "Define the clusters")
  )
}

#' clusters Server Functions
#'
#' @noRd 
mod_clusters_server <- function(id, LaCyTools_summary){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Create textInputs -------------------------------------------------------
    
    # Create inputIds for the cluster textInputs based on the
    # value of input$n_clusters:
    cluster_inputIds <- reactive({
      req(input$n_clusters)
      cluster_inputIds <- purrr::map(seq_len(input$n_clusters),
                                     ~ paste0("cluster", .x))
      return(cluster_inputIds)
    })
    
    # Create textInputs for the clusters. The number of inputs created is the
    # same as the value of input$n_clusters.
    output$clusters <- renderUI({
      req(cluster_inputIds())
      purrr::imap(cluster_inputIds(),
                  function(inputId, i) textInput(
                    ns(inputId),
                    label = paste("By what word/letters within the analyte name can the analytes belonging to cluster",
                                  i,
                                  "be recognized?")
                  ))
    })
    
    
    # Keyword checks ----------------------------------------------------------
    
    cluster_keywords_found <- reactive({
      req(cluster_inputIds())
      
      unique_analytes_in_data <- unique(LaCyTools_summary()$analyte)
      
      keywords_found <- purrr::map_lgl(
        cluster_inputIds(),
        function(cluster_inputId) {
          req(!is.null(input[[cluster_inputId]]))
          
          if (input[[cluster_inputId]] != "") {
            find_cluster_keyword_match(unique_analytes = unique_analytes_in_data,
                                       cluster_keyword = input[[cluster_inputId]])
          } else {
            TRUE
          }
          
        })
      
      names(keywords_found) <- cluster_inputIds()
      
      return(keywords_found)
    })
    
    
    keywords <- reactive({
      req(purrr::map(cluster_inputIds(),
                     ~input[[.x]]))
      
      purrr::map(cluster_inputIds(),
                 ~input[[.x]])
    })
    
    
    # Display feedback based on the checks that were performed:
    observe({
      req(!is.null(cluster_keywords_found()))
      
      purrr::map(cluster_inputIds(),
                 ~ shinyFeedback::hideFeedback(.x))
      
      if (any(!cluster_keywords_found())) {
        purrr::imap(cluster_keywords_found(),
                    function(keyword_found,
                             inputId) {
                      shinyFeedback::feedbackDanger(
                        inputId,
                        show = !keyword_found,
                        text = paste("This keyword did not match any analytes in your data.", 
                                     "Please choose a different keyword.")
                      )
                    })
      }
    })
    
    
    # Add clusters to data ----------------------------------------------------
    
    observe({
      shinyjs::toggleState("button",
                           condition = all(
                             is_truthy(LaCyTools_summary()),
                             is_truthy(cluster_keywords_found())
                           ))
    })
    
    r <- reactiveValues()
    
    observe({
      if (!is_truthy(LaCyTools_summary()) & is_truthy(r$with_clusters)) {
        r$with_clusters <- NULL
        showNotification("Clusters have to be re-added to the data.",
                         type = "warning")
      }
    })
    
    observe({
      req(LaCyTools_summary(),
          keywords())
      req(all(keywords() != ""))
      
      r$with_clusters <- tryCatch(
        expr = {
          define_clusters(data = LaCyTools_summary(),
                          cluster_keywords = keywords())
        },
        unmatched_analytes = function(c) {
          showNotification(c$message,
                           type = "error",
                           duration = NULL)
          NULL
        })
    }) %>% bindEvent(input$button)
    
    return(list(
      data = reactive({ r$with_clusters }),
      button = reactive({ input$button })
    ))
    
  })
}