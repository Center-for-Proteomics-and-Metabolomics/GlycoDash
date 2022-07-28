#' Title
#'
#' @param gp 
#' @param size 
#'
#' @return
#' @export
#'
#' @examples
facet_strip_bigger <- function(ggplotly, size = 38){
  if(missing(ggplotly)){
    rlang::abort(class = "no_ggplotly_object",
                 message = "This function needs a facet_wrap ggplotly object.")
  }
  
  ggplotly[["x"]][["layout"]][["margin"]][["t"]] <- as.numeric(size)
  
  n_facets <- c(1:length(ggplotly[["x"]][["layout"]][["shapes"]]))
  
  for(i in n_facets){
    if(n_facets[i] %% 2 == 0){
      ggplotly[["x"]][["layout"]][["shapes"]][[i]][["y0"]] <- as.numeric(size)
      ggplotly[["x"]][["layout"]][["shapes"]][[i]][["y1"]] <- 0
    }
  }
  
  return(ggplotly)
}

#' Title
#'
#' @param plotly_object 
#' @param x_distance 
#' @param y_distance 
#'
#' @return
#' @export
#'
#' @examples
change_axis_title_distance <- function(plotly_object, 
                                       x_distance = 50, 
                                       y_distance = 50) {
  plotly_object[["x"]][["layout"]][["annotations"]][[2]][["xshift"]] <- -x_distance
  
  plotly_object[["x"]][["layout"]][["margin"]][["l"]] <- x_distance + 20
  
  plotly_object[["x"]][["layout"]][["annotations"]][[1]][["yshift"]] <- -y_distance
  
  return(plotly_object)
}

try_call <- function(uncalled_reactive_expression) {
  tryCatch(
    expr = {
      # Call the reactive expression:
      uncalled_reactive_expression()
    },
    error = function(e) {
      # If there is an error, because the reactive expression was never
      # validated/doesn't exist, return NULL:
      NULL
    }
  )
}
