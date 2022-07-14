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


