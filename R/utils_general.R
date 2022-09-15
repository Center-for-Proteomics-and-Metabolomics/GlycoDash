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

#' Load R-objects and assign them a new name
#'
#' When you load an R-object with \code{\link[base]{load}} the object keeps the
#' name it had at the moment that it was saved. Sometimes it's more convenient
#' to assign a new name to the object when it is being loaded.
#' \code{load_and_assign()} allows you to do that.
#' 
#' @section Credits:
#' The code for this function was found on StackOverflow.\cr
#' Question asked by Ryan C. Thompson:\cr
#' \url{https://stackoverflow.com/questions/5577221/how-can-i-load-an-object-into-a-variable-name-that-i-specify-from-an-r-data-file}\cr
#' Solution provided by ricardo:\cr
#' \url{https://stackoverflow.com/users/1453172/ricardo}
#' 
#' @param file_path The path to the R-object that you want to load.
#'
#' @return
#' @export
#'
#' @examples
#' old_name <- c("This is a random vector",
#'               "to serve as an example")
#'
#' # Creating a temporary directory:
#' file_path <- file.path(tempdir(),
#'                        "R_object.rds")
#'
#' # Saving the vector as an R-object to the temporary directory:
#' save(old_name,
#'      file = file_path)
#'
#' # Loading the R object and assigning a new name to it:
#' new_name <- load_and_assign(file_path)
#' 
load_and_assign <- function(file_path){
  tryCatch(
    load(file_path),
    error = function(e) {
      
    },
    warning = function(w) { 
      stop(paste("Check if the file_path was not misspelled.",
                 "Was the R-object you're trying to load saved with the function saveRDS()?",
                 "load_and_assign only works with objects saved with the function save()"))
    })
  get(ls()[ls() != "file_path"])
}


comma_and <- function(string_components) {
  comma_string <- paste(string_components, collapse = ", ")
  comma_and_string <- sub(pattern = ",\\s([^,]+)$", 
                          replacement = " and \\1",
                          x = comma_string)
  return(comma_and_string)
}

comma_or <- function(string_components) {
  comma_string <- paste(string_components, collapse = ", ")
  comma_and_string <- sub(pattern = ",\\s([^,]+)$", 
                          replacement = " or \\1",
                          x = comma_string)
  return(comma_and_string)
}

is_truthy <- function(x) {
  valid <- tryCatch(
    expr = {
      x
      TRUE
    },
    error = function(e) {
      FALSE
    })
  if (valid) {
    return(isTruthy(x))
  } else {
    return(FALSE)
  }
}
