#' Increase the height of the facet strips of a plotly object.
#'
#' @param ggplotly A plotly object. 
#' @param size The height that the facet strip should become.
#'
#' @return A plotly object with taller facet strips.
#' @export
#'
#' @examples
#' data(mtcars)
#' plot <- ggplot2::ggplot(mtcars,
#'                         ggplot2::aes(x = mpg, y = disp)) +
#'         ggplot2::geom_point() +
#'         ggplot2::facet_wrap(~cyl)
#'         
#' plotly_object <- plotly::ggplotly(plot)
#'
#' plotly_object
#' 
#' facet_strip_bigger(plotly_object)
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

#' Increase distance between axis titles and the axis in a plotly object
#'
#' @param plotly_object A plotly object.
#' @param x_distance Distance between the x-axis title and the x-axis.
#' @param y_distance Distance between the y-axis title and the y-axis.
#'
#' @return A plotly object with changed distances between axis titles and axis.
#' @export
#'
#' @examples
#' data(mtcars)
#' plot <- ggplot2::ggplot(mtcars,
#'                         ggplot2::aes(x = mpg, y = disp)) +
#'         ggplot2::geom_point() +
#'         ggplot2::facet_wrap(~cyl)
#'         
#' plotly_object <- plotly::ggplotly(plot)
#'
#' plotly_object
#' 
#' change_axis_title_distance(plotly_object)
change_axis_title_distance <- function(plotly_object, 
                                       x_distance = 50, 
                                       y_distance = 50) {
  plotly_object[["x"]][["layout"]][["annotations"]][[2]][["xshift"]] <- -y_distance
  
  plotly_object[["x"]][["layout"]][["margin"]][["l"]] <- y_distance + 20
  
  plotly_object[["x"]][["layout"]][["annotations"]][[1]][["yshift"]] <- -x_distance
  
  return(plotly_object)
}

#' Determine the number of facets in a ggplot object.
#'
#' @param ggplot_object A ggplot object.
#'
#' @return An integer that indicates the number of facets in the ggplot.
#' @export
#'
#' @examples
#' data(mtcars)
#' plot <- ggplot2::ggplot(mtcars,
#'                      ggplot2::aes(x = mpg, y = disp)) +
#'                      ggplot2::geom_point() +
#'                      ggplot2::facet_wrap(~ cyl)
#'                      
#' nfacets(plot)
nfacets <- function(ggplot_object) {
  build <- ggplot2::ggplot_build(ggplot_object)
  length(levels(build$data[[1]]$PANEL))
}

#' Hide outliers in a plotly box plot
#'
#' Plotly ignores the ggplot argument \code{outlier.shape = NA}. This
#' function can be used instead to hide the outliers.
#'
#' @param plotly_object A plotly object with a box plot.
#'
#' @return A plotly object with outliers hidden.
#' @export
#'
#' @examples
#' data(mtcars)
#' 
#' # Introduce an outlier:
#' mtcars[3, "disp"] <- 400
#' 
#' plot <- ggplot2::ggplot(mtcars,
#'                         ggplot2::aes(x = as.factor(gear), y = disp)) +
#'   ggplot2::geom_boxplot(outlier.shape = NA) +
#'   ggplot2::facet_wrap(~ cyl)
#' 
#' plotly_object <- plotly::ggplotly(plot)
#' 
#' # Outlier is shown, even though we used outlier.shape = NA:
#' plotly_object
#' 
#' # Hide the outlier:
#' hide_outliers(plotly_object)
hide_outliers <- function(plotly_object) {
  
  plotly_object[["x"]][["data"]] <- purrr::map(
    plotly_object[["x"]][["data"]], 
    function(x) {
      # Only the hover info for the boxplot traces is left as the default "y",
      # so here only the markers belonging to the boxplot traces will be made
      # invisible:
      if (x$hoverinfo == 'y') {  
        x$marker = list(opacity = 0) 
      }  
      return(x) 
    })
  
  return(plotly_object)
}

#' Try to call a reactive expression
#'
#' This functions tries to call a reactive expression and returns NULL when
#' there is an error.
#'
#' @param uncalled_reactive_expression The reactive expression that you want to
#'   call.
#'
#' @return Either the called reactive expression or NULL if the reactive
#'   expression was invalidated or did not exist.
#' @export
#'
#' @examples
#' # Can't show an example with reactives outside of a Shiny app.
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
#' @return The loaded R-object with its assigned name.
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


#' Collapse strings into one string separated by comma's and "and"
#'
#' @param string_components A character vector or list with character strings.
#'
#' @return The strings in string_components pasted together, separated by
#'   comma's. The last two strings are separated by "and".
#' @export
#'
#' @examples
#' some_words <- c("apple", "pear", "peach", "raspberry")
#' comma_and(string_components = some_words)
comma_and <- function(string_components) {
  comma_string <- paste(string_components, collapse = ", ")
  comma_and_string <- sub(pattern = ",\\s([^,]+)$", 
                          replacement = " and \\1",
                          x = comma_string)
  return(comma_and_string)
}

#' Collapse strings into one string separated by comma's and "or"
#'
#' @param string_components A character vector or list with character strings.
#'
#' @return The strings in string_components pasted together, separated by
#'   comma's. The last two strings are separated by "or".
#' @export
#'
#' @examples
#' some_words <- list("apple", "pear", "peach", "raspberry")
#' comma_or(string_components = some_words)
comma_or <- function(string_components) {
  comma_string <- paste(string_components, collapse = ", ")
  comma_and_string <- sub(pattern = ",\\s([^,]+)$", 
                          replacement = " or \\1",
                          x = comma_string)
  return(comma_and_string)
}

#' Version of isTruthy that works with non-existing reactive expressions
#'
#' When you use isTruthy on a reactive expression but that reactive doesn't
#' exist (yet) an error occurs. To avoid that, this function first checks if the
#' expression x can be called before checking its truthiness.
#'
#' @param x An expression of which you want to test the truthiness.
#'
#' @return If calling the expression x results in an error this function returns
#'   FALSE. If the expression x can be called this function returns isTruthy(x).
#' @export
#'
#' @examples
#' # I can't show an example of this outside of a Shiny app.
is_truthy <- function(x) {
  
  valid <- tryCatch(
    expr = {
      x
      TRUE
    },
    error = function(e) {
      FALSE
    }
  )
  
  if (valid) {
    return(isTruthy(x))
  } else {
    return(FALSE)
  }
}


#' Convert the first letter of a string to uppercase
#'
#' @param string A single character string or a vector with character strings.
#'
#' @return The same string or vector of strings but with the first letter(s) in
#'   uppercase.
#' @export
#'
#' @examples
#' firstupper(string = "this sentence starts with uppercase.")
#' 
#' some_strings <- c("you can also", 
#'                   "use this function", 
#'                   "on a vector of strings")
#' 
#' firstupper(some_strings)
#' 
firstupper <- function(string) {
  substr(string, 1, 1) <- toupper(substr(string, 1, 1))
  return(string)
}


#' Convert the first letter of a string to lowercase
#'
#' @param string A single character string or a vector with character strings.
#'
#' @return The same string or vector of strings but with the first letter(s) in
#'   lowercase.
#' @export
#'
#' @examples
#' firstlower("This sentence starts with LOWERcase.")
#' 
#' some_strings <- c("You can also", 
#'                   "Use this FUNCTION", 
#'                   "On a vector of strings")
#' 
#' firstupper(some_strings)
#' 
firstlower <- function(string) {
  substr(string, 1, 1) <- tolower(substr(string, 1, 1))
  return(string)
}



# Function to create a color palette
color_palette <- function(n_colors) {
  
  # 10 colors used by matplotlib (because they are nice)
  plt_colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
                  "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")
  
  if (n_colors <= length(plt_colors)) {
    # Use these 10 colors if number of required colors is less than 10
    my_palette <- plt_colors[1:n_colors]
  } else {
    # Otherwise create different colors
    my_palette <- colorRampPalette(plt_colors)(n_colors)
  }
  
  return(my_palette)
}

