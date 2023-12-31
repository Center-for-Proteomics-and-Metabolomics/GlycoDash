% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_general.R
\name{hide_outliers}
\alias{hide_outliers}
\title{Hide outliers in a plotly box plot}
\usage{
hide_outliers(plotly_object)
}
\arguments{
\item{plotly_object}{A plotly object with a box plot.}
}
\value{
A plotly object with outliers hidden.
}
\description{
Plotly ignores the ggplot argument \code{outlier.shape = NA}. This
function can be used instead to hide the outliers.
}
\examples{
data(mtcars)

# Introduce an outlier:
mtcars[3, "disp"] <- 400

plot <- ggplot2::ggplot(mtcars,
                        ggplot2::aes(x = as.factor(gear), y = disp)) +
  ggplot2::geom_boxplot(outlier.shape = NA) +
  ggplot2::facet_wrap(~ cyl)

plotly_object <- plotly::ggplotly(plot)

# Outlier is shown, even though we used outlier.shape = NA:
plotly_object

# Hide the outlier:
hide_outliers(plotly_object)
}
