% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_general.R
\name{firstupper}
\alias{firstupper}
\title{Convert the first letter of a string to uppercase}
\usage{
firstupper(string)
}
\arguments{
\item{string}{A single character string or a vector with character strings.}
}
\value{
The same string or vector of strings but with the first letter(s) in
  uppercase.
}
\description{
Convert the first letter of a string to uppercase
}
\examples{
firstupper(string = "this sentence starts with uppercase.")

some_strings <- c("you can also", 
                  "use this function", 
                  "on a vector of strings")

firstupper(some_strings)

}
