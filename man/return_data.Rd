% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{return_data}
\alias{return_data}
\title{Control data return}
\usage{
return_data(obj, class = FALSE)
}
\arguments{
\item{obj}{A Class based on a \code{DataClass}}

\item{class}{Logical, defaults to FALSE. If TRUE returns the
\code{DataClass} object rather than a tibble or a list of tibbles.
Overrides \code{steps}.}
}
\description{
Controls data return for \code{get_reigonal_data} and
\code{get_national_data}
}
\concept{utility}
