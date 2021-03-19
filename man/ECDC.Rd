% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ECDC.R
\name{ECDC}
\alias{ECDC}
\title{R6 Class containing specific attributes and methods for ECDC dataset}
\source{
https://opendata.ecdc.europa.eu/covid19/casedistribution/csv
}
\description{
Country specific information for downloading, cleaning
and processing covid-19 region data from the European Centre for
Disease Prevention and Control.
}
\details{
Inherits from \code{DataClass}
}
\examples{
\dontrun{
national <- ECDC$new(verbose = TRUE, steps = TRUE)
national$download()
national$clean()
national$process()
national$return()
}

}
\section{Super class}{
\code{\link[covidregionaldata:DataClass]{covidregionaldata::DataClass}} -> \code{ECDC}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{level_1_region}}{the level 1 region name.}

\item{\code{data_url}}{link to raw data}

\item{\code{source_data_cols}}{existing columns within the raw data}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-clean}{\code{ECDC$clean()}}
\item \href{#method-return}{\code{ECDC$return()}}
\item \href{#method-new}{\code{ECDC$new()}}
\item \href{#method-clone}{\code{ECDC$clone()}}
}
}
\if{html}{
\out{<details open ><summary>Inherited methods</summary>}
\itemize{
\item \out{<span class="pkg-link" data-pkg="covidregionaldata" data-topic="DataClass" data-id="download">}\href{../../covidregionaldata/html/DataClass.html#method-download}{\code{covidregionaldata::DataClass$download()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="covidregionaldata" data-topic="DataClass" data-id="get_region_codes">}\href{../../covidregionaldata/html/DataClass.html#method-get_region_codes}{\code{covidregionaldata::DataClass$get_region_codes()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="covidregionaldata" data-topic="DataClass" data-id="process">}\href{../../covidregionaldata/html/DataClass.html#method-process}{\code{covidregionaldata::DataClass$process()}}\out{</span>}
}
\out{</details>}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clean"></a>}}
\if{latex}{\out{\hypertarget{method-clean}{}}}
\subsection{Method \code{clean()}}{
ECDC specific state level data cleaning
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ECDC$clean()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-return"></a>}}
\if{latex}{\out{\hypertarget{method-return}{}}}
\subsection{Method \code{return()}}{
Specific return settings for the ECDC dataset.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ECDC$return()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
Set up the country class with attributes set to input
parameters
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ECDC$new(
  level = "1",
  totals = FALSE,
  localise = TRUE,
  verbose = FALSE,
  steps = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{level}}{The region level for the data}

\item{\code{totals}}{Boolean. If TRUE, returns totalled data per region up to
today's date.}

\item{\code{localise}}{Boolean. Should region names be localised.}

\item{\code{verbose}}{Boolean. Display information at various stages.}

\item{\code{steps}}{Boolean. Keep data from each processing step.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ECDC$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}