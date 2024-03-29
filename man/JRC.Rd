% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/JRC.R
\name{JRC}
\alias{JRC}
\title{R6 Class containing specific attributes and methods for European Commission's
Joint Research Centre data}
\source{
\url{https://github.com/ec-jrc/COVID-19}
}
\description{
Class for downloading, cleaning and processing COVID-19
region data from the European Commission's Joint Research Centre. Subnational
data (admin level 1) on numbers of contagious and fatalities by COVID-19,
collected directly from the National Authoritative sources (National
monitoring websites, when available). For more details see
https://github.com/ec-jrc/COVID-19
}
\examples{
\dontrun{
# get country level data
jrc_level_1 <- JRC$new(level = "1", verbose = TRUE, steps = TRUE, get = TRUE)
jrc_level_1$return()

# show available regions with data at the first level of interest (country)
jrc_level_1$available_regions()

# get region level data
jrc_level_2 <- JRC$new(level = "2", verbose = TRUE, steps = TRUE, get = TRUE)
jrc_level_2$return()

# show available regions with data at the second level of interest (region)
jrc_level_2$available_regions()
}
}
\seealso{
National data sources
\code{\link{Covid19DataHub}},
\code{\link{ECDC}},
\code{\link{Google}},
\code{\link{JHU}},
\code{\link{WHO}}
}
\concept{dataset}
\concept{national}
\section{Super classes}{
\code{\link[covidregionaldata:DataClass]{covidregionaldata::DataClass}} -> \code{\link[covidregionaldata:CountryDataClass]{covidregionaldata::CountryDataClass}} -> \code{JRC}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{origin}}{name of origin to fetch data for}

\item{\code{supported_levels}}{A list of supported levels.}

\item{\code{supported_region_names}}{A list of region names in order of level.}

\item{\code{supported_region_codes}}{A list of region codes in order of level.}

\item{\code{level_data_urls}}{List of named links to raw data.}

\item{\code{source_data_cols}}{existing columns within the raw data}

\item{\code{source_text}}{Plain text description of the source of the data}

\item{\code{source_url}}{Website address for explanation/introduction of the
data}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-clean_common}{\code{JRC$clean_common()}}
\item \href{#method-clean_level_1}{\code{JRC$clean_level_1()}}
\item \href{#method-clean_level_2}{\code{JRC$clean_level_2()}}
\item \href{#method-clone}{\code{JRC$clone()}}
}
}
\if{html}{
\out{<details ><summary>Inherited methods</summary>}
\itemize{
\item \out{<span class="pkg-link" data-pkg="covidregionaldata" data-topic="DataClass" data-id="available_regions">}\href{../../covidregionaldata/html/DataClass.html#method-available_regions}{\code{covidregionaldata::DataClass$available_regions()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="covidregionaldata" data-topic="DataClass" data-id="clean">}\href{../../covidregionaldata/html/DataClass.html#method-clean}{\code{covidregionaldata::DataClass$clean()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="covidregionaldata" data-topic="DataClass" data-id="download">}\href{../../covidregionaldata/html/DataClass.html#method-download}{\code{covidregionaldata::DataClass$download()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="covidregionaldata" data-topic="DataClass" data-id="download_JSON">}\href{../../covidregionaldata/html/DataClass.html#method-download_JSON}{\code{covidregionaldata::DataClass$download_JSON()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="covidregionaldata" data-topic="DataClass" data-id="get">}\href{../../covidregionaldata/html/DataClass.html#method-get}{\code{covidregionaldata::DataClass$get()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="covidregionaldata" data-topic="DataClass" data-id="initialize">}\href{../../covidregionaldata/html/DataClass.html#method-initialize}{\code{covidregionaldata::DataClass$initialize()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="covidregionaldata" data-topic="DataClass" data-id="process">}\href{../../covidregionaldata/html/DataClass.html#method-process}{\code{covidregionaldata::DataClass$process()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="covidregionaldata" data-topic="DataClass" data-id="return">}\href{../../covidregionaldata/html/DataClass.html#method-return}{\code{covidregionaldata::DataClass$return()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="covidregionaldata" data-topic="DataClass" data-id="set_region_codes">}\href{../../covidregionaldata/html/DataClass.html#method-set_region_codes}{\code{covidregionaldata::DataClass$set_region_codes()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="covidregionaldata" data-topic="DataClass" data-id="summary">}\href{../../covidregionaldata/html/DataClass.html#method-summary}{\code{covidregionaldata::DataClass$summary()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="covidregionaldata" data-topic="DataClass" data-id="test">}\href{../../covidregionaldata/html/DataClass.html#method-test}{\code{covidregionaldata::DataClass$test()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="covidregionaldata" data-topic="CountryDataClass" data-id="filter">}\href{../../covidregionaldata/html/CountryDataClass.html#method-filter}{\code{covidregionaldata::CountryDataClass$filter()}}\out{</span>}
}
\out{</details>}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clean_common"></a>}}
\if{latex}{\out{\hypertarget{method-clean_common}{}}}
\subsection{Method \code{clean_common()}}{
JRC specific data cleaning. The raw source data columns are
converted to the correct type and renamed appropriately to match the
standard for general processing.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{JRC$clean_common()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clean_level_1"></a>}}
\if{latex}{\out{\hypertarget{method-clean_level_1}{}}}
\subsection{Method \code{clean_level_1()}}{
JRC specific country level data cleaning. Selects country
level (level 1) columns from the data ready for further processing.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{JRC$clean_level_1()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clean_level_2"></a>}}
\if{latex}{\out{\hypertarget{method-clean_level_2}{}}}
\subsection{Method \code{clean_level_2()}}{
JRC specific region level data cleaning. Selects country
(level 1) and region (level 2) columns from the data ready for further
processing.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{JRC$clean_level_2()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{JRC$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
