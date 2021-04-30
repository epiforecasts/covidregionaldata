
---
title: "covidregionaldata: An R Package for subnational and national level COVID-19 data"
authors:
  - name: Joseph PG. Palmer
    affiliation: 1
affiliations:
 - name: Royal Holloway University of London
   index: 1
date: "27 April 2021"
bibliography: paper.bib
tags:
  - R
  - COVID-19

output: articles::joss_article
csl: apa.csl
journal: JOSS
link-citations: yes
---
# Summary 

`covidregionaldata` is an R [@Rdev:2013] package that provides an interface sub-national and national level COVID-19 data, providing cleaned and checked COVID-19 case and, where available, deaths, recoveries and hospitalisation data for countries supported. All package code is archived on Zenodo [@covidregionaldata2020] and [Github](https://github.com/epiforecasts/covidregionaldata). Currently `covidregionaldata` provides subnational data collated by official government bodies or by credible non-governmental efforts for 14 countries including the UK, India, USA, and Brazil. In addition, national level data is provided from the World Health Organisation (WHO) [@who], European Centre for Disease Prevention and Control (ECDC), John Hopkins University (JHU) [@Dong2020], and the Google COVID-19 open data project [@Wahltinez2020]. In addition, we also provide an interface to subnational data curated by JHU used for their [2019 Novel Coronavirus Visual Dashboard](https://github.com/CSSEGISandData/COVID-19) [@Dong2020], and the Google COVID-19 [open data project](https://github.com/GoogleCloudPlatform/covid-19-open-data) [@Wahltinez2020]. The package standardises downloading, cleaning, and processing data from different sources in an accessible and transparent pipeline with the aim of allowing data users to contribute to shared data handling 

The onset of the COVID-19 pandemic in late 2019 has placed immense pressures on the public health community to not only advise governments around the world on how this disease might impact communities, but to also suggest what policies would reduce transmission and mitigate harm. Evidence based science requires reliable sources of data, or at least sources where the biases present are known. Evaluating available data is often the task of individual scientists or groups of collaborators which may lead to issues in later analyses if done incorrectly or if data sources change unexpectedly.


In many instances, COVID-19 data is available to download from official sources, such as the [WHO](https://covid19.who.int/) and [ECDC](https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide}) at a national level, whilst some country specific data is provided by government bodies, such as the [Office for National Statistics (ONS)](https://coronavirus.data.gov.uk/details/download}) in the United Kingdom. Stemming from different sources, these data come in a variety of different formats, requiring researchers to check and standardise data before it can be combined and used for analysis. This is a time-consuming process as different data sets require different methods and it can be very easy for errors to creep into code, either through programmer error or through changes to a dependency package, potentially misrepresenting the data in ways which are difficult to identify. At best an independent workflow slows down the pace of research, at worst it can lead to misleading and erroneous results.  

With the current pressures to understand the development of the pandemic across the globe, we feel it is important researchers have a package which brings together data from multiple different sources and allows it to be easily cleaned, checked and standardised ready for application. `covidregionaldata` uses packages many researchers are familiar with (such as the `tidyverse` suite) and so can be easily adopted by researchers working in R. In addition to code coverage tests, all data sets are tested daily and their [data-status](https://epiforecasts.io/covidregionaldata/articles/dataset-status.html) is reported.

Multiple organisations, including JHU, Google and the COVID-19 Data Hub [@covid19datahub:2020], have responded to the COVID-19 pandemic by building similar data curation and cleaning piplines to that provided in `covidregionaldata`. However, most of these efforts typically aggregate the data they extract into a separate data stream, breaking the linkage with the raw data, and often do not fully surface their data pipeline for others to clean and inspect. In contrast `covidregionaldata` provides a clear set of documented tools that directly operate on raw data where possible in order to make the full data cleaning process transparent to end users. Other interfaces to COVID-19 data are available in R though fewer provide tools for downloading subnational data for multiple countries and none known to the authors provide a consistent cleaning pipeline of the data sources they support. COVID-19 Data Hub provides cleaning functions, a wrapper to a custom database hosted by COVID-19 Data Hub, and access to snapshots of data reported historically. `Covdata` [@covdata] provides weekly COVID-19 data updates as well as mobility and activity data from Apple and [Google](https://www.google.com/covid19/mobility/data_documentation.html). `sars2pack`[@sars2pack] provides interfaces to a large number of data sets curated by external organisations, however, like the other sources quoted here, does not provide an interface to individual country data sources or provide a consistent set of data handling tools. The objective of `covidregionaldata` is to provide national and subnational COVID-19 data in a consistent and fully transparent framework, allowing researchers to inspect where their data comes from and how it is prepared. Furthermore, our tools and processes are designed to be readily extensible, allowing users to easily extend the packages' capabilities and contribute to the further development of the package.

So far `covidregionaldata` has been used by researchers as part of data cleansing pipelines for estimating the effective reproductive number of COVID-19 in real-time both nationally and subnationally [@AbbottSam2020Ettr]. It has also been used in analyses comparing effective reproduction numbers from different data sources in the United Kingdom [@Sherratt2020] and estimating the increase in transmission related to the B.1.1.7 variant [@Davieseabg3055]. As well as its use in Scientific research it has also been used by scientists and members of the public to visualise and explore current trands in COVID-19 case, deaths, and hospitalisations.

# Acknowledgements

This package provides an interface to data sources which are often collected and maintained by individuals or small teams. Our work, both in this package and more generally, would not be possible without their efforts. Thanks to all contributors and package users who have otherwise provided feedback. Thanks to Tim Talyor for useful design discussions.

# References
