
---
title: "covidregionaldata: Subnational data for COVID-19 epidemiology"
authors:
  - name: Joseph Palmer$^{\star}$
    affiliation: 1
  - name: Katharine Sherratt$^{\star}$
    affiliation: 2
  - name: Richard Martin-Nielson
    affiliation: 3
  - name: Jonnie Bevan
    affiliation: 4
  - name: Hamish Gibbs
    affiliation: 2
  - name: CMMID COVID-19 Working Group
    affiliation: 2
  - name: Sebastian Funk
    affiliation: 2
  - name: Sam Abbott$^{\dagger}$
    affiliation: 2
affiliations:
 - name: Department of Biological Sciences, Royal Holloway University of London
   index: 1
 - name: Centre for Mathematical Modelling of Infectious Diseases, London School of Hygiene & Tropical Medicine
   index: 2
 - name: None
   index: 3
 - name: Tessella
   index: 4
date: "11 May 2021"
bibliography: paper.bib
tags:
  - R
  - COVID-19
  - Open data
  - rstats
  - Sars-Cov-2

output: articles::joss_article
csl: apa.csl
journal: JOSS
link-citations: yes
---

#### $^{\star}$ co-first author $^{\dagger}$ corresponding author

# Summary

`covidregionaldata` is an R [@Rdev:2013] package that provides an interface to subnational and national level COVID-19 data. The package provides cleaned and verified COVID-19 test-positive case counts and, where available, counts of deaths, recoveries, and hospitalisations in a consistent and fully transparent framework. The package automates common processing steps while allowing researchers to easily and transparently trace the origin of the underlying data sources. It has been designed to be readily extensible, allowing users to easily extend the packages' capabilities and contribute to shared data handling. All package code is archived on [Zenodo](https://zenodo.org/record/4718466) [@covidregionaldata] and [Github](https://github.com/epiforecasts/covidregionaldata).

# Statement of need

The onset of the COVID-19 pandemic in late 2019 has placed pressure on public health and research communities to generate evidence that can help advise national and international policy in order to reduce transmission and mitigate harm. At the same time, there has been a renewed policy and public health emphasis on localised, subnational decision making and implementation [@Hale2021; @Liu2021]. This requires reliable sources of data disaggregated to a fine spatial scale, ideally with few and/or known sources of bias.

At a national level, epidemiological COVID-19 data is available to download from official sources such as the [World Health Organisation (WHO)](https://covid19.who.int/) [@WorldHealthOrganisation] or the [European Centre for Disease Control (ECDC)](https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide%7D) [@EuropeanCentreforDiseasePreventionandControl]. Many government bodies provide a wider range of country specific data, such as [Public Health England in the United Kingdom](https://coronavirus.data.gov.uk/details/about-data) [@PublicHealthEngland], and this is often the only way to access data at a subnational scale, for example by state, district, or province.

Sometimes collated from a range of national and subnational sources, these data come in a variety of formats, requiring users to check and standardise data before it can be combined or processed for analysis.  This is a particularly time-consuming process for subnational data sets, which are often only available in the originating countries’ languages and require customised methods for downloading and processing. This generates potential for errors through programming mistakes, changes to a dependency package, or unexpected changes to a data source. This can lead to misrepresenting the data in ways which are difficult to identify. At best, an independent data processing workflow only slows down the pace of research and analysis, while at worst it can lead to misleading and erroneous results.

Because of these issues, it is important to develop robust tools that provide cleaned, checked and standardised data from multiple sources in a transparent manner. `covidregionaldata` provides easy access to clean data using a single-argument function, ready for analysing the epidemiology of COVID-19 from local to global scales, and in a framework that is easy to trace from raw data to the final standardised data set. Additional arguments to this function support users to, amongst other options, specify the spatial level of subnational data, return data with either standardised or country-specific variable names, or to access the full pipeline from raw to clean data, with data returned at each step in the data cleaning process. `covidregionaldata` largely depends on popular packages that many researchers are familiar with (such as the `tidyverse` suite [@Wickham2019]) and can therefore be easily adopted by researchers working in R. In addition to code coverage tests, we test and report the status of all data sets daily.

Currently, `covidregionaldata` provides subnational data collated by official government bodies or by credible non-governmental efforts for 14 countries, including the UK, India, USA, and Brazil. It also provides an interface to subnational data curated by Johns Hopkins University [@Dong2020], and the [Google COVID-19 open data project](https://github.com/GoogleCloudPlatform/covid-19-open-data) [@Wahltinez2020]. National-level data is provided from the World Health Organisation (WHO) [@WorldHealthOrganisation], European Centre for Disease Prevention and Control (ECDC) [@EuropeanCentreforDiseasePreventionandControl], Johns Hopkins University (JHU) [@Dong2020], and the Google COVID-19 open data project [@Wahltinez2020].

# State of the field

Multiple organisations  have built private COVID-19 data curation pipelines similar to that provided in `covidregionaldata`, including Johns Hopkins University (JHU) [@Dong2020], Google [@Wahltinez2020], and the COVID-19 Data Hub [@Guidotti2020]. However, most of these efforts aggregate the data they collate into a separate data stream, breaking the linkage with the raw data, and often do not fully surface their data processing pipeline for others to inspect. In contrast `covidregionaldata` provides a clear set of open and fully documented tools that directly operate on raw data where possible in order to make the full data cleaning process transparent to end users.

Other interfaces to COVID-19 data are available in R, though there are fewer that provide tools for downloading subnational data for multiple countries and none that are known to the authors provide a consistent cleaning pipeline of the data sources they support. COVID-19 Data Hub [@Guidotti2020] provides cleaning functions, a wrapper to a custom database hosted by COVID-19 Data Hub, and access to snapshots of data reported historically. `Covdata` [@covdata] provides weekly COVID-19 data updates as well as mobility and activity data from [Apple](https://covid19.apple.com/mobility) [@Apple] and [Google](https://www.google.com/covid19/mobility/data_documentation.html) [@Google]. `Sars2pack` [@sars2pack] provides interfaces to a large number of data sets curated by external organisations. To our knowledge, none of these packages provide an interface to individual country data sources or a consistent set of data handling tools for both raw and processed data.

`covidregionaldata` has been used by researchers to source standardised data for estimating the effective reproductive number of COVID-19 in real-time both nationally and subnationally [@Abbott2020]. It has also been used in analyses comparing effective reproduction numbers from different subnational data sources in the United Kingdom [@Sherratt2020], and estimating the increase in transmission related to the B.1.1.7 variant [@Davies2021]. As well as its use in research it has also been used to visualise and explore current trends in COVID-19 case, deaths, and hospitalisations.

# Acknowledgements

This package provides an interface to data sources which are often collected and maintained by individuals or small teams. Our work, both in this package and more generally, would not be possible without their efforts. Thanks to all contributors and package users who have otherwise provided feedback. Thanks to Tim Taylor for useful design discussions.

# Funding statement

This work was supported by a studentship to J.P. funded by the Biotechnology and Biological Sciences Research Council (BBSRC) grant nr. (BB/M011178/1). SEA, KS, and SF were funded by a Wellcome Trust Senior Research Fellowship to Sebastian Funk (210758/Z/18/Z).

# References
