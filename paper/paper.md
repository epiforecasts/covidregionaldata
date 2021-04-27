
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

`covidregionaldata` is an R [@Rdev:2013] package to interface subnational and national level COVID-19 data, providing cleaned and checked COVID-19 case and, where available, deaths, recoveries and hospitalisation data for all countries supported. All package code is archived on Zenodo [@covidregionaldata2020] and [Github](https://github.com/epiforecasts/covidregionaldata) Currently `covidregionaldata` provides subnational data collated by official government bodies for 14 countries including the UK, India, USA, and Brazil. In addition, national level data is provided from the World Health Organisation (WHO) and European Centre for Disease Prevention and Control (ECDC). The package reduces the burden of having to process data from different sources, allowing researchers to focus their efforts on research. 

# Statement of need 

The onset of the COVID-19 pandemic in late 2019 has placed immense pressures on the public health community to not only advise governments around the world on how this disease might impact communities, but to also suggest what policies would most reduce transmission. Mathematical models forecasting case numbers, deaths and hospitalisations have proven to be a key tool for guiding government policy in many countries, yet generating valuable insights requires that the incidence data used is reliable and consistent.  

In many instances, COVID-19 data is freely available to download from official sources, such as the [WHO](https://covid19.who.int/) and [ECDC](https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide}) at a national level, whilst some country specific data is provided by government bodies, such as the [Office for National Statistics (ONS)](https://coronavirus.data.gov.uk/details/download}) in the United Kingdom. Stemming from different sources, these data come in a variety of different formats, requiring researchers to check and standardise data before it can be combined and used for analysis. This is a time-consuming process as different data sets require different methods and it can be very easy for errors to creep into code, either through programmer error or through changes to a dependency package, potentially misrepresenting the data in ways which are difficult to identify. At best an independent workflow slows down the pace of research, at worst it can lead to misleading and erroneous results.  

With the current pressures to understand the development of the pandemic across the globe, we feel it is important researchers have a package which brings together data from multiple different sources and allows it to be easily cleaned, checked and standardised ready for application. `covidregionaldata` uses packages many researchers are familiar with (such as the `tidyverse` suite) and so can be easily adopted by researchers working in R. In addition, `covidregionaldata` provides transparent workflows which keeps the data cleaning connected with the raw sources and makes it easy to inspect at each step. 

So far `covidregionaldata` has been used by researchers to get data for calculating reproductive numbers [@AbbottSam2020Ettr]. As far as the authors are aware, only one other R package providing similar COVID-19 incidence data retrievable from R is COVID-19 Data Hub [@covid19datahub:2020]. The difference between COVID-19 Data Hub and `covidregionaldata	` is that `covidregionaldata` provides cleaning steps for data downloaded from their official source as part of the package, whereas COVID-19 Data Hubprovides a wrapper to a custom database hosted by COVID-19 Data Hub and cleaning functions. The objective of `covidregionaldata` is to provide subnational covid-19 data in a regulated and fully transparent framework, allowing researchers to inspect where their data comes from and how it is prepared. 

# References
